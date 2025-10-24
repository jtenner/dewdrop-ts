import type { NameToken, StringToken, TypeToken } from "../lexer.js";
import type {
  ApplicationTypeExpression,
  BlockExpression,
  BodyExpression,
  BoolExpression,
  CallExpression,
  ConstructorPatternExpression,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  FloatExpression,
  Fn,
  FnExpression,
  FnTypeExpression,
  Identifier,
  IfExpression,
  Import,
  InfixExpression,
  IntExpression,
  LetBindBodyExpression,
  LetDeclaration,
  Module,
  NamedTypeExpression,
  NameIdentifier,
  PatternExpression,
  PostfixExpression,
  PrefixExpression,
  RecordExpression,
  RecordPatternExpression,
  RecordTypeExpression,
  SelectExpression,
  SelectTypeExpression,
  SelfExpression,
  TupleExpression,
  TuplePatternExpression,
  TupleTypeExpression,
  TypeExpression,
} from "../parser.js";
import type {
  Kind,
  Pattern,
  Term,
  Type,
  VariantType,
} from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import type { Scopable, Scope } from "./create_scopes.js";

function arrows(args: Type[], ret: Type): Type {
  return args.reduceRight((acc, from) => ({ arrow: { from, to: acc } }), ret);
}

function lookup_type(name: string, scope: Scope) {
  let current: Scope | null = scope;
  while (current) {
    const value = current.type_elements.get(name);
    if (value) return value;
    current = current.parent;
  }
  return null;
}

function lookup_term(name: string, scope: Scope) {
  let current: Scope | null = scope;
  while (current) {
    const value = current.term_elements.get(name);
    if (value) return value;
    current = current.parent;
  }
  return null;
}

export type TypeMap = Map<TypeExpression | EnumVariant, Type>;
export type TermMap = Map<BodyExpression | Expression | Fn | EnumVariant, Term>;

export type ElaborationError =
  | { not_a_record_type: TypeExpression }
  | { record_has_no_field: { type: TypeExpression; field: string } }
  | { cannot_resolve_symbol: Identifier }
  | { variant_type_not_found: string }
  | { type_is_not_a_variant: string };

type VisitorMode = "type" | "term" | "pattern";

export class ElaboratePass extends BaseVisitor {
  private modeStack: VisitorMode[] = [];
  errors = [] as ElaborationError[];
  types: TypeMap = new Map();
  terms: TermMap = new Map();
  patterns = new Map<PatternExpression, Pattern>();

  enumTypes = new Map<EnumDeclaration, VariantType>();

  constructor(
    public scopes: Map<Scopable, Scope>,
    public variants: Map<EnumVariant, EnumDeclaration>,
  ) {
    super();
  }

  protected get mode(): VisitorMode {
    return this.modeStack[this.modeStack.length - 1] ?? "term";
  }

  protected withMode<T>(mode: VisitorMode, fn: () => T): T {
    this.modeStack.push(mode);
    try {
      return fn();
    } finally {
      this.modeStack.pop();
    }
  }

  elaborate(module: Module) {
    this.visitModule(module);
  }

  override visitExpression(node: Expression): Expression {
    return this.withMode("term", () => super.visitExpression(node));
  }

  override visitTypeExpression(node: TypeExpression): TypeExpression {
    return this.withMode("type", () => super.visitTypeExpression(node));
  }

  override visitPatternExpression(node: PatternExpression): PatternExpression {
    return this.withMode("pattern", () => super.visitPatternExpression(node));
  }

  override visitFnTypeExpression(node: FnTypeExpression): TypeExpression {
    super.visitFnTypeExpression(node);
    const arg_tys = node.fn.args.map(
      (a) => this.types.get(a) ?? { con: "bot" },
    );
    const ret_ty = this.types.get(node.fn.ret) ?? { con: "bot" };
    const type = arrows(arg_tys, ret_ty);
    this.types.set(node, type);
    return node;
  }

  override visitTupleTypeExpression(node: TupleTypeExpression): TypeExpression {
    super.visitTupleTypeExpression(node);
    const tuple_types = node.tuple.map(
      (a) => this.types.get(a) ?? { con: "bot" },
    );
    this.types.set(node, {
      tuple: tuple_types,
    });
    return node;
  }

  override visitRecordTypeExpression(
    node: RecordTypeExpression,
  ): TypeExpression {
    super.visitRecordTypeExpression(node);

    const record = node.record.map(
      (t) =>
        [t.name.name, this.types.get(t.ty) ?? { con: "bot" }] as [string, Type],
    );
    this.types.set(node, { record });
    return node;
  }

  override visitSelectTypeExpression(
    node: SelectTypeExpression,
  ): TypeExpression {
    // only visit the root. The field is an identifier
    super.visitTypeExpression(node.select.root);
    const rootType: Type = this.types.get(node.select.root) ?? { con: "bot" };

    if ("record" in rootType) {
      const fieldName =
        "name" in node.select.name
          ? node.select.name.name
          : node.select.name.type; // handle token types
      const fieldEntry = rootType.record.find(([label]) => label === fieldName);

      if (!fieldEntry) {
        // You could throw or store a bottom type for undefined fields
        console.warn(
          `Type selection failed: record has no field "${String(fieldName)}"`,
        );
        this.errors.push({
          record_has_no_field: {
            type: node.select.root,
            field: fieldName,
          },
        });
        this.types.set(node, { con: "bot" });
        return node;
      }

      const [, fieldType] = fieldEntry;
      this.types.set(node, fieldType);
      return node;
    }

    this.errors.push({ not_a_record_type: node.select.root });
    this.types.set(node, { con: "bot" });
    return node;
  }

  override visitApplicationTypeExpression(
    node: ApplicationTypeExpression,
  ): TypeExpression {
    super.visitApplicationTypeExpression(node);
    const calleeType: Type = this.types.get(node.app.callee) ?? { con: "bot" };
    const argTypes: Type[] = node.app.args.map(
      (arg) => this.types.get(arg) ?? { con: "bot" },
    );
    let accumulated = calleeType;
    for (const argType of argTypes) {
      accumulated = {
        app: {
          func: accumulated,
          arg: argType,
        },
      };
    }
    this.types.set(node, accumulated);
    return node;
  }

  override visitBlockExpression(node: BlockExpression): Expression {
    super.visitBlockExpression(node);
    const last = node.block[node.block.length - 1]!;
    let lastExpr: Expression | null = null;
    if ("let_bind" in last) lastExpr = last.let_bind.expression;
    else if ("expression" in last) lastExpr = last.expression;
    if (!lastExpr) throw new Error("Impossible state.");
    let term = this.terms.get(lastExpr);
    if (!term) throw new Error("No expression generated.");

    for (let i = node.block.length - 2; i >= 0; i--) {
      const next = node.block[i]!;
      if ("let_bind" in next) {
        const { assert: _, expression, pattern } = next.let_bind;
        const arms = [] as [Pattern, Term][];
        const nextPattern = this.patterns.get(pattern);
        if (!nextPattern) throw new Error("No pattern generated.");
        const nextTerm = this.terms.get(expression);
        if (!nextTerm) throw new Error("No term generated.");

        arms.push([nextPattern, term]);
        arms.push([
          { wildcard: null },
          {
            app: {
              callee: { var: "unreachable" },
              arg: { tuple: [] },
            },
          },
        ]);
        term = {
          match: {
            scrutinee: nextTerm,
            cases: arms,
          },
        };
      } else if ("expression" in next) {
        const arms = [] as [Pattern, Term][];
        const nextTerm = this.terms.get(next.expression);
        if (!nextTerm) throw new Error("No term generated.");

        arms.push([{ wildcard: null }, term]);
        term = {
          match: {
            scrutinee: nextTerm,
            cases: arms,
          },
        };
      } else throw new Error("Impossible state.");
    }

    this.terms.set(node, term);

    return node;
  }

  override visitBoolExpression(node: BoolExpression): Expression {
    if (node.bool) {
      this.terms.set(node, { con: { name: "true", type: { con: "Bool" } } });
    } else {
      this.terms.set(node, { con: { name: "false", type: { con: "Bool" } } });
    }
    return node;
  }

  override visitTupleExpression(node: TupleExpression): Expression {
    const tuple = [] as Term[];

    for (const el of node.tuple) {
      const term = this.terms.get(el);
      if (!term) throw new Error("No expression generated.");
      tuple.push(term);
    }

    this.terms.set(node, { tuple });
    return node;
  }

  override visitIntExpression(node: IntExpression): Expression {
    super.visitIntExpression(node);
    this.terms.set(node, {
      con: {
        name: node.int.value.toString(),
        type: { con: `I${node.int.size}` },
      },
    });
    return node;
  }

  override visitFloatExpression(node: FloatExpression): Expression {
    this.terms.set(node, {
      con: {
        name: node.float.value.toString(),
        type: { con: `F${node.float.size}` },
      },
    });
    return node;
  }

  override visitRecordExpression(node: RecordExpression): Expression {
    super.visitRecordExpression(node);
    const record = [] as [string, Term][];

    for (const pair of node.record) {
      const term = this.terms.get(pair[1]);
      if (!term) throw new Error("Expression not generated.");
      record.push([pair[0].name, term]);
    }
    this.terms.set(node, { record });
    return node;
  }

  override visitSelfExpression(node: SelfExpression): Expression {
    this.terms.set(node, { var: "self" });
    return node;
  }

  override visitIfExpression(node: IfExpression): Expression {
    super.visitIfExpression(node);
    const scrutinee = this.terms.get(node.if_expr.cond);
    if (!scrutinee) throw new Error("Expression not generated.");

    const trueCase = this.terms.get(node.if_expr.if_body);
    if (!trueCase) throw new Error("Expression not generated.");

    const falseCase = node.if_expr.else_body
      ? this.terms.get(node.if_expr.else_body)
      : { tuple: [] };
    if (!falseCase) throw new Error("Expression not generated.");

    const cases = [] as [Pattern, Term][];
    cases.push([{ con: { name: "true", type: { con: "Bool" } } }, trueCase]);
    cases.push([{ con: { name: "false", type: { con: "Bool" } } }, falseCase]);

    const term = {
      match: {
        cases,
        scrutinee,
      },
    } as Term;
    this.terms.set(node, term);
    return node;
  }

  override visitStringExpression(node: StringToken): Expression {
    this.terms.set(node, {
      con: { name: node.string, type: { con: "String" } },
    });
    return node;
  }

  override visitSelectExpression(node: SelectExpression): Expression {
    super.visitSelectExpression(node);
    const record = this.terms.get(node.select[0]);
    if (!record) throw new Error("Expression not generated");

    this.terms.set(node, { project: { record, label: node.select[1].name } });
    return node;
  }

  override visitCallExpression(node: CallExpression): Expression {
    super.visitCallExpression(node);
    const [fn, params] = node.call;
    const callee = this.terms.get(fn);
    if (!callee) throw new Error("Expression not generated.");

    let term: Term | undefined =
      params.length === 0
        ? { tuple: [] }
        : this.terms.get(params[params.length - 1]!);

    if (!term) throw new Error("Expression not generated");

    for (let i = params.length - 2; i >= 0; i--) {
      const next = this.terms.get(params[i]!);
      if (!next) throw new Error("Expression not generated");
      term = { app: { callee: term, arg: next } };
    }

    term = { app: { callee, arg: term } };

    this.terms.set(node, term);
    return node;
  }

  override visitPrefixExpression(node: PrefixExpression): Expression {
    super.visitPrefixExpression(node);
    const { operand, op } = node.prefix;
    const term = this.terms.get(operand);
    if (!term) throw new Error("Expression not generated.");

    this.terms.set(node, {
      app: {
        callee: { var: op },
        arg: term,
      },
    });
    return node;
  }

  override visitInfixExpression(node: InfixExpression): Expression {
    super.visitInfixExpression(node);
    const { left, op, right } = node.infix;
    const leftArg = this.terms.get(left);
    if (!leftArg) throw new Error("Expression not generated.");
    const rightArg = this.terms.get(right);
    if (!rightArg) throw new Error("Expression not generated.");

    this.terms.set(node, {
      app: {
        callee: {
          app: {
            callee: { var: op },
            arg: rightArg,
          },
        },
        arg: leftArg,
      },
    });
    return node;
  }

  override visitPostfixExpression(node: PostfixExpression): Expression {
    super.visitPostfixExpression(node);
    const { operand, op } = node.postfix;
    const term = this.terms.get(operand);
    if (!term) throw new Error("Expression not generated.");

    this.terms.set(node, {
      app: {
        callee: { var: op },
        arg: term,
      },
    });
    return node;
  }

  override visitFnExpression(node: FnExpression): Expression {
    super.visitFnExpression(node);
    const fn = node.fn;

    const term = this.terms.get(fn);
    if (!term) throw new Error("Expression not generated.");

    this.terms.set(node, term);
    return node;
  }

  private varid = 0;
  protected freshVar(prefix: string) {
    return `α-${prefix}-${this.varid++}`;
  }

  override visitFn(node: Fn): Fn {
    super.visitFn(node);

    // Step 1: Lookup or elaborate the return type if given
    const returnType = node.return_type
      ? this.types.get(node.return_type)
      : { var: this.freshVar("return_type") };
    if (!returnType) throw new Error("Return type not generated for fn.");

    // Step 2: Elaborate the function body (expression -> Term)
    const bodyTerm = this.terms.get(node.body);
    if (!bodyTerm) throw new Error("Body term not generated for fn.");

    // Step 3: Elaborate parameters — wrap as LamTerms
    let term = bodyTerm;
    for (let i = node.params.length - 1; i >= 0; i--) {
      const param = node.params[i]!;
      const paramName = param.name.name;
      const paramType = (param.guard && this.types.get(param.guard)) ?? {
        con: "bot",
      };

      term = {
        lam: {
          arg: paramName,
          type: paramType,
          body: term,
        },
      };
    }

    // Step 4: Elaborate type parameters — wrap as TyLamTerms
    for (let i = node.type_params.length - 1; i >= 0; i--) {
      const typeParam = node.type_params[i]!;
      const name = typeParam.name;
      const kind = { star: null }; // default kind
      term = {
        tylam: {
          var: name,
          kind,
          body: term,
        },
      };
    }

    this.terms.set(node, term);

    return node;
  }

  override visitNamePatternExpression(node: NameIdentifier): PatternExpression {
    const name: Pattern = { var: node.name };
    this.patterns.set(node, name);
    return node;
  }

  override visitTuplePatternExpression(
    node: TuplePatternExpression,
  ): PatternExpression {
    super.visitTuplePatternExpression(node);
    const tuple = [] as Pattern[];

    for (const p of node.tuple) {
      const innerPattern = this.patterns.get(p);
      if (!innerPattern) throw new Error("Pattern not generated.");
      tuple.push(innerPattern);
    }

    this.patterns.set(node, { tuple });
    return node;
  }

  override visitRecordPatternExpression(
    node: RecordPatternExpression,
  ): PatternExpression {
    super.visitRecordPatternExpression(node);
    const record = [] as [string, Pattern][];

    for (const [{ name: key }, p] of node.record) {
      const valuePattern = this.patterns.get(p);
      if (!valuePattern) throw new Error("Pattern not generated.");
      record.push([key, valuePattern]);
    }

    this.patterns.set(node, { record });
    return node;
  }

  override visitConstructorPatternExpression(
    node: ConstructorPatternExpression,
  ): PatternExpression {
    super.visitConstructorPatternExpression(node);

    // resolve the variant and type
    const scope = this.scopes.get(node);
    if (!scope) throw new Error("Scope not generated for pattern.");

    const element = lookup_type(node.constr.type.type, scope);
    // we need the type of the constructor
    if (element && "variant" in element) {
      const parent = this.variants.get(element.variant);
      if (!parent) throw new Error("Variant not generated for enum element.");

      const enumType = this.enumTypes.get(parent);
      if (!enumType) throw new Error("EnumType not generated for declaration.");

      const variantType = enumType.variant.find(
        (t) => t[0] === node.constr.type.type,
      );
      if (!variantType)
        throw new Error("Variant has no variant type generated.");

      const pattern: Pattern = {
        con: { name: node.constr.type.type, type: variantType[1] },
      };

      this.patterns.set(node, pattern);
      return node;
    } else {
      this.patterns.set(node, { wildcard: null });
      this.errors.push({ cannot_resolve_symbol: node.constr.type });
      return node;
    }
  }

  override visitNameIdentifier(node: NameToken): NameToken {
    if (this.mode === "type") {
      this.types.set(node, { var: node.name });
    } else if (this.mode === "term") {
      this.terms.set(node, { var: node.name } as Term);
    }

    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    super.visitEnumDeclaration(node);
    const variant = [] as [string, Type][];
    let enumType: Type = { variant };

    for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
      enumType = {
        forall: {
          var: node.enum.type_params[i]!.name,
          body: enumType,
          kind: { star: null },
        },
      } as Type;
    }

    for (const v of node.enum.variants) {
      if ("fields" in v) {
        const record = [] as [string, Type][];
        const recordType = { record };
        const fields = v.fields.fields;
        for (const {
          name: { name },
          ty,
        } of fields) {
          const fieldType = this.types.get(ty);
          if (!fieldType)
            throw new Error("Type not generated for enum variant parameter.");
          record.push([name, fieldType]);
        }

        // 1. Add the variant to the type itself
        variant.push([v.fields.id.type, recordType]);

        // 2. Add a term constructor with type params
        let body: Term = {
          lam: {
            arg: "$value",
            type: {
              lam: {
                body: enumType,
                kind: { star: null },
                var: "$value",
              },
            },
            body: {
              inject: {
                label: v.fields.id.type,
                value: { var: "$value" },
                variant_type: enumType,
              },
            },
          },
        };

        for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
          body = {
            tylam: {
              body: body,
              kind: { star: null },
              var: node.enum.type_params[i]!.name,
            },
          };
        }
        this.terms.set(v, body);

        let ctorType: Type = {
          arrow: {
            from: recordType,
            to: enumType,
          },
        };
        for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
          ctorType = {
            forall: {
              body: ctorType,
              kind: { star: null },
              var: node.enum.type_params[i]!.name,
            },
          };
        }
        this.types.set(v, ctorType);
      } else {
        const termTypes = [] as Type[];
        const tupleType = { tuple: termTypes };
        for (const ty of v.values.values) {
          const fieldType = this.types.get(ty);
          if (!fieldType)
            throw new Error("Type not generated for enum variant parameter.");
          termTypes.push(fieldType);
        }
        variant.push([v.values.id.type, tupleType]);

        // variant is a constructor term with term parameters named `$value{n}`
        // to `$value{0}`

        let term: Term = {
          inject: {
            variant_type: enumType,
            label: v.values.id.type,
            value: {
              tuple: termTypes.map((_, i) => ({ var: `$value${i}` })),
            },
          },
        };

        // wrap params backwards
        for (let i = termTypes.length - 1; i >= 0; i--) {
          term = {
            lam: {
              arg: `$value${i}`,
              body: term,
              type: termTypes[i]!,
            },
          };
        }

        // wrap type lambdas
        for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
          term = {
            tylam: {
              body: term,
              kind: { star: null },
              var: node.enum.type_params[i]!.name,
            },
          };
        }
        this.terms.set(v, term);

        // function type
        let ctorType = enumType;

        // function parameter types
        for (let i = termTypes.length - 1; i >= 0; i--) {
          ctorType = {
            arrow: {
              from: termTypes[i]!,
              to: ctorType,
            },
          };
        }
        for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
          ctorType = {
            forall: {
              body: ctorType,
              kind: { star: null },
              var: node.enum.type_params[i]!.name,
            },
          };
        }
        this.types.set(v, ctorType);
        // function type parameters
      }
    }
    // We need to create the enum terms
    // and constructor types here
    return node;
  }

  override visitEnumVariant(node: EnumVariant): EnumVariant {
    // no need to visit the names of these variants
    if ("fields" in node) {
      for (const variant of node.fields.fields) {
        this.visitTypeExpression(variant.ty);
      }
    } else {
      for (const variant of node.values.values) {
        this.visitTypeExpression(variant);
      }
    }
    return node;
  }

  override visitImport(node: Import) {
    // no need to visit imports here
    return node;
  }

  protected generateConstructor(name: string, variantType: VariantType) {
    const variant = variantType.variant.find((t) => t[0] === name);
    // if the type was found already, we should know the name
    if (!variant) throw new Error("Invalid constructor name for variant.");

    if ("field" in variant[1]) {
    }
  }

  override visitTypeIdentifier(node: TypeToken): TypeToken {
    const scope = this.scopes.get(node);
    if (!scope) throw new Error("Scope not found for identifier!");

    const item = lookup_type(node.type, scope);
    if (!item) {
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    if (this.mode === "type") {
      if (
        "enum" in item ||
        "star_import" in item ||
        "trait" in item ||
        "type_decl" in item ||
        "type_import" in item ||
        "variant" in item
      ) {
        this.types.set(node, { con: node.type });
      } else throw new Error("This is impossible.");
    }

    if (this.mode === "term") {
      if ("variant" in item) {
        const term: Term = { var: node.type };
        this.terms.set(node, term);
      } else {
        this.errors.push({ type_is_not_a_variant: node.type });
      }
    }

    return node;
  }
}
