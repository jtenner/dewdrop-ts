import type { StringToken, TypeToken } from "../lexer.js";
import type {
  ApplicationTypeExpression,
  BlockExpression,
  BodyExpression,
  BoolExpression,
  BuiltinDeclaration,
  CallExpression,
  ConstructorPatternExpression,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  FloatExpression,
  Fn,
  FnExpression,
  FnParam,
  FnTypeExpression,
  Identifier,
  IfExpression,
  ImplDeclaration,
  Import,
  InfixExpression,
  IntExpression,
  MatchExpression,
  Module,
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
  TraitDeclaration,
  TupleExpression,
  TuplePatternExpression,
  TupleTypeExpression,
  TypeExpression,
} from "../parser.js";
import {
  type Context,
  type Kind,
  type MatchTerm,
  type Pattern,
  showType,
  type Term,
  type TraitDef,
  type Type,
  unitType,
} from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import {
  lookup_term,
  lookup_type,
  type Scopable,
  type Scope,
} from "./create_scopes.js";

function arrows(args: Type[], ret: Type): Type {
  if (args.length === 0) return { arrow: { from: unitType, to: ret } };
  return args.reduceRight((acc, from) => ({ arrow: { from, to: acc } }), ret);
}

function getVariantFromType(name: string, t: Type) {
  if ("variant" in t) {
    return t.variant.find((t) => t[0] === name);
  }
  if ("forall" in t) {
    return getVariantFromType(name, t.forall.body);
  }
  if ("lam" in t) {
    return getVariantFromType(name, t.lam.body);
  }
}

export type TypeMap = Map<
  | TraitDeclaration
  | ImplDeclaration
  | Expression
  | Fn
  | BuiltinDeclaration
  | EnumDeclaration
  | TypeExpression
  | EnumVariant,
  Type
>;
export type TermMap = Map<
  | ImplDeclaration
  | BuiltinDeclaration
  | BodyExpression
  | Expression
  | Fn
  | EnumVariant,
  Term
>;

export type ElaborationError =
  | { not_a_record_type: TypeExpression }
  | { record_has_no_field: { type: TypeExpression; field: string } }
  | { cannot_resolve_symbol: Identifier }
  | { variant_type_not_found: string }
  | { type_is_not_a_variant: string }
  | { function_parameter_type_required: FnParam };

type VisitorMode = "type" | "term" | "pattern";

export class ElaboratePass extends BaseVisitor {
  private modeStack: VisitorMode[] = [];
  errors = [] as ElaborationError[];
  types: TypeMap = new Map();
  terms: TermMap = new Map();
  patterns = new Map<PatternExpression, Pattern>();
  context = [] as Context;

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

  override visitBuiltinDeclaration(node: BuiltinDeclaration): Declaration {
    const builtin = node.builtin;
    for (const param of builtin.params) {
      if (!param.guard) {
        this.errors.push({ function_parameter_type_required: param });
        return node;
      }
      this.visitTypeExpression(param.guard);
    }
    this.visitTypeExpression(builtin.return_type);

    // 1. Elaborate parameter types
    const paramTypes: Type[] = builtin.params.map((p) => {
      const ty = this.types.get(p.guard!);
      if (!ty) throw new Error("Parameter type not elaborated");
      return ty;
    });

    // 2. Elaborate return type
    const returnType = this.types.get(builtin.return_type);
    if (!returnType) throw new Error("Return type not elaborated");
    // 3. Build the function type: τ₁ → τ₂ → ... → τₙ → τᵣₑₜ
    const builtinType = arrows(paramTypes, returnType);

    // 4. Create a con term with the i`nternal builtin name
    const builtinTerm: Term = {
      con: {
        name: builtin.name.string, // e.g., "llvm.add"
        type: builtinType,
      },
    };

    // 5. Register both the term and type
    this.terms.set(node, builtinTerm);
    this.types.set(node, builtinType);
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
    if (!term) {
      throw new Error("No expression generated.");
    }

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

  override visitMatchExpression(node: MatchExpression) {
    super.visitMatchExpression(node);

    const scrutinee = this.terms.get(node.match[0]);
    if (!scrutinee) throw new Error("Expression not generated.");

    const cases = [] as [Pattern, Term][];

    for (const arm of node.match[1]) {
      const pattern = this.patterns.get(arm.pattern);
      if (!pattern) throw new Error("Pattern not generated.");

      const body = this.terms.get(arm.body);
      if (!body) throw new Error("Expression not generated");

      cases.push([pattern, body]);
    }

    const term: MatchTerm = {
      match: {
        scrutinee,
        cases,
      },
    };
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
    const calleeType = this.types.get(fn);

    if (!callee) throw new Error("Expression not generated.");
    if (!calleeType) throw new Error("Expression generated no type.");

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
    let type = returnType;
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
      type = {
        arrow: {
          from: paramType,
          to: type,
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
      type = {
        forall: {
          var: name,
          body: type,
          kind,
        },
      };
    }

    this.terms.set(node, term);
    this.types.set(node, type);
    return node;
  }

  override visitNamePatternExpression(node: NameIdentifier): PatternExpression {
    if (node.name === "_") {
      this.patterns.set(node, { wildcard: null });
    } else {
      this.patterns.set(node, { var: node.name });
    }
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

    const scope = this.scopes.get(node);
    if (!scope) throw new Error("Scope not generated for pattern.");

    const element = lookup_type(node.constr.type.type, scope);

    if (element && "variant" in element) {
      const parent = this.variants.get(element.variant);
      if (!parent) throw new Error("Variant not generated for enum element.");

      const enumType = this.types.get(parent);
      if (!enumType) throw new Error("EnumType not generated for declaration.");

      const variantType = getVariantFromType(node.constr.type.type, enumType);

      if (!variantType)
        throw new Error("Variant has no variant type generated.");

      // Build the inner pattern based on the number of patterns and variant type
      let innerPattern: Pattern;

      if (node.constr.patterns.length === 0) {
        // No patterns: unit/wildcard
        innerPattern = { tuple: [] }; // or { wildcard: null }
      } else if (node.constr.patterns.length === 1) {
        // Single pattern: use it directly (not wrapped in tuple)
        const elaborated = this.patterns.get(node.constr.patterns[0]!);
        if (!elaborated) {
          throw new Error("Inner pattern not elaborated");
        }
        innerPattern = elaborated;
      } else {
        // Multiple patterns: wrap in tuple
        const tuplePatterns: Pattern[] = [];
        for (const p of node.constr.patterns) {
          const elaborated = this.patterns.get(p);
          if (!elaborated) {
            throw new Error("Inner pattern not elaborated");
          }
          tuplePatterns.push(elaborated);
        }
        innerPattern = { tuple: tuplePatterns };
      }

      const pattern: Pattern = {
        variant: {
          label: node.constr.type.type,
          pattern: innerPattern,
        },
      };

      this.patterns.set(node, pattern);
      return node;
    } else {
      this.patterns.set(node, { wildcard: null });
      this.errors.push({ cannot_resolve_symbol: node.constr.type });
      return node;
    }
  }

  override visitNameIdentifier(node: NameIdentifier): NameIdentifier {
    if (this.mode === "type") {
      this.types.set(node, { var: node.name });
    } else if (this.mode === "term") {
      const scope = this.scopes.get(node);
      if (!scope) throw new Error(`No scope generated for: ${node.name}`);
      const scopedType = lookup_term(node.name, scope);
      if (!scopedType)
        throw new Error(`No element generated for: ${node.name}`);
      if ("fn" in scopedType) {
        const ty = this.types.get(scopedType.fn);
        if (!ty) throw new Error("No function type generated for fn.");
        this.types.set(node, ty);
      } else if ("builtin" in scopedType) {
        const ty = this.types.get(scopedType.builtin);
        if (!ty)
          throw new Error(
            `No function type generated for builtin: ${node.name}`,
          );
        this.types.set(node, ty);
      }
      this.terms.set(node, { var: node.name } as Term);
    }

    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    super.visitEnumDeclaration(node);
    const variant = [] as [string, Type][];
    let enumType: Type = { variant };
    const variantType = enumType;

    let enumKind: Kind = { star: null };
    for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
      enumKind = {
        arrow: {
          from: { star: null },
          to: enumKind,
        },
      };
    }

    for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
      enumType = {
        lam: {
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

        variant.push([v.fields.id.type, recordType]);

        let body: Term = {
          lam: {
            arg: "$value",
            type: recordType, // ✅ Changed from type-level lambda
            body: {
              inject: {
                label: v.fields.id.type,
                value: { var: "$value" },
                variant_type: variantType,
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
            to: variantType,
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

        let term: Term = {
          inject: {
            variant_type: variantType,
            label: v.values.id.type,
            value: {
              tuple: termTypes.map((_, i) => ({ var: `$value${i}` })),
            },
          },
        };

        for (let i = termTypes.length - 1; i >= 0; i--) {
          term = {
            lam: {
              arg: `$value${i}`,
              body: term,
              type: termTypes[i]!,
            },
          };
        }

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

        let ctorType = variantType as Type;

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
      }
    }

    this.types.set(node, enumType);
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

  override visitTypeIdentifier(node: TypeToken): TypeToken {
    const scope = this.scopes.get(node);
    if (!scope) throw new Error("Scope not found for identifier!");

    // short for "never" type which is always defined
    if (this.mode === "type" && node.type === "Never") {
      this.types.set(node, { never: null });
      return node;
    }

    const item = lookup_type(node.type, scope);
    if (!item) {
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    if (this.mode === "type") {
      if ("enum" in item) {
        const enumDecl = item.enum;
        const enumType = this.types.get(enumDecl);
        if (!enumType) throw new Error("Enum type not elaborated.");
        this.types.set(node, enumType);
      } else if (
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
        const type = this.types.get(item.variant);
        if (!type) throw new Error("No type was generated for this variant.");
        this.terms.set(node, term);
        this.types.set(node, type);
      } else {
        this.errors.push({ type_is_not_a_variant: node.type });
      }
    }

    return node;
  }

  // Traits:
  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    console.log("visiting trait declaration in elaborate");
    const trait_decl = node.trait;

    // Build the kind for the trait based on type parameters
    let traitKind: Kind = { star: null };
    for (let i = trait_decl.type_params.length - 1; i >= 0; i--) {
      traitKind = {
        arrow: {
          from: { star: null },
          to: traitKind,
        },
      };
    }

    // Build method signatures
    // For Map<t, u>.map<r, s>(cb: (r) => s, self: Self<t>): Self<u>
    // This becomes: ∀r. ∀s. (r → s) → Self<t> → Self<u>
    const methods: [string, Type][] = [];

    for (const traitFn of trait_decl.fns) {
      // Visit all types
      for (const param of traitFn.params) {
        this.visitTypeExpression(param.ty);
      }
      this.visitTypeExpression(traitFn.return_type);

      // Get elaborated types
      const paramTypes: Type[] = traitFn.params.map((p) => {
        const ty = this.types.get(p.ty);
        if (!ty)
          throw new Error(`Parameter type not elaborated: ${p.name.name}`);
        return ty;
      });

      const returnType = this.types.get(traitFn.return_type);
      if (!returnType) throw new Error("Return type not elaborated");

      // Build Self type applied to first trait type parameter
      // For Map<t, u>, Self becomes Self<t>
      let selfType: Type = { var: "Self" };
      if (trait_decl.type_params.length > 0) {
        selfType = {
          app: {
            func: selfType,
            arg: { var: trait_decl.type_params[0]!.name },
          },
        };
      }

      // Build function type: param1 → param2 → ... → self → return
      let methodType = returnType;

      // Add self as the LAST parameter
      methodType = {
        arrow: {
          from: selfType,
          to: methodType,
        },
      };

      // Add regular parameters (right to left)
      for (let i = paramTypes.length - 1; i >= 0; i--) {
        methodType = {
          arrow: {
            from: paramTypes[i]!,
            to: methodType,
          },
        };
      }

      // Wrap in foralls for method-level type parameters
      for (let i = traitFn.type_params.length - 1; i >= 0; i--) {
        methodType = {
          forall: {
            var: traitFn.type_params[i]!.name,
            kind: { star: null },
            body: methodType,
          },
        };
      }

      methods.push([traitFn.name.name, methodType]);
    }

    // Create trait definition
    const traitDef: TraitDef = {
      name: trait_decl.id.type,
      type_param: "Self",
      kind: traitKind,
      methods,
    };

    // Store in context for later lookup
    this.context.push({ trait_def: traitDef });

    // Also store a marker type
    this.types.set(node, { con: `Trait<${trait_decl.id.type}>` });

    return node;
  }

  override visitImplDeclaration(node: ImplDeclaration): Declaration {
    console.log("visiting impl declaration in elaboration");
    const impl_decl = node.impl;

    // Visit the target type
    this.visitTypeExpression(impl_decl.for);
    const forType = this.types.get(impl_decl.for);
    if (!forType) throw new Error("Impl 'for' type not elaborated");

    // Visit trait type arguments (e.g., <r, u> in Map<r, u>)
    for (const typeParam of impl_decl.type_params) {
      this.visitTypeExpression(typeParam);
    }

    // Collect all free type variables in forType to determine skolems
    // For Either<l, r>, we need to abstract over 'l' and 'r'
    const skolems = this.collectTypeVars(forType);

    // Elaborate each method implementation
    const methodImpls: [string, Term][] = [];

    for (const fn of impl_decl.fns) {
      // The function already has 'self' available in its body
      this.visitFn(fn);

      const fnTerm = this.terms.get(fn);
      if (!fnTerm) throw new Error(`Method ${fn.name?.name} not elaborated`);

      if (!fn.name) throw new Error("Impl method must have a name");

      methodImpls.push([fn.name.name, fnTerm]);
    }

    // Build the dictionary term
    let dictTerm: Term = {
      dict: {
        trait: impl_decl.name.type,
        type: forType,
        methods: methodImpls,
      },
    };

    // Wrap in type lambdas for each skolem variable
    // For impl Map<r, u> for Either<l, r>, this wraps in Λl.
    for (let i = skolems.length - 1; i >= 0; i--) {
      dictTerm = {
        tylam: {
          var: skolems[i]!,
          kind: { star: null },
          body: dictTerm,
        },
      };
    }

    // Compute the dictionary type
    // For impl<l> Map<r, u> for Either<l, r>, type is:
    // ∀l. Dict<Map<r, u>, Either<l, r>>
    let dictType: Type = {
      con: `Dict<${impl_decl.name.type}, ${showType(forType)}>`,
    };

    for (let i = skolems.length - 1; i >= 0; i--) {
      dictType = {
        forall: {
          var: skolems[i]!,
          kind: { star: null },
          body: dictType,
        },
      };
    }

    this.terms.set(node, dictTerm);
    this.types.set(node, dictType);

    // Register the impl in the type checker context
    // This makes it available for automatic dictionary resolution
    this.context.push({
      trait_impl: {
        trait: impl_decl.name.type,
        type: forType,
        dict: dictTerm,
      },
    });

    return node;
  }

  // Helper to collect type variables from a type
  private collectTypeVars(type: Type, vars = new Set<string>()): string[] {
    if ("var" in type) {
      vars.add(type.var);
    } else if ("app" in type) {
      this.collectTypeVars(type.app.func, vars);
      this.collectTypeVars(type.app.arg, vars);
    } else if ("arrow" in type) {
      this.collectTypeVars(type.arrow.from, vars);
      this.collectTypeVars(type.arrow.to, vars);
    } else if ("forall" in type) {
      // Don't collect bound variables
      const inner = new Set<string>();
      this.collectTypeVars(type.forall.body, inner);
      inner.delete(type.forall.var);
      for (const v of inner) vars.add(v);
    } else if ("record" in type) {
      for (const [_, fieldType] of type.record) {
        this.collectTypeVars(fieldType, vars);
      }
    } else if ("tuple" in type) {
      for (const elem of type.tuple) {
        this.collectTypeVars(elem, vars);
      }
    } else if ("variant" in type) {
      for (const [_, caseType] of type.variant) {
        this.collectTypeVars(caseType, vars);
      }
    } else if ("lam" in type) {
      const inner = new Set<string>();
      this.collectTypeVars(type.lam.body, inner);
      inner.delete(type.lam.var);
      for (const v of inner) vars.add(v);
    }

    return Array.from(vars);
  }
}
