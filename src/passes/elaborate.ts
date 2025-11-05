import type { StringToken, TypeToken } from "../lexer.js";
import {
  type ApplicationTypeExpression,
  type BlockExpression,
  type BodyExpression,
  type BoolExpression,
  type BuiltinDeclaration,
  type CallExpression,
  type ConstructorPatternExpression,
  type Declaration,
  type EnumDeclaration,
  type EnumVariant,
  type Expression,
  type FloatExpression,
  type Fn,
  type FnExpression,
  type FnParam,
  type FnTypeExpression,
  type Identifier,
  type IfExpression,
  type ImplDeclaration,
  type Import,
  type InfixExpression,
  type IntExpression,
  type MatchExpression,
  type Module,
  type NameIdentifier,
  type PatternExpression,
  type PostfixExpression,
  type PrefixExpression,
  type RecordExpression,
  type RecordPatternExpression,
  type RecordTypeExpression,
  type SelectExpression,
  type SelectTypeExpression,
  type SelfExpression,
  showExpression,
  showPatternExpression,
  type TraitDeclaration,
  type TraitFn,
  type TupleExpression,
  type TuplePatternExpression,
  type TupleTypeExpression,
  type TypeExpression,
} from "../parser.js";
import {
  app_type,
  arrow_kind,
  arrow_type,
  type BoundedForallType,
  type Context,
  collectTypeVars,
  con_type,
  type EnumDef,
  type FieldScheme,
  fold_term,
  forall_type,
  type Kind,
  lam_term,
  lam_type,
  type MatchTerm,
  mu_type,
  normalizeType,
  type Pattern,
  showPattern,
  showType,
  starKind,
  substituteType,
  type Term,
  type TraitDef,
  type TraitLamTerm,
  type Type,
  type TypingError,
  tylam_term,
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

function getTypeConstructorName(typeExpr: TypeExpression): string | null {
  if ("type" in typeExpr) return typeExpr.type;
  if ("app" in typeExpr) return getTypeConstructorName(typeExpr.app.callee);
  return null;
}

export function collectTypeVarsFromTypes(types: Type[], vars: Set<string>) {
  for (const ty of types) {
    for (const v of collectTypeVars(ty)) {
      vars.add(v);
    }
  }
}

export function computeEnumKind(params: NameIdentifier[]): Kind {
  let kind: Kind = starKind;
  for (let i = params.length - 1; i >= 0; i--) {
    kind = arrow_kind(starKind, kind);
  }
  return kind;
}

export type TypeMap = Map<
  | TraitDeclaration
  | TraitFn
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
  | { function_parameter_type_required: FnParam }
  | TypingError;

type VisitorMode = "type" | "term" | "pattern";

export class ElaboratePass extends BaseVisitor {
  private modeStack: VisitorMode[] = [];
  errors = [] as ElaborationError[];
  types: TypeMap = new Map();
  terms: TermMap = new Map();
  patterns = new Map<PatternExpression, Pattern>();
  context = [] as Context;

  private traitMethodRenamings = new Map<string, Map<string, string>>();

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

  // Helper: Map for caching derived projection types (optional, for perf)
  private projectionTypes = new Map<string, Type>(); // "map" → bounded_forall type

  // Helper: Map for projection terms (trait_lam)
  private projectionTerms = new Map<string, Term>(); // "map" → trait_lam term

  private elaboratedTypes = new Set<Declaration>(); // Top-level decls elaborated in phase 1
  private elaboratedTerms = new Set<
    Declaration | Expression | Fn | EnumVariant
  >(); // Phase 2

  elaborate(module: Module) {
    // Clear tracking for fresh run
    this.elaboratedTypes.clear();
    this.elaboratedTerms.clear();

    // Phase 1: Elaborate all type constructors first (enums, traits, type_decls, type imports)
    // This populates types map before any references
    console.log("Phase 1: Elaborating types (enums, traits, etc.)");
    this.elaborateTypes(module);

    // Phase 2: Full elaboration for terms, patterns, bodies (now types are available)
    console.log("Phase 2: Full elaboration (terms, expressions)");
    this.fullElaborate(module);

    // Log summary (optional)
    console.log(
      `Elaborated ${this.types.size} types, ${this.terms.size} terms`,
    );
  }

  private fullElaborate(module: Module) {
    // Run the full visitor now that types are ready
    this.visitModule(module); // Your existing super.visitModule(module)

    // Explicitly elaborate term-level decls if not covered above
    for (const decl of module.module) {
      if (!this.elaboratedTerms.has(decl) && this.elaboratedTypes.has(decl)) {
        this.visitDeclaration(decl);
        this.elaboratedTerms.add(decl);
      }
    }
  }

  private elaborateTypes(module: Module) {
    for (const decl of module.module) {
      if (this.elaboratedTypes.has(decl)) continue; // Idempotent

      if ("enum" in decl) {
        // Elaborate enum type (populates types for enum + variants)
        this.visitEnumDeclaration(decl);
        this.elaboratedTypes.add(decl);
      } else if ("trait" in decl) {
        // Elaborate trait (populates types for method sigs, selfKind)
        this.visitTraitDeclaration(decl);
        this.elaboratedTypes.add(decl);
      } else if ("type_dec" in decl) {
        // Elaborate type alias/decl (populates types for the alias)
        this.visitTypeDeclaration(decl); // Assume you have this visitor (add if missing, see below)
        this.elaboratedTypes.add(decl);
      } else if ("import_dec" in decl) {
        // Elaborate type/ enum/ trait imports (binds external types to types map)
        this.visitImportDeclaration(decl); // Your existing logic, but focus on type imports
        this.elaboratedTypes.add(decl);
      } else if ("impl" in decl) {
        // Elaborate impl "for" type and method sig types (but not bodies yet)
        const impl_decl = decl.impl;
        this.visitTypeExpression(impl_decl.for); // Bind forType to types map
        for (const fn of impl_decl.fns) {
          // Elaborate sig types only (params, return_type) – no body yet
          for (const param of fn.params) {
            if (param.guard) this.visitTypeExpression(param.guard);
          }
          if (fn.return_type) this.visitTypeExpression(fn.return_type);

          // Type params too
          for (const tp of fn.type_params) {
            this.visitTypeExpression(tp);
          }
        }
        this.elaboratedTypes.add(decl);
      }
    }
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
      (a) => this.types.get(a) ?? { never: null },
    );
    const ret_ty = this.types.get(node.fn.ret) ?? { never: null };
    const type = arrows(arg_tys, ret_ty);
    this.types.set(node, type);
    return node;
  }

  override visitTupleTypeExpression(node: TupleTypeExpression): TypeExpression {
    super.visitTupleTypeExpression(node);
    const tuple_types = node.tuple.map(
      (a) => this.types.get(a) ?? { never: null },
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
        [t.name.name, this.types.get(t.ty) ?? { never: null }] as [
          string,
          Type,
        ],
    );
    this.types.set(node, { record });
    return node;
  }

  override visitSelectTypeExpression(
    node: SelectTypeExpression,
  ): TypeExpression {
    // only visit the root. The field is an identifier
    super.visitTypeExpression(node.select.root);
    const rootType: Type = this.types.get(node.select.root) ?? { never: null };

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
        this.types.set(node, { never: null });
        return node;
      }

      const [, fieldType] = fieldEntry;
      this.types.set(node, fieldType);
      return node;
    }

    this.errors.push({ not_a_record_type: node.select.root });
    this.types.set(node, { never: null });
    return node;
  }

  override visitApplicationTypeExpression(
    node: ApplicationTypeExpression,
  ): TypeExpression {
    super.visitApplicationTypeExpression(node);
    const calleeType: Type = this.types.get(node.app.callee) ?? { never: null };
    const argTypes: Type[] = node.app.args.map(
      (arg) => this.types.get(arg) ?? { never: null },
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
    super.visitCallExpression(node); // Visits sub-expressions (e.g., fn and params)

    const [fnExpr, params] = node.call;
    const calleeTerm = this.terms.get(fnExpr);
    if (!calleeTerm) {
      throw new Error(
        `Callee term not generated for ${showExpression(fnExpr)}`,
      );
    }

    let term: Term;
    if (params.length === 0) {
      // Zero args: f() → app(f, unit)
      term = { app: { callee: calleeTerm, arg: { tuple: [] } } };
    } else {
      // Non-zero args: accumulate left-associative apps forward
      term = calleeTerm;
      for (const param of params) {
        // Forward loop: first to last
        const argTerm = this.terms.get(param);
        if (!argTerm) {
          throw new Error(
            `Argument term not generated for ${showExpression(param)}`,
          );
        }
        term = { app: { callee: term, arg: argTerm } };
      }
    }

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
        never: null,
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
      const kind = starKind; // default kind
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
    if (!scope) {
      this.errors.push({ cannot_resolve_symbol: node.constr.type });
      this.patterns.set(node, { wildcard: null });
      return node;
    }

    const element = lookup_type(node.constr.type.type, scope);
    if (!element || !("variant" in element)) {
      this.errors.push({ cannot_resolve_symbol: node.constr.type });
      this.patterns.set(node, { wildcard: null });
      return node;
    }

    const parent = this.variants.get(element.variant);
    if (!parent) {
      this.errors.push({ variant_type_not_found: node.constr.type.type });
      this.patterns.set(node, { wildcard: null });
      return node;
    }

    // Build inner pattern structurally (no type instantiation or enum reference needed)
    // - For 0 patterns (unit variant like None): Use wildcard or empty tuple (binds nothing)
    // - For 1 pattern: Direct subpattern
    // - For >1: Tuple of subpatterns
    let innerPattern: Pattern;
    const subPatterns = node.constr.patterns.map((p) => {
      const elaborated = this.patterns.get(p);
      if (!elaborated)
        throw new Error(
          `Subpattern not elaborated: ${showPatternExpression(p)}`,
        );
      return elaborated;
    });

    if (subPatterns.length === 0) {
      innerPattern = { wildcard: null }; // Or { tuple: [] } for explicit unit (both work)
    } else if (subPatterns.length === 1) {
      innerPattern = subPatterns[0]!;
    } else {
      innerPattern = { tuple: subPatterns };
    }

    const pattern: Pattern = {
      variant: {
        label: node.constr.type.type, // e.g., "Left"
        pattern: innerPattern,
      },
    };

    this.patterns.set(node, pattern);

    // Optional debugging (remove in production)
    console.log(
      `Elaborated structural pattern for "${node.constr.type.type}": label="${node.constr.type.type}", inner=${showPattern(pattern)}`,
    );

    return node;
  }

  override visitNameIdentifier(node: NameIdentifier): NameIdentifier {
    if (this.mode === "type") {
      this.types.set(node, { var: node.name });
      return node;
    }

    if (this.mode === "pattern") {
      // Patterns are handled separately
      return node;
    }

    // Term mode - look up in scope
    const scope = this.scopes.get(node);
    if (!scope) throw new Error(`No scope generated for: ${node.name}`);

    const scopedElement = lookup_term(node.name, scope);
    if (!scopedElement) {
      if (this.projectionTypes.has(node.name)) {
        console.log(`Resolving global projection "${node.name}"`);
        this.types.set(node, this.projectionTypes.get(node.name)!);
        this.terms.set(node, this.projectionTerms.get(node.name)!);
        return node;
      }
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    // Handle different kinds of term bindings
    if ("fn_param" in scopedElement) {
      // Function parameter: it's a bound variable
      // The type should already be in the scope from the function signature
      const param = scopedElement.fn_param;

      // Get the parameter type
      let paramType: Type;
      if (param.guard) {
        const guardType = this.types.get(param.guard);
        if (!guardType) {
          throw new Error(`Parameter type not elaborated: ${param.name.name}`);
        }
        paramType = guardType;
      } else {
        // No type annotation - this shouldn't happen after elaboration
        paramType = { var: this.freshVar("param") };
      }

      this.terms.set(node, { var: node.name });
      this.types.set(node, paramType);
      return node;
    }

    if ("let_decl" in scopedElement) {
      // Let binding: it's a variable reference
      const letDecl = scopedElement.let_decl;
      const letType = this.types.get(
        letDecl.let_dec.guard ?? letDecl.let_dec.value,
      );

      if (!letType) {
        throw new Error(`Let binding type not elaborated: ${node.name}`);
      }

      this.terms.set(node, { var: node.name });
      this.types.set(node, letType);
      return node;
    }

    if ("fn" in scopedElement) {
      // Function reference
      const fnDecl = scopedElement.fn;
      const fnType = this.types.get(fnDecl);

      if (!fnType) {
        throw new Error(`Function type not elaborated: ${node.name}`);
      }

      this.terms.set(node, { var: node.name });
      this.types.set(node, fnType);
      return node;
    }

    if ("builtin" in scopedElement) {
      // Builtin reference
      const builtinDecl = scopedElement.builtin;
      const builtinType = this.types.get(builtinDecl);

      if (!builtinType) {
        throw new Error(`Builtin type not elaborated: ${node.name}`);
      }

      this.terms.set(node, { var: node.name });
      this.types.set(node, builtinType);
      return node;
    }

    if ("variant" in scopedElement) {
      const variant = scopedElement.variant;
      const parentEnum = this.variants.get(variant);
      if (!parentEnum) throw new Error("Parent enum not found");

      // Get the FULL polymorphic constructor type (e.g., ∀t u. t → Either<t,u>)
      // This is already set in visitEnumDeclaration as the wrapped arrow/forall
      const ctorPolyType = this.types.get(variant);
      if (!ctorPolyType) throw new Error("Variant constructor type not found");

      // Term: Plain variable reference (no tyapp_terms or pre-instantiation)
      // Type checker will instantiate via inferAppType when applied (e.g., Left(21))
      this.terms.set(node, { var: node.name });

      // Type: Full polymorphic type (no partial apps or metas)
      this.types.set(node, ctorPolyType);

      // Optional log for debugging
      console.log(
        `Elaborated constructor ${node.name} as plain poly var: type=${showType(ctorPolyType)}`,
      );

      return node;
    }

    if ("name_pattern" in scopedElement) {
      // Pattern binding from let expression or match arm
      const pattern = scopedElement.name_pattern;

      // Try to infer the type from the bound expression
      let bindingType: Type;
      const exprType = this.types.get(scopedElement.name_pattern);

      if (exprType) {
        // Extract the type for this specific binding from the pattern
        bindingType = this.extractPatternBindingType(
          pattern,
          node.name,
          exprType,
        );
      } else {
        bindingType = { var: this.freshVar("let_bind") };
      }

      this.terms.set(node, { var: node.name });
      this.types.set(node, bindingType);
      return node;
    }

    if ("enum" in scopedElement) {
      // This shouldn't happen in term position
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    if ("trait" in scopedElement) {
      // This shouldn't happen in term position
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    if ("type_decl" in scopedElement) {
      // This shouldn't happen in term position
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    if ("trait_fn" in scopedElement) {
      const type = this.types.get(scopedElement.trait_fn);
      if (!type) throw new Error("Type not generated for trait function.");

      this.terms.set(node, { var: node.name });
      this.types.set(node, type);
      return node;
    }

    // if ("term_import" in scopedElement) {
    //   // Imported term
    //   const importedName =
    //     scopedElement.term_import.alias?.name ??
    //     scopedElement.term_import.name.name;

    //   this.terms.set(node, { var: importedName });
    //   // Type should be resolved from the imported module
    //   // For now, use a placeholder
    //   this.types.set(node, { var: this.freshVar("import") });
    //   return node;
    // }

    if (
      "star_import" in scopedElement ||
      "type_import" in scopedElement ||
      "trait_import" in scopedElement
    ) {
      // These shouldn't appear in term position
      this.errors.push({ cannot_resolve_symbol: node });
      return node;
    }

    // In ElaboratePass.visitNameIdentifier, for projection case:
    if (this.projectionTypes.has(node.name)) {
      console.log(`Resolving "${node.name}" as trait projection`);

      // Get the projection info
      const projType = this.projectionTypes.get(node.name)!;
      const projTerm = this.projectionTerms.get(node.name)!;

      // Check if we need to instantiate with a specific dictionary
      // For now, just return the projection - dictionary passing happens at application time
      this.types.set(node, projType);
      this.terms.set(node, projTerm);
      return node;
    }

    // Unknown scope element type
    this.errors.push({ cannot_resolve_symbol: node });
    return node;
  }

  // Helper method to extract the type of a specific binding from a pattern
  private extractPatternBindingType(
    pattern: PatternExpression,
    bindingName: string,
    patternType: Type,
  ): Type {
    if ("name" in pattern && pattern.name === bindingName) {
      return patternType;
    }

    if ("tuple" in pattern && "tuple" in patternType) {
      for (let i = 0; i < pattern.tuple.length; i++) {
        const result = this.extractPatternBindingType(
          pattern.tuple[i]!,
          bindingName,
          patternType.tuple[i]!,
        );
        if (result !== unitType) return result;
      }
    }

    if ("record" in pattern && "record" in patternType) {
      for (const [fieldName, fieldPattern] of pattern.record) {
        const fieldType = patternType.record.find(
          ([label]) => label === fieldName.name,
        );
        if (fieldType) {
          const result = this.extractPatternBindingType(
            fieldPattern,
            bindingName,
            fieldType[1],
          );
          if (result !== unitType) return result;
        }
      }
    }

    if ("constr" in pattern && "variant" in patternType) {
      const variantCase = patternType.variant.find(
        ([label]) => label === pattern.constr.type.type,
      );
      if (variantCase) {
        for (const innerPattern of pattern.constr.patterns) {
          const result = this.extractPatternBindingType(
            innerPattern,
            bindingName,
            variantCase[1],
          );
          if (result !== unitType) return result;
        }
      }
    }

    return unitType; // Not found in this pattern
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    super.visitEnumDeclaration(node);

    const enumId = node.enum.id.type;
    const params = node.enum.type_params; // e.g., ["t", "u"]
    const kind = computeEnumKind(params);

    // Build variants as FieldScheme (unbound param vars)
    const variants: [string, FieldScheme][] = [];
    for (const v of node.enum.variants) {
      let fieldScheme: FieldScheme;
      if ("fields" in v) {
        const fieldTypes = v.fields.fields.map((f) => this.types.get(f.ty)!);
        // Unwrap single fields
        fieldScheme =
          fieldTypes.length === 1 ? fieldTypes[0]! : { tuple: fieldTypes };
      } else {
        const fieldTypes = v.values.values.map((ty) => this.types.get(ty)!);
        // Unwrap single fields for positional variants too
        fieldScheme =
          fieldTypes.length === 1 ? fieldTypes[0]! : { tuple: fieldTypes };
      }
      variants.push([
        "fields" in v ? v.fields.id.type : v.values.id.type,
        fieldScheme,
      ]);
    }

    // NEW: Push EnumDef to context
    const enumDef: EnumDef = {
      enum: {
        name: enumId,
        kind,
        params: params.map((p) => p.name), // ["t", "u"]
        variants,
      },
    };
    this.context.push({ enum: enumDef });

    // Bind the family type { con: "Either" } with kind * → * → *
    this.context.push({ type: { name: enumId, kind } });
    this.types.set(node, con_type(enumId));

    // Build nominal instance type constructor (nested app: app(app(con, param1), param2))
    let familyType = con_type(enumId) as Type;
    for (const param of params) {
      // param is NameIdentifier, type is {var: param.name}
      const paramType = { var: param.name };
      familyType = app_type(familyType, paramType); // Left-assoc: app(app(con, t), u)
    }

    const isRecursive = variants.some(
      ([_, scheme]) =>
        node.enum.recursive || collectTypeVars(scheme).includes(enumId),
    );

    familyType = isRecursive ? mu_type(enumId, familyType) : familyType;
    this.types.set(node, familyType);

    for (const v of node.enum.variants) {
      const label = "fields" in v ? v.fields.id.type : v.values.id.type;
      const fieldScheme = variants.find(([l]) => l === label)![1];
      const resultType = familyType;

      // Check if nullary (unit/zero-field constructor)
      const isNullary =
        "tuple" in fieldScheme && fieldScheme.tuple.length === 0;

      let ctorType: Type;
      let ctorTerm: Term;

      if (isNullary) {
        // Nullary: direct inject with unit value
        ctorType = resultType;
        ctorTerm = {
          inject: {
            label,
            value: { tuple: [] }, // Unit value
            variant_type: resultType,
          },
        };
      } else {
        // Non-nullary: wrap in lambda as before
        ctorType = arrow_type(fieldScheme, resultType);
        ctorTerm = {
          inject: {
            label,
            value: { var: "$fields" },
            variant_type: resultType,
          },
        };
        ctorTerm = lam_term(
          "$fields",
          fieldScheme,
          isRecursive
            ? fold_term(familyType, ctorTerm) // Wrap body
            : ctorTerm,
        );
      }

      // Wrap foralls (unchanged)
      for (let i = params.length - 1; i >= 0; i--) {
        ctorType = forall_type(params[i]!.name, starKind, ctorType);
        ctorTerm = tylam_term(params[i]!.name, starKind, ctorTerm);
      }

      this.types.set(v, ctorType);
      this.terms.set(v, ctorTerm);
    }

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

    if (this.mode === "type" && node.type === "Self") {
      this.types.set(node, { var: "Self" });
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

        // Just use the constructor name
        this.types.set(node, con_type(enumDecl.enum.id.type));
        return node;
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

  // In ElaboratePass.visitTraitDeclaration:
  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    const trait_decl = node.trait;

    // Build method signatures
    const methods: [string, Type][] = [];

    // First, visit all type expressions to populate the type map
    for (const traitFn of trait_decl.fns) {
      for (const param of traitFn.params) this.visitTypeExpression(param.ty);
      this.visitTypeExpression(traitFn.return_type);
    }

    // Now, analyze how Self is used to determine its kind
    let selfKind = starKind; // Default to * if Self isn't used

    // Look at all Self applications in method signatures
    for (const traitFn of trait_decl.fns) {
      // Check return type
      const returnSelfInfo = this.analyzeSelfUsage(traitFn.return_type);
      if (returnSelfInfo.isUsed) {
        // Update kind based on the arity
        selfKind = starKind;
        for (let i = 0; i < returnSelfInfo.arity; i++)
          selfKind = arrow_kind(starKind, selfKind);
      }

      // Check parameter types
      for (const param of traitFn.params) {
        const paramSelfInfo = this.analyzeSelfUsage(param.ty);
        if (paramSelfInfo.isUsed) {
          // Update kind based on the arity
          selfKind = starKind;
          for (let i = 0; i < paramSelfInfo.arity; i++)
            selfKind = arrow_kind(starKind, selfKind);
        }
      }
    }

    // Now build method types
    // Create unique renaming for this trait's method generics
    const traitName = trait_decl.id.type;
    const methodGenericRenaming = new Map<string, string>();
    let renameCounter = 0;

    // Collect all generic vars EXCEPT "Self"
    const allMethodGenerics = new Set<string>();
    for (const traitFn of trait_decl.fns) {
      for (const tp of traitFn.type_params) {
        if (tp.name !== "Self") allMethodGenerics.add(tp.name);
      }

      const paramTypes = traitFn.params
        .map((p) => this.types.get(p.ty))
        .filter(Boolean) as Type[];
      const returnType = this.types.get(traitFn.return_type) as Type;

      // Collect vars but filter out "Self"
      const vars = new Set<string>();
      collectTypeVarsFromTypes([...paramTypes, returnType], vars);
      vars.delete("Self"); // ← KEY FIX: Don't rename Self

      for (const v of vars) {
        allMethodGenerics.add(v);
      }
    }

    // Also add trait-level params (but NOT if they're "Self")
    for (const tp of trait_decl.type_params)
      if (tp.name !== "Self") allMethodGenerics.add(tp.name);

    // Assign unique names
    for (const oldName of allMethodGenerics) {
      const newName = `τ${renameCounter++}`;
      methodGenericRenaming.set(oldName, newName);
    }

    this.traitMethodRenamings.set(traitName, methodGenericRenaming);

    // Build method types with renaming (but Self stays as Self)
    for (const traitFn of trait_decl.fns) {
      // Apply renaming to param/return types (Self won't be renamed)
      const paramTypes: Type[] = traitFn.params.map((p) => {
        let ty = this.types.get(p.ty)!;
        for (const [oldName, newName] of methodGenericRenaming.entries()) {
          ty = substituteType(oldName, { var: newName }, ty);
        }
        return ty;
      });

      let returnType = this.types.get(traitFn.return_type)!;
      for (const [oldName, newName] of methodGenericRenaming.entries()) {
        returnType = substituteType(oldName, { var: newName }, returnType);
      }

      // Build Self application (using RENAMED trait param if present)
      let selfType: Type = { var: "Self" }; // ← Keep as "Self"
      if (trait_decl.type_params.length > 0) {
        const origParam = trait_decl.type_params[0]!.name;
        // Only rename if it's not "Self" itself
        const renamedParam =
          origParam === "Self"
            ? "Self"
            : methodGenericRenaming.get(origParam) || origParam;

        selfType = {
          app: {
            func: { var: "Self" }, // ← Always "Self" for the constructor
            arg: { var: renamedParam }, // ← Use renamed arg (e.g., τ0)
          },
        };
      }

      // Build method type: Self → param₁ → ... → result
      let methodType: Type = returnType;
      methodType = { arrow: { from: selfType, to: methodType } };

      for (let i = paramTypes.length - 1; i >= 0; i--) {
        methodType = { arrow: { from: paramTypes[i]!, to: methodType } };
      }

      // Collect free vars (excluding Self and already bound)
      const boundVars = new Set(
        traitFn.type_params.map((tp) => {
          return tp.name === "Self"
            ? "Self"
            : methodGenericRenaming.get(tp.name) || tp.name;
        }),
      );
      boundVars.add("Self"); // ← Self is implicitly bound by the trait

      // Now: Rename ALL free vars in the full methodType (including inner Self apps)
      const renamedMethodType = this.renameFreeVarsInType(
        methodType,
        methodGenericRenaming,
      );

      // For generality foralls: Since we renamed free vars, and bound vars may need adjustment if clashing
      // But bound (method type_params) are already in renaming map (e.g., if method has ∀t. ..., t → τ0)
      // Wait: For method's own foralls, the var is renamed in the wrapping loop below, but body uses renamed.
      const sigFreeVars = collectTypeVars(renamedMethodType).filter(
        (v) => !boundVars.has(v),
      );
      const freeVarsInSig = sigFreeVars.sort().reverse();

      // Wrap as before, but body is already renamed
      let generalizedMethodType = renamedMethodType;
      for (const freeVar of freeVarsInSig.sort().reverse()) {
        // freeVars now use renamed names (e.g., τ2 if added)
        generalizedMethodType = {
          forall: {
            var: freeVar,
            kind: starKind,
            body: generalizedMethodType,
          },
        };
      }

      for (let i = traitFn.type_params.length - 1; i >= 0; i--) {
        const origVar = traitFn.type_params[i]!.name;
        if (origVar === "Self") continue;
        const renamedVar = methodGenericRenaming.get(origVar) || origVar; // Already renamed
        generalizedMethodType = {
          forall: {
            var: renamedVar,
            kind: starKind,
            body: generalizedMethodType,
          },
        };
      }

      methods.push([traitFn.name.name, generalizedMethodType]);
      this.types.set(traitFn, generalizedMethodType); // Cache renamed
    }

    const traitDef: TraitDef = {
      name: trait_decl.id.type,
      type_param: "Self",
      kind: selfKind, // Use the computed kind for Self
      methods,
    };

    // Store in context for later lookup
    this.context.push({ trait_def: traitDef });

    // Also store a marker type
    this.types.set(node, { con: `Trait<${trait_decl.id.type}>` });

    for (const [methodName, methodType] of traitDef.methods) {
      const projKey = methodName; // e.g., "map" (assume unique; qualify if needed: `${traitName}.${methodName}`)

      // Derive projection type: bounded_forall { Self :: kind where { Map<Self> }. methodType }
      // methodType already has foralls for t,u and "Self" vars (e.g., ∀t u. (t→u) → Self t → Self u)
      const projectionType: BoundedForallType = {
        bounded_forall: {
          var: "Self",
          kind: traitDef.kind, // e.g., { arrow: { from: *, to: * } }
          constraints: [
            {
              trait: traitName, // "Map"
              type: { var: "Self" }, // Self must implement Map
            },
          ],
          body: methodType, // Already correct: uses "Self" as var (no sub needed)
        },
      };

      // Derive projection term: trait_lam that abstracts over Self and provides dict
      const projectionTerm: TraitLamTerm = {
        trait_lam: {
          trait_var: `dict_${projKey}`, // Temp dict var (bound at use via trait_app)
          trait: traitName,
          type_var: "Self",
          kind: traitDef.kind,
          constraints: projectionType.bounded_forall.constraints, // Same as above
          body: {
            trait_method: {
              dict: { var: `dict_${projKey}` }, // Ref to the provided dict (inferred at app)
              method: methodName,
            },
          },
        },
      };

      // Cache
      this.projectionTypes.set(projKey, {
        bounded_forall: projectionType.bounded_forall,
      });
      this.projectionTerms.set(projKey, projectionTerm);

      console.log(
        `Cached projection for ${methodName} (${traitName}): ${showType({ bounded_forall: projectionType.bounded_forall })}`,
      );
    }

    return node;
  }

  // Helper to analyze Self usage in a type expression
  private analyzeSelfUsage(typeExpr: TypeExpression): {
    isUsed: boolean;
    arity: number;
  } {
    if ("type" in typeExpr && typeExpr.type === "Self") {
      return { isUsed: true, arity: 0 }; // Self itself, not an application
    }

    if ("app" in typeExpr) {
      // Check if the root is Self
      const root = this.getApplicationRoot(typeExpr);
      if ("type" in root && root.type === "Self") {
        // Count the arguments
        return { isUsed: true, arity: this.countApplicationArgs(typeExpr) };
      }
    }

    return { isUsed: false, arity: 0 };
  }

  private getApplicationRoot(typeExpr: TypeExpression): TypeExpression {
    if ("app" in typeExpr) {
      return this.getApplicationRoot(typeExpr.app.callee);
    }
    return typeExpr;
  }

  private countApplicationArgs(typeExpr: TypeExpression): number {
    if ("app" in typeExpr) {
      return typeExpr.app.args.length;
    }
    return 0;
  }

  override visitImplDeclaration(node: ImplDeclaration): Declaration {
    const impl_decl = node.impl;

    // Visit the target type
    this.visitTypeExpression(impl_decl.for);
    const forType = this.types.get(impl_decl.for);
    if (!forType) throw new Error("Impl 'for' type not elaborated");

    console.log("Raw forType structure:", JSON.stringify(forType, null, 2));
    console.log(
      "impl type_params:",
      impl_decl.type_params.map((tp) =>
        "name" in tp ? tp.name : "type" in tp ? tp.type : "??",
      ),
    );
    console.log(
      "Are any type_param nodes same as for args?",
      impl_decl.type_params.some((tp) => {
        // Check if tp appears in impl_decl.for
        if ("app" in impl_decl.for) {
          return impl_decl.for.app.args.includes(tp);
        }
        return false;
      }),
    );

    // Get the trait definition
    const traitDef = this.context.find(
      (b) => "trait_def" in b && b.trait_def.name === impl_decl.name.type,
    );

    if (!traitDef || !("trait_def" in traitDef)) {
      this.errors.push({ cannot_resolve_symbol: impl_decl.name });
      return node;
    }

    // Create a temporary context [REMOVED: No adding trait's original params 't','u']
    const implContext: Context = [...this.context];

    // [FIX: Add impl's type parameters to context, even for vars]
    const implParamVars: string[] = [];
    for (const typeParam of impl_decl.type_params) {
      this.visitTypeExpression(typeParam);
      const paramTy = this.types.get(typeParam);
      if (paramTy && "var" in paramTy) {
        const v = paramTy.var;
        implParamVars.push(v);
        if (!implContext.some((e) => "type" in e && e.type.name === v)) {
          implContext.push({ type: { name: v, kind: starKind } });
        }
      }
      // For concrete, elaborate but don't add as var
    }

    // Extract type variables from the 'for' type (e.g., 'l', 'r')
    const forTypeVars = this.extractTypeVarsFromForType(impl_decl.for);

    // Add them if not already present (adds 'l'; 'r' already from above)
    for (const typeVar of forTypeVars) {
      if (
        !implContext.some(
          (entry) => "type" in entry && entry.type.name === typeVar,
        )
      ) {
        implContext.push({
          type: {
            name: typeVar,
            kind: starKind,
          },
        });
      }
    }

    // Elaborate each method implementation
    const methodImpls: [string, Term][] = [];
    for (const fn of impl_decl.fns) {
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

    // [FIX: Skolems only over true free vars (exclude impl param vars like 'r'; only 'l')]
    const allFreeVars = collectTypeVars(forType);
    const skolems = allFreeVars.filter((v) => !implParamVars.includes(v)); // e.g., ['l']

    // Wrap in type lambdas for each skolem (only 'l')
    for (let i = skolems.length - 1; i >= 0; i--) {
      dictTerm = {
        tylam: {
          var: skolems[i]!,
          kind: starKind,
          body: dictTerm,
        },
      };
    }

    // [REMOVED: The entire kind computation, areKindsCompatible, app/lam branches, and arity wrapping]
    // (Elaboration doesn't need deep kind checks; defer to type checker)

    // Compute the dictionary type: ∀l. Dict<Map<r, u>, Either<l, r>>
    let dictType: Type = {
      con: `Dict<${impl_decl.name.type}, ${showType(forType)}>`,
    };

    // Wrap foralls for skolems (only 'l')
    for (let i = skolems.length - 1; i >= 0; i--) {
      dictType = {
        forall: {
          var: skolems[i]!,
          kind: starKind,
          body: dictType,
        },
      };
    }

    this.terms.set(node, dictTerm);
    this.types.set(node, dictType);

    if (
      impl_decl.name.type === "Map" &&
      getTypeConstructorName(impl_decl.for) === "Either"
    ) {
      console.log("Handling Either Map implementation");

      // Extract both parameters 'l' and 'r' properly
      const leftVar = "l"; // From Either l r
      const rightVar = "r"; // From Either l r

      // Make sure both are in the context
      if (!implContext.some((e) => "type" in e && e.type.name === leftVar)) {
        implContext.push({ type: { name: leftVar, kind: starKind } });
      }
      if (!implContext.some((e) => "type" in e && e.type.name === rightVar)) {
        implContext.push({ type: { name: rightVar, kind: starKind } });
      }

      // ... rest of the method processing ...
    }

    // Strip impl param apps to match trait's Self kind (e.g., Either<l,r> → Either<l>)
    let strippedForType = normalizeType(forType);
    for (let i = 0; i < implParamVars.length; i++) {
      if ("app" in strippedForType) {
        strippedForType = strippedForType.app.func; // Peel one app
      } else {
        break; // Shouldn't happen, but safety
      }
    }
    console.log(
      `Stripped forType for registration: ${showType(strippedForType)}`,
    );

    // Use strippedForType for the impl binding (but keep full forType for dict type)
    // Wrap the stripped type in foralls for skolems (e.g., ∀l. Either<l>)
    let implType = strippedForType;
    for (let i = skolems.length - 1; i >= 0; i--) {
      implType = forall_type(skolems[i]!, starKind, implType);
    }

    this.context.push({
      trait_impl: {
        trait: impl_decl.name.type,
        type: implType, // ← Now ∀l::*. Either<l> instead of Either<l>
        dict: dictTerm,
      },
    });
    return node;
  }

  // Helper to extract type variables from a 'for' type by analyzing the AST
  private extractTypeVarsFromForType(
    typeExpr: TypeExpression,
    vars = new Set<string>(),
  ): string[] {
    if ("type" in typeExpr) {
      // Check if this is a type constructor in the context
      const isTypeConstructor = this.context.some(
        (entry) => "type" in entry && entry.type.name === typeExpr.type,
      );

      // Only add if it's not a type constructor
      if (!isTypeConstructor) {
        vars.add(typeExpr.type);
      }
    } else if ("app" in typeExpr) {
      this.extractTypeVarsFromForType(typeExpr.app.callee, vars);
      for (const arg of typeExpr.app.args) {
        this.extractTypeVarsFromForType(arg, vars);
      }
    }

    return Array.from(vars);
  }

  // Add to ElaboratePass class (type_checker.ts or elaborate.ts)
  private renameFreeVarsInType(
    type: Type,
    renaming: Map<string, string>,
  ): Type {
    if ("var" in type) {
      const newName = renaming.get(type.var) || type.var;
      return { var: newName };
    }

    if ("arrow" in type) {
      return {
        arrow: {
          from: this.renameFreeVarsInType(type.arrow.from, renaming),
          to: this.renameFreeVarsInType(type.arrow.to, renaming),
        },
      };
    }

    if ("app" in type)
      return app_type(
        this.renameFreeVarsInType(type.app.func, renaming),
        this.renameFreeVarsInType(type.app.arg, renaming),
      );

    if ("forall" in type)
      // Don't rename bound var; recurse on body
      // If bound var is to be renamed (if it's a method generic), but since renaming excludes bound, keep
      return forall_type(
        type.forall.var, // Bound vars not in renaming
        type.forall.kind,
        this.renameFreeVarsInType(type.forall.body, renaming),
      );

    if ("lam" in type)
      // Similar to forall
      return lam_type(
        type.lam.var,
        type.lam.kind,
        this.renameFreeVarsInType(type.lam.body, renaming),
      );

    if ("record" in type) {
      const record = type.record.map(([label, ty]) => [
        label,
        this.renameFreeVarsInType(ty, renaming),
      ]) as [string, Type][];
      return { record };
    }

    if ("variant" in type) {
      return {
        variant: type.variant.map(([label, ty]) => [
          label,
          this.renameFreeVarsInType(ty, renaming),
        ]),
      };
    }

    if ("tuple" in type) {
      return {
        tuple: type.tuple.map((ty) => this.renameFreeVarsInType(ty, renaming)),
      };
    }

    // con, never, mu, bounded_forall: recurse if needed
    if ("bounded_forall" in type) {
      return {
        bounded_forall: {
          var: type.bounded_forall.var,
          kind: type.bounded_forall.kind,
          constraints: type.bounded_forall.constraints.map((c) => ({
            trait: c.trait,
            type: this.renameFreeVarsInType(c.type, renaming),
          })),
          body: this.renameFreeVarsInType(type.bounded_forall.body, renaming),
        },
      };
    }

    if ("mu" in type)
      return mu_type(
        type.mu.var,
        this.renameFreeVarsInType(type.mu.body, renaming),
      );

    return type; // Fallback
  }
}
