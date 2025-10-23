import type { NameToken, StringToken, TypeToken } from "../lexer.js";
import type {
  ApplicationTypeExpression,
  BlockExpression,
  BodyExpression,
  BoolExpression,
  CallExpression,
  Expression,
  FloatExpression,
  Fn,
  FnExpression,
  FnTypeExpression,
  Identifier,
  IfExpression,
  IntExpression,
  LetBindBodyExpression,
  NamedTypeExpression,
  PatternExpression,
  RecordExpression,
  RecordTypeExpression,
  SelectExpression,
  SelectTypeExpression,
  SelfExpression,
  TupleExpression,
  TupleTypeExpression,
  TypeExpression,
} from "../parser.js";
import type { Pattern, Term, Type } from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import type { Scopable, Scope } from "./create_scopes.js";

function arrows(args: Type[], ret: Type): Type {
  return args.reduceRight((acc, from) => ({ arrow: { from, to: acc } }), ret);
}

export type ElaborationError =
  | { not_a_record_type: TypeExpression }
  | { record_has_no_field: { type: TypeExpression; field: string } }
  | { cannot_resolve_symbol: Identifier };

type VisitorMode = "type" | "term" | "pattern";

export class ElaboratePass extends BaseVisitor {
  private modeStack: VisitorMode[] = [];
  errors = [] as ElaborationError[];
  types = new Map<TypeExpression, Type>();
  terms = new Map<BodyExpression | Expression | Fn, Term>();
  patterns = new Map<PatternExpression, Pattern>();

  constructor(public scopes: Map<Scopable, Scope>) {
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
              arg: { con: { name: "Unit", type: { con: "Unit" } } },
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
    const scrutinee = this.terms.get(node.if_expr.cond);
    if (!scrutinee) throw new Error("Expression not generated.");

    const trueCase = this.terms.get(node.if_expr.if_body);
    if (!trueCase) throw new Error("Expression not generated.");

    const falseCase = node.if_expr.else_body
      ? this.terms.get(node.if_expr.else_body)
      : { con: { name: "Unit", type: { con: "Unit" } } };
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
    const record = this.terms.get(node.select[0]);
    if (!record) throw new Error("Expression not generated");

    this.terms.set(node, { project: { record, label: node.select[1].name } });
    return node;
  }

  override visitCallExpression(node: CallExpression): Expression {
    const [fn, params] = node.call;
    const callee = this.terms.get(fn);
    if (!callee) throw new Error("Expression not generated.");

    let term: Term | undefined =
      params.length === 0
        ? { con: { name: "Unit", type: { con: "Unit" } } }
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

  override visitNameIdentifier(node: NameToken): NameToken {
    if (this.mode === "type") {
      this.types.set(node, { var: node.name });
    } else if (this.mode === "term") {
      this.terms.set(node, { var: node.name } as Term);
    }

    // TODO: Pattern mode
    return node;
  }

  override visitTypeIdentifier(node: TypeToken): TypeToken {
    const scope = this.scopes.get(node);
    if (!scope) throw new Error("Scope not found for identifier!");

    if (this.mode === "type") {
      const item = scope.type_elements.get(node.type);
      if (!item) {
        this.errors.push({ cannot_resolve_symbol: node });
        return node;
      }

      // enum: EnumDeclaration
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
    // TODO: Expression mode
    // TODO: Pattern mode
    return node;
  }
}
