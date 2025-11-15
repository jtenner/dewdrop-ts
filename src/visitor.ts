import type { TypingError } from "system-f-omega";
import type { ModuleEntry } from "./graph.js";
import type { IntToken, NameToken, StringToken, TypeToken } from "./lexer.js";
import type {
  ApplicationTypeExpression,
  ArrowKindBodyExpression,
  AssignBodyExpression,
  BlockExpression,
  BodyExpression,
  BoolExpression,
  BuiltinDeclaration,
  CallExpression,
  ConstructorImport,
  ConstructorPatternExpression,
  Declaration,
  EnumDeclaration,
  EnumImport,
  EnumVariant,
  Expression,
  ExpressionBodyExpression,
  FloatExpression,
  FloatPatternExpression,
  Fn,
  FnDeclaration,
  FnExpression,
  FnImport,
  FnParam,
  FnSignature,
  FnTypeExpression,
  GlobalImport,
  IfExpression,
  ImplDeclaration,
  Import,
  ImportDeclaration,
  InfixExpression,
  IntExpression,
  IntPatternExpression,
  LetBindBodyExpression,
  LetDeclaration,
  MatchArm,
  MatchExpression,
  MemoryImport,
  Module,
  NamedTypeExpression,
  NameIdentifier,
  NameImport,
  PatternExpression,
  PostfixExpression,
  PrefixExpression,
  RecordExpression,
  RecordPatternExpression,
  RecordTypeExpression,
  SelectExpression,
  SelectTypeExpression,
  SelfExpression,
  StarImport,
  StringPatternExpression,
  TableImport,
  TraitDeclaration,
  TraitFn,
  TraitImport,
  TupleExpression,
  TuplePatternExpression,
  TupleTypeExpression,
  TypeDeclaration,
  TypeExpression,
  TypeImport,
} from "./parser.js";
import type { ASTNode, CompilerContext, Scope } from "./util.js";

// Visitor interface with transform capability
export interface ASTVisitor {
  // Module
  visitModule(node: Module): Module;

  // Declarations
  visitFnDeclaration(node: FnDeclaration): Declaration;
  visitEnumDeclaration(node: EnumDeclaration): Declaration;
  visitImportDeclaration(node: ImportDeclaration): Declaration;
  visitTypeDeclaration(node: TypeDeclaration): Declaration;
  visitLetDeclaration(node: LetDeclaration): Declaration;
  visitTraitDeclaration(node: TraitDeclaration): Declaration;
  visitImplDeclaration(node: ImplDeclaration): Declaration;
  visitBuiltinDeclaration(node: BuiltinDeclaration): Declaration;

  // Type Expressions
  visitTypeExpression(node: TypeExpression): TypeExpression;
  visitSelectTypeExpression(node: SelectTypeExpression): TypeExpression;
  visitApplicationTypeExpression(
    node: ApplicationTypeExpression,
  ): TypeExpression;
  visitFnTypeExpression(node: FnTypeExpression): TypeExpression;
  visitRecordTypeExpression(node: RecordTypeExpression): TypeExpression;
  visitTupleTypeExpression(node: TupleTypeExpression): TypeExpression;

  // Body Expressions
  visitBodyExpression(node: BodyExpression): BodyExpression;
  visitArrowKindBodyExpression(node: ArrowKindBodyExpression): BodyExpression;
  visitLetBindBodyExpression(node: LetBindBodyExpression): BodyExpression;
  visitAssignBodyExpression(node: AssignBodyExpression): BodyExpression;
  visitExpressionBodyExpression(node: ExpressionBodyExpression): BodyExpression;

  // Expressions
  visitExpression(node: Expression): Expression;
  visitCallExpression(node: CallExpression): Expression;
  visitBlockExpression(node: BlockExpression): Expression;
  visitIfExpression(node: IfExpression): Expression;
  visitSelectExpression(node: SelectExpression): Expression;
  visitMatchExpression(node: MatchExpression): Expression;
  visitFloatExpression(node: FloatExpression): Expression;
  visitIntExpression(node: IntExpression): Expression;
  visitBoolExpression(node: BoolExpression): Expression;
  visitPrefixExpression(node: PrefixExpression): Expression;
  visitPostfixExpression(node: PostfixExpression): Expression;
  visitInfixExpression(node: InfixExpression): Expression;
  visitStringExpression(node: StringToken): Expression;
  visitFnExpression(node: FnExpression): Expression;
  visitRecordExpression(node: RecordExpression): Expression;
  visitTupleExpression(node: TupleExpression): Expression;
  visitSelfExpression(node: SelfExpression): Expression;

  // Pattern Expressions
  visitPatternExpression(node: PatternExpression): PatternExpression;
  visitConstructorPatternExpression(
    node: ConstructorPatternExpression,
  ): PatternExpression;
  visitIntPatternExpression(node: IntPatternExpression): PatternExpression;
  visitFloatPatternExpression(node: FloatPatternExpression): PatternExpression;
  visitStringPatternExpression(
    node: StringPatternExpression,
  ): PatternExpression;
  visitRecordPatternExpression(
    node: RecordPatternExpression,
  ): PatternExpression;
  visitTuplePatternExpression(node: TuplePatternExpression): PatternExpression;

  // Other nodes
  visitMatchArm(node: MatchArm): MatchArm;
  visitEnumVariant(node: EnumVariant): EnumVariant;
  visitFn(node: Fn): Fn;
  visitTraitFn(node: TraitFn): TraitFn;
  visitFnParam(node: FnParam): FnParam;
  visitNamedTypeExpression(node: NamedTypeExpression): NamedTypeExpression;

  visitImport(node: Import): Import;
  visitTypeImport(node: TypeImport): Import;
  visitFnImport(node: FnImport): Import;
  visitGlobalImport(node: GlobalImport): Import;
  visitTableImport(node: TableImport): Import;
  visitStarImport(node: StarImport): Import;
  visitMemoryImport(node: MemoryImport): Import;
  visitNameImport(node: NameImport): Import;
  visitTraitImport(node: TraitImport): Import;
  visitConstructorImport(node: ConstructorImport): Import;

  visitFnSignature(node: FnSignature): FnSignature;
  visitTypeIdentifier(node: TypeToken): TypeToken;
  visitNameIdentifier(node: NameToken): NameToken;
  visitString(node: StringToken): StringToken;
  visitInt(node: IntToken): IntToken;
}

export interface ASTWalker {
  // Module
  walkModule(node: Module): void;

  // Declarations
  walkFnDeclaration(node: FnDeclaration): void;
  walkEnumDeclaration(node: EnumDeclaration): void;
  walkImportDeclaration(node: ImportDeclaration): void;
  walkTypeDeclaration(node: TypeDeclaration): void;
  walkLetDeclaration(node: LetDeclaration): void;
  walkTraitDeclaration(node: TraitDeclaration): void;
  walkImplDeclaration(node: ImplDeclaration): void;
  walkBuiltinDeclaration(node: BuiltinDeclaration): void;

  // Type Expressions
  walkTypeExpression(node: TypeExpression): void;
  walkSelectTypeExpression(node: SelectTypeExpression): void;
  walkApplicationTypeExpression(node: ApplicationTypeExpression): void;
  walkFnTypeExpression(node: FnTypeExpression): void;
  walkRecordTypeExpression(node: RecordTypeExpression): void;
  walkTupleTypeExpression(node: TupleTypeExpression): void;

  // Body Expressions
  walkBodyExpression(node: BodyExpression): void;
  walkArrowKindBodyExpression(node: ArrowKindBodyExpression): void;
  walkLetBindBodyExpression(node: LetBindBodyExpression): void;
  walkAssignBodyExpression(node: AssignBodyExpression): void;
  walkExpressionBodyExpression(node: ExpressionBodyExpression): void;

  // Expressions
  walkExpression(node: Expression): void;
  walkCallExpression(node: CallExpression): void;
  walkBlockExpression(node: BlockExpression): void;
  walkIfExpression(node: IfExpression): void;
  walkSelectExpression(node: SelectExpression): void;
  walkMatchExpression(node: MatchExpression): void;
  walkFloatExpression(node: FloatExpression): void;
  walkIntExpression(node: IntExpression): void;
  walkBoolExpression(node: BoolExpression): void;
  walkPrefixExpression(node: PrefixExpression): void;
  walkPostfixExpression(node: PostfixExpression): void;
  walkInfixExpression(node: InfixExpression): void;
  walkFnExpression(node: FnExpression): void;
  walkRecordExpression(node: RecordExpression): void;
  walkTupleExpression(node: TupleExpression): void;
  walkSelfExpression(node: SelfExpression): void;

  // Pattern Expressions
  walkPatternExpression(node: PatternExpression): void;
  walkConstructorPatternExpression(node: ConstructorPatternExpression): void;
  walkIntPatternExpression(node: IntPatternExpression): void;
  walkFloatPatternExpression(node: FloatPatternExpression): void;
  walkStringPatternExpression(node: StringPatternExpression): void;
  walkRecordPatternExpression(node: RecordPatternExpression): void;
  walkTuplePatternExpression(node: TuplePatternExpression): void;

  // Other nodes
  walkMatchArm(node: MatchArm): void;
  walkEnumVariant(node: EnumVariant): void;
  walkImport(node: Import): void;
  walkFn(node: Fn): void;
  walkTraitFn(node: TraitFn): void;
  walkFnParam(node: FnParam): void;
  walkNamedTypeExpression(node: NamedTypeExpression): void;

  walkTypeImport(node: TypeImport): void;
  walkFnImport(node: FnImport): void;
  walkEnumImport(node: EnumImport): void;
  walkGlobalImport(node: GlobalImport): void;
  walkTableImport(node: TableImport): void;
  walkStarImport(node: StarImport): void;
  walkMemoryImport(node: MemoryImport): void;
  walkNameImport(node: NameImport): void;
  walkTraitImport(node: TraitImport): void;
  walkConstructorImport(node: ConstructorImport): void;
  walkFnSignature(node: FnSignature): void;
  walkTypeIdentifier(node: TypeToken): void;
  walkNameIdentifier(node: NameToken): void;
  walkString(node: StringToken): void;
  walkInt(node: IntToken): void;
}

export type IDMode = "ambient" | "expression" | "type" | "pattern";

export type UnboundError = { unbound: { name: string; scope: Scope } };
export type InvalidNameError = { invalid_name: ASTNode };
export type TypeError = { type_error: TypingError };
export type TypeParameterCountMismatchError = {
  type_parameter_count: {
    node: ASTNode;
    actual: number;
    expected: number;
  };
};
export type NotAFunctionError = {
  not_a_function: ASTNode;
};
export type NotAConstructorError = {
  not_a_constructor: ASTNode;
};
export type NotATypeError = {
  not_a_type: ASTNode;
};
export type CompilerError =
  | TypeError
  | UnboundError
  | InvalidNameError
  | TypeParameterCountMismatchError
  | NotAFunctionError
  | NotAConstructorError
  | NotATypeError;

const id = new WeakMap<Map<any, any>, number>();

export abstract class BaseContext {
  constructor(public context: CompilerContext) {}

  modulePath: string = "";
  idMode: IDMode = "ambient";
  errors: CompilerError[] = [];
  abstract visit(module: ModuleEntry): ModuleEntry;

  getScope(node: ASTNode): Scope | null {
    return this.context.scopes.get(node) ?? null;
  }

  setScope(node: ASTNode, scope: Scope) {
    this.context.scopes.set(node, scope);
  }

  lookupTerm(node: ASTNode, name: string) {
    let scope = this.context.scopes.get(node) ?? null;
    if (!scope) throw new Error(`Node scope not found!`);

    while (scope) {
      const term = scope.terms.get(name);
      if (term) return term;
      scope = scope.parent;
    }

    return null;
  }

  lookupType(node: ASTNode, name: string) {
    let scope = this.context.scopes.get(node) ?? null;
    if (!scope) {
      for (const scopeEntry of this.context.scopes.keys()) {
        if ("impl" in scopeEntry) {
          console.log(scopeEntry.position);
          if (node === scopeEntry) console.log("Hit!");
        }
      }
      throw new Error(`Node scope not found!`);
    }

    while (scope) {
      const ty = scope.types.get(name);
      if (ty) return ty;
      scope = scope.parent;
    }

    return null;
  }
}

export class BaseWalker extends BaseContext implements ASTWalker {
  override visit(module: ModuleEntry) {
    this.modulePath = module.relativePath;
    this.walkModule(module.module!);
    return module;
  }

  // Module
  walkModule(node: Module): void {
    for (const decl of node.module) {
      this.walkDeclaration(decl);
    }
  }

  // Declarations
  walkDeclaration(node: Declaration) {
    if ("builtin" in node) this.walkBuiltinDeclaration(node);
    else if ("enum" in node) this.walkEnumDeclaration(node);
    else if ("fn" in node) this.walkFnDeclaration(node);
    else if ("impl" in node) this.walkImplDeclaration(node);
    else if ("import_dec" in node) this.walkImportDeclaration(node);
    else if ("let_dec" in node) this.walkLetDeclaration(node);
    else if ("trait" in node) this.walkTraitDeclaration(node);
    else if ("type_dec" in node) this.walkTypeDeclaration(node);
    else throw new Error("Invalid declaration kind?");
  }
  walkFnDeclaration(node: FnDeclaration): void {
    this.walkFn(node.fn.fn);
  }
  walkEnumDeclaration(node: EnumDeclaration): void {
    this.walkTypeIdentifier(node.enum.id);
    for (const t of node.enum.type_params) {
      this.walkNameIdentifier(t);
    }
    for (const v of node.enum.variants) {
      this.walkEnumVariant(v);
    }
  }
  walkImportDeclaration(node: ImportDeclaration): void {
    for (const i of node.import_dec.imports) {
      this.walkImport(i);
    }
  }
  walkTypeDeclaration(node: TypeDeclaration): void {
    this.walkTypeIdentifier(node.type_dec.id);
    for (const p of node.type_dec.params) {
      this.walkNameIdentifier(p);
    }
    this.walkTypeExpression(node.type_dec.value);
  }
  walkLetDeclaration(node: LetDeclaration): void {
    this.walkPatternExpression(node.let_dec.pattern);
    this.walkExpression(node.let_dec.value);
  }
  walkTraitDeclaration(node: TraitDeclaration): void {
    this.walkTypeIdentifier(node.trait.id);
    for (const p of node.trait.type_params) {
      this.walkNameIdentifier(p);
    }
    for (const tfn of node.trait.fns) {
      this.walkTraitFn(tfn);
    }
  }
  walkImplDeclaration(node: ImplDeclaration): void {
    this.walkTypeIdentifier(node.impl.name);
    for (const tp of node.impl.type_params) {
      this.walkNameIdentifier(tp);
    }

    for (const tp of node.impl.trait_params) {
      this.walkTypeExpression(tp);
    }
    this.walkTypeExpression(node.impl.for);
    for (const fn of node.impl.fns) {
      this.walkFn(fn);
    }
  }
  walkBuiltinDeclaration(node: BuiltinDeclaration): void {
    this.walkString(node.builtin.name);
    for (const p of node.builtin.params) {
      this.walkFnParam(p);
    }
    this.walkTypeExpression(node.builtin.return_type);
    this.walkNameIdentifier(node.builtin.alias);
  }

  // Type Expressions
  walkTypeExpression(node: TypeExpression): void {
    const previous = this.idMode;
    this.idMode = "type";
    if ("app" in node) this.walkApplicationTypeExpression(node);
    else if ("fn" in node) this.walkFnTypeExpression(node);
    else if ("name" in node) this.walkNameIdentifier(node);
    else if ("record" in node) this.walkRecordTypeExpression(node);
    else if ("select" in node) this.walkSelectTypeExpression(node);
    else if ("tuple" in node) this.walkTupleTypeExpression(node);
    else if ("type" in node) this.walkTypeIdentifier(node);
    else throw new Error("Invalid Type Expression?");
    this.idMode = previous;
  }
  walkSelectTypeExpression(node: SelectTypeExpression): void {
    this.walkTypeExpression(node.select.root);
    if ("name" in node.select.name) this.walkNameIdentifier(node.select.name);
    else this.walkTypeIdentifier(node.select.name);
  }
  walkApplicationTypeExpression(node: ApplicationTypeExpression): void {
    this.walkTypeExpression(node.app.callee);

    for (const tp of node.app.args) {
      this.walkTypeExpression(tp);
    }
  }
  walkFnTypeExpression(node: FnTypeExpression): void {
    for (const a of node.fn.args) {
      this.walkTypeExpression(a);
    }

    this.walkTypeExpression(node.fn.ret);
  }
  walkRecordTypeExpression(node: RecordTypeExpression): void {
    for (const { ty } of node.record) {
      this.walkTypeExpression(ty);
    }
  }
  walkTupleTypeExpression(node: TupleTypeExpression): void {
    for (const t of node.tuple) {
      this.walkTypeExpression(t);
    }
  }

  // Body Expressions
  walkBodyExpression(node: BodyExpression): void {
    if ("arrow_bind" in node) this.walkArrowKindBodyExpression(node);
    else if ("assign" in node) this.walkAssignBodyExpression(node);
    else if ("expression" in node) this.walkExpressionBodyExpression(node);
    else if ("let_bind" in node) this.walkLetBindBodyExpression(node);
    else throw new Error("Invalid body expression");
  }

  walkArrowKindBodyExpression(node: ArrowKindBodyExpression): void {
    this.walkNameIdentifier(node.arrow_bind.name);
    this.walkExpression(node.arrow_bind.expression);
  }
  walkLetBindBodyExpression(node: LetBindBodyExpression): void {
    this.walkPatternExpression(node.let_bind.pattern);
    this.walkExpression(node.let_bind.expression);
  }
  walkAssignBodyExpression(node: AssignBodyExpression): void {
    this.walkNameIdentifier(node.assign.name);
    this.walkExpression(node.assign.expression);
  }
  walkExpressionBodyExpression(node: ExpressionBodyExpression): void {
    this.walkExpression(node.expression);
  }

  // Expressions
  walkExpression(node: Expression): void {
    const previous = this.idMode;
    this.idMode = "expression";
    if ("call" in node) this.walkCallExpression(node);
    else if ("block" in node) this.walkBlockExpression(node);
    else if ("bool" in node) this.walkBoolExpression(node);
    else if ("float" in node) this.walkFloatExpression(node);
    else if ("fn" in node) this.walkFnExpression(node);
    else if ("if_expr" in node) this.walkIfExpression(node);
    else if ("infix" in node) this.walkInfixExpression(node);
    else if ("int" in node) this.walkIntExpression(node);
    else if ("match" in node) this.walkMatchExpression(node);
    else if ("name" in node) this.walkNameIdentifier(node);
    else if ("postfix" in node) this.walkPostfixExpression(node);
    else if ("prefix" in node) this.walkPrefixExpression(node);
    else if ("record" in node) this.walkRecordExpression(node);
    else if ("select" in node) this.walkSelectExpression(node);
    else if ("self" in node) this.walkSelfExpression(node);
    else if ("string" in node) this.walkString(node);
    else if ("tuple" in node) this.walkTupleExpression(node);
    else if ("type" in node) this.walkTypeIdentifier(node);
    else throw new Error("Invalid Expression?");
    this.idMode = previous;
  }
  walkCallExpression(node: CallExpression): void {
    this.walkExpression(node.call[0]);
    for (const arg of node.call[1]) {
      this.walkExpression(arg);
    }
  }
  walkBlockExpression(node: BlockExpression): void {
    for (const b of node.block) {
      this.walkBodyExpression(b);
    }
  }
  walkIfExpression(node: IfExpression): void {
    this.walkExpression(node.if_expr.cond);
    this.walkExpression(node.if_expr.if_body);
    if (node.if_expr.else_body) this.walkExpression(node.if_expr.else_body);
  }
  walkSelectExpression(node: SelectExpression): void {
    this.walkExpression(node.select[0]);
    this.walkNameIdentifier(node.select[1]);
  }
  walkMatchExpression(node: MatchExpression): void {
    this.walkExpression(node.match[0]);
    for (const ma of node.match[1]) {
      this.walkMatchArm(ma);
    }
  }
  walkFloatExpression(_node: FloatExpression): void {}
  walkIntExpression(_node: IntExpression): void {}
  walkBoolExpression(_node: BoolExpression): void {}
  walkPrefixExpression(node: PrefixExpression): void {
    this.walkExpression(node.prefix.operand);
  }
  walkPostfixExpression(node: PostfixExpression): void {
    this.walkExpression(node.postfix.operand);
  }
  walkInfixExpression(node: InfixExpression): void {
    this.walkExpression(node.infix.left);
    this.walkExpression(node.infix.right);
  }
  walkStringExpression(_node: StringToken): void {}
  walkFnExpression(node: FnExpression): void {
    this.walkFn(node.fn);
  }
  walkRecordExpression(node: RecordExpression): void {
    for (const [, t] of node.record) {
      this.walkExpression(t);
    }
  }
  walkTupleExpression(node: TupleExpression): void {
    for (const t of node.tuple) {
      this.walkExpression(t);
    }
  }
  walkSelfExpression(_node: SelfExpression): void {}

  // Pattern Expressions
  walkPatternExpression(node: PatternExpression): void {
    const previous = this.idMode;
    this.idMode = "pattern";
    if ("constr" in node) this.walkConstructorPatternExpression(node);
    else if ("float" in node) this.walkFloatPatternExpression(node);
    else if ("int" in node) this.walkIntPatternExpression(node);
    else if ("name" in node) this.walkNameIdentifier(node);
    else if ("record" in node) this.walkRecordPatternExpression(node);
    else if ("string" in node) this.walkStringPatternExpression(node);
    else if ("tuple" in node) this.walkTuplePatternExpression(node);
    else throw new Error("Invalid pattern?");
    this.idMode = previous;
  }
  walkConstructorPatternExpression(node: ConstructorPatternExpression): void {
    this.walkTypeIdentifier(node.constr.type);
    for (const p of node.constr.patterns) {
      this.walkPatternExpression(p);
    }
  }
  walkIntPatternExpression(_node: IntPatternExpression): void {}
  walkFloatPatternExpression(_node: FloatPatternExpression): void {}
  walkStringPatternExpression(_node: StringPatternExpression): void {}
  walkRecordPatternExpression(node: RecordPatternExpression): void {
    for (const [, p] of node.record) {
      this.walkPatternExpression(p);
    }
  }
  walkTuplePatternExpression(node: TuplePatternExpression): void {
    for (const p of node.tuple) {
      this.walkPatternExpression(p);
    }
  }

  // Other nodes
  walkMatchArm(node: MatchArm): void {
    this.walkPatternExpression(node.pattern);
    if (node.guard) this.walkExpression(node.guard);
    this.walkExpression(node.body);
  }
  walkEnumVariant(node: EnumVariant): void {
    if ("fields" in node) {
      this.walkTypeIdentifier(node.fields.id);
      for (const f of node.fields.fields as NamedTypeExpression[]) {
        this.walkNamedTypeExpression(f);
      }
    } else if ("values" in node) {
      this.walkTypeIdentifier(node.values.id);
      for (const t of node.values.values) {
        this.walkTypeExpression(t);
      }
    }
  }

  walkFn(node: Fn): void {
    const current = this.idMode;
    this.idMode = "ambient";
    if (node.name) this.walkNameIdentifier(node.name);
    for (const tp of node.type_params) {
      this.walkNameIdentifier(tp);
    }
    for (const p of node.params) {
      this.walkFnParam(p);
    }
    if (node.return_type) this.walkTypeExpression(node.return_type);
    this.walkExpression(node.body);
    this.idMode = current;
  }

  walkTraitFn(node: TraitFn): void {
    const current = this.idMode;
    this.idMode = "ambient";
    this.walkNameIdentifier(node.name);
    for (const tp of node.type_params) {
      this.walkNameIdentifier(tp);
    }
    for (const nte of node.params) {
      this.walkNamedTypeExpression(nte);
    }
    this.walkTypeExpression(node.return_type);
    this.idMode = current;
  }
  walkFnParam(node: FnParam): void {
    this.walkNameIdentifier(node.name);
    if (node.guard) this.walkTypeExpression(node.guard);
  }
  walkNamedTypeExpression(node: NamedTypeExpression): void {
    this.walkNameIdentifier(node.name);
    this.walkTypeExpression(node.ty);
  }

  walkImport(node: Import): void {
    if ("constr" in node) this.walkConstructorImport(node);
    else if ("enum" in node) this.walkEnumImport(node);
    else if ("fn" in node) this.walkFnImport(node);
    else if ("global" in node) this.walkGlobalImport(node);
    else if ("memory" in node) this.walkMemoryImport(node);
    else if ("name" in node) this.walkNameImport(node);
    else if ("star" in node) this.walkStarImport(node);
    else if ("table" in node) this.walkTableImport(node);
    else if ("trait" in node) this.walkTraitImport(node);
    else if ("type" in node) this.walkTypeImport(node);
    else throw new Error("Invalid import?");
  }
  walkEnumImport(node: EnumImport): void {
    this.walkTypeIdentifier(node.enum.name);
    if (node.enum.alias) this.walkTypeIdentifier(node.enum.alias);
  }
  walkTypeImport(node: TypeImport): void {
    this.walkTypeIdentifier(node.type.name);
    if (node.type.alias) this.walkTypeIdentifier(node.type.name);
  }
  walkFnImport(node: FnImport): void {
    if ("name" in node.fn.name) this.walkNameIdentifier(node.fn.name);
    else this.walkString(node.fn.name);
    this.walkFnSignature(node.fn.signature);
    if (node.fn.alias) this.walkNameIdentifier(node.fn.alias);
  }
  walkGlobalImport(node: GlobalImport): void {
    if ("name" in node.global.name) this.walkNameIdentifier(node.global.name);
    else this.walkString(node.global.name);
    this.walkTypeExpression(node.global.global_type);
    if (node.global.alias) this.walkNameIdentifier(node.global.alias);
  }
  walkTableImport(node: TableImport): void {
    if ("name" in node.table.name) this.walkNameIdentifier(node.table.name);
    else this.walkString(node.table.name);
    this.walkTypeExpression(node.table.table_type);
    if (node.table.min) this.walkInt(node.table.min);
    if (node.table.max) this.walkInt(node.table.max);
    if (node.table.alias) this.walkNameIdentifier(node.table.alias);
  }
  walkStarImport(node: StarImport): void {
    this.walkTypeIdentifier(node.star);
  }
  walkMemoryImport(node: MemoryImport): void {
    if ("name" in node.memory.name) this.walkNameIdentifier(node.memory.name);
    else this.walkString(node.memory.name);
    if (node.memory.min) this.walkInt(node.memory.min);
    if (node.memory.max) this.walkInt(node.memory.max);
    if (node.memory.alias) this.walkNameIdentifier(node.memory.alias);
  }
  walkNameImport(node: NameImport): void {
    this.walkNameIdentifier(node.name.name);
    if (node.name.alias) this.walkNameIdentifier(node.name.alias);
  }
  walkTraitImport(node: TraitImport): void {
    this.walkTypeIdentifier(node.trait.name);
    if (node.trait.alias) this.walkTypeIdentifier(node.trait.alias);
  }
  walkConstructorImport(node: ConstructorImport): void {
    this.walkTypeIdentifier(node.constr.name);
    if (node.constr.alias) this.walkTypeIdentifier(node.constr.alias);
  }
  walkFnSignature(node: FnSignature): void {
    for (const p of node.param_types) {
      this.walkFnParam(p);
    }
    this.walkTypeExpression(node.return_type);
  }

  walkTypeIdentifier(_node: TypeToken): void {}
  walkNameIdentifier(_node: NameToken): void {}
  walkString(_node: StringToken): void {}
  walkInt(_node: IntToken): void {}
}

// Default visitor that recursively visits and reconstructs the tree
export class BaseVisitor extends BaseContext implements ASTVisitor {
  override visit(entry: ModuleEntry) {
    this.modulePath = entry.relativePath;
    entry.module = this.visitModule(entry.module!);
    return entry;
  }

  visitModule(node: Module): Module {
    return {
      module: node.module.map((decl) => this.visitDeclaration(decl)),
    };
  }

  visitDeclaration(node: Declaration): Declaration {
    if ("fn" in node) return this.visitFnDeclaration(node);
    if ("enum" in node) return this.visitEnumDeclaration(node);
    if ("import_dec" in node) return this.visitImportDeclaration(node);
    if ("type_dec" in node) return this.visitTypeDeclaration(node);
    if ("let_dec" in node) return this.visitLetDeclaration(node);
    if ("trait" in node) return this.visitTraitDeclaration(node);
    if ("impl" in node) return this.visitImplDeclaration(node);
    if ("builtin" in node) return this.visitBuiltinDeclaration(node);
    throw new Error("Unknown declaration type");
  }

  visitBuiltinDeclaration(node: BuiltinDeclaration): Declaration {
    return {
      builtin: {
        name: this.visitString(node.builtin.name),
        alias: this.visitNameIdentifier(node.builtin.alias),
        type_params: node.builtin.type_params.map((t) =>
          this.visitNameIdentifier(t),
        ),
        params: node.builtin.params.map((t) => this.visitFnParam(t)),
        return_type: this.visitTypeExpression(node.builtin.return_type),
      },
      position: node.position,
    };
  }

  visitFnDeclaration(node: FnDeclaration): Declaration {
    return {
      fn: {
        pub: node.fn.pub,
        fn: this.visitFn(node.fn.fn),
      },
      position: node.position,
    };
  }

  visitEnumDeclaration(node: EnumDeclaration): Declaration {
    return {
      enum: {
        pub: node.enum.pub,
        id: this.visitTypeIdentifier(node.enum.id),
        recursive: node.enum.recursive,
        type_params: node.enum.type_params.map((v) =>
          this.visitNameIdentifier(v),
        ),
        variants: node.enum.variants.map((v) => this.visitEnumVariant(v)),
      },
      position: node.position,
    };
  }

  visitImportDeclaration(node: ImportDeclaration): Declaration {
    return {
      import_dec: {
        import_from: node.import_dec.import_from,
        imports: node.import_dec.imports.map((i) => this.visitImport(i)),
      },
      position: node.position,
    };
  }

  visitTypeDeclaration(node: TypeDeclaration): Declaration {
    return {
      type_dec: {
        pub: node.type_dec.pub,
        id: this.visitTypeIdentifier(node.type_dec.id),
        params: node.type_dec.params.map((t) => this.visitNameIdentifier(t)),
        value: this.visitTypeExpression(node.type_dec.value),
      },
      position: node.position,
    };
  }

  visitLetDeclaration(node: LetDeclaration): Declaration {
    return {
      let_dec: {
        assert: node.let_dec.assert,
        pub: node.let_dec.pub,
        pattern: this.visitPatternExpression(node.let_dec.pattern),
        value: this.visitExpression(node.let_dec.value),
      },
      position: node.position,
    };
  }

  visitTraitDeclaration(node: TraitDeclaration): Declaration {
    return {
      trait: {
        pub: node.trait.pub,
        id: this.visitTypeIdentifier(node.trait.id),
        type_params: node.trait.type_params.map((t) =>
          this.visitNameIdentifier(t),
        ),
        fns: node.trait.fns.map((f) => this.visitTraitFn(f)),
      },
      position: node.position,
    };
  }

  visitImplDeclaration(node: ImplDeclaration): Declaration {
    return {
      impl: {
        type_params: node.impl.type_params.map((t) =>
          this.visitNameIdentifier(t),
        ),
        name: this.visitTypeIdentifier(node.impl.name),
        trait_params: node.impl.trait_params.map((t) =>
          this.visitTypeExpression(t),
        ),
        for: this.visitTypeExpression(node.impl.for),
        fns: node.impl.fns.map((f) => this.visitFn(f)),
      },
      position: node.position,
    };
  }

  visitTypeExpression(node: TypeExpression): TypeExpression {
    if ("select" in node) return this.visitSelectTypeExpression(node);
    if ("app" in node) return this.visitApplicationTypeExpression(node);
    if ("fn" in node) return this.visitFnTypeExpression(node);
    if ("record" in node) return this.visitRecordTypeExpression(node);
    if ("tuple" in node) return this.visitTupleTypeExpression(node);
    if ("name" in node) return this.visitNameIdentifier(node);
    if ("type" in node) return this.visitTypeIdentifier(node);
    throw new Error("Invalid type expression");
  }

  visitSelectTypeExpression(node: SelectTypeExpression): TypeExpression {
    return {
      select: {
        root: this.visitTypeExpression(node.select.root),
        name:
          "name" in node.select.name
            ? this.visitNameIdentifier(node.select.name)
            : this.visitTypeIdentifier(node.select.name),
      },
      position: node.position,
    };
  }

  visitApplicationTypeExpression(
    node: ApplicationTypeExpression,
  ): TypeExpression {
    return {
      app: {
        callee: this.visitTypeExpression(node.app.callee),
        args: node.app.args.map((a) => this.visitTypeExpression(a)),
      },
      position: node.position,
    };
  }

  visitFnTypeExpression(node: FnTypeExpression): TypeExpression {
    return {
      fn: {
        args: node.fn.args.map((a) => this.visitTypeExpression(a)),
        ret: this.visitTypeExpression(node.fn.ret),
      },
      position: node.position,
    };
  }

  visitRecordTypeExpression(node: RecordTypeExpression): TypeExpression {
    return {
      record: node.record.map((f) => this.visitNamedTypeExpression(f)),
      position: node.position,
    };
  }

  visitTupleTypeExpression(node: TupleTypeExpression): TypeExpression {
    return {
      tuple: node.tuple.map((t) => this.visitTypeExpression(t)),
      position: node.position,
    };
  }

  visitNamedTypeExpression(node: NamedTypeExpression): NamedTypeExpression {
    return {
      name: this.visitNameIdentifier(node.name),
      ty: this.visitTypeExpression(node.ty),
    };
  }

  visitBodyExpression(node: BodyExpression): BodyExpression {
    if ("arrow_bind" in node) return this.visitArrowKindBodyExpression(node);
    if ("let_bind" in node) return this.visitLetBindBodyExpression(node);
    if ("assign" in node) return this.visitAssignBodyExpression(node);
    if ("expression" in node) return this.visitExpressionBodyExpression(node);
    throw new Error("Unknown body expression type");
  }

  visitArrowKindBodyExpression(node: ArrowKindBodyExpression): BodyExpression {
    return {
      arrow_bind: {
        name: this.visitNameIdentifier(node.arrow_bind.name),
        expression: this.visitExpression(node.arrow_bind.expression),
      },
      position: node.position,
    };
  }

  visitLetBindBodyExpression(node: LetBindBodyExpression): BodyExpression {
    return {
      let_bind: {
        pattern: this.visitPatternExpression(node.let_bind.pattern),
        assert: node.let_bind.assert,
        expression: this.visitExpression(node.let_bind.expression),
      },
      position: node.position,
    };
  }

  visitAssignBodyExpression(node: AssignBodyExpression): BodyExpression {
    return {
      assign: {
        name: this.visitNameIdentifier(node.assign.name),
        expression: this.visitExpression(node.assign.expression),
      },
      position: node.position,
    };
  }

  visitExpressionBodyExpression(
    node: ExpressionBodyExpression,
  ): BodyExpression {
    return {
      expression: this.visitExpression(node.expression),
      position: node.position,
    };
  }

  visitExpression(node: Expression): Expression {
    if ("call" in node) return this.visitCallExpression(node);
    if ("block" in node) return this.visitBlockExpression(node);
    if ("if_expr" in node) return this.visitIfExpression(node);
    if ("select" in node) return this.visitSelectExpression(node);
    if ("match" in node) return this.visitMatchExpression(node);
    if ("float" in node) return this.visitFloatExpression(node);
    if ("int" in node) return this.visitIntExpression(node);
    if ("bool" in node) return this.visitBoolExpression(node);
    if ("prefix" in node) return this.visitPrefixExpression(node);
    if ("postfix" in node) return this.visitPostfixExpression(node);
    if ("infix" in node) return this.visitInfixExpression(node);
    if ("string" in node) return this.visitStringExpression(node);
    if ("fn" in node) return this.visitFnExpression(node);
    if ("record" in node) return this.visitRecordExpression(node);
    if ("tuple" in node) return this.visitTupleExpression(node);
    if ("self" in node) return this.visitSelfExpression(node);
    if ("name" in node) return this.visitNameIdentifier(node);
    if ("type" in node) return this.visitTypeIdentifier(node);
    throw new Error("Invalid expression kind", { cause: node });
  }

  visitBoolExpression(node: BoolExpression): Expression {
    return node;
  }

  visitIntExpression(node: IntExpression): Expression {
    return node;
  }

  visitFloatExpression(node: FloatExpression): Expression {
    return node;
  }

  visitStringExpression(node: StringToken): Expression {
    return this.visitString(node);
  }

  visitCallExpression(node: CallExpression): Expression {
    return {
      call: [
        this.visitExpression(node.call[0]),
        node.call[1].map((e) => this.visitExpression(e)),
      ],
      position: node.position,
    };
  }

  visitBlockExpression(node: BlockExpression): Expression {
    return {
      block: node.block.map((b) => this.visitBodyExpression(b)),
      position: node.position,
    };
  }

  visitIfExpression(node: IfExpression): Expression {
    return {
      if_expr: {
        cond: this.visitExpression(node.if_expr.cond),
        if_body: this.visitExpression(node.if_expr.if_body),
        else_body:
          node.if_expr.else_body &&
          this.visitExpression(node.if_expr.else_body),
      },
      position: node.position,
    };
  }

  visitSelectExpression(node: SelectExpression): Expression {
    return {
      select: [this.visitExpression(node.select[0]), node.select[1]],
      position: node.position,
    };
  }

  visitMatchExpression(node: MatchExpression): Expression {
    return {
      match: [
        this.visitExpression(node.match[0]),
        node.match[1].map((a) => this.visitMatchArm(a)),
      ],
      position: node.position,
    };
  }

  visitPrefixExpression(node: PrefixExpression): Expression {
    return {
      prefix: {
        op: node.prefix.op,
        operand: this.visitExpression(node.prefix.operand),
      },
      position: node.position,
    };
  }

  visitPostfixExpression(node: PostfixExpression): Expression {
    return {
      postfix: {
        op: node.postfix.op,
        operand: this.visitExpression(node.postfix.operand),
      },
      position: node.position,
    };
  }

  visitInfixExpression(node: InfixExpression): Expression {
    return {
      infix: {
        op: node.infix.op,
        left: this.visitExpression(node.infix.left),
        right: this.visitExpression(node.infix.right),
      },
      position: node.position,
    };
  }

  visitFnExpression(node: FnExpression): Expression {
    return {
      fn: this.visitFn(node.fn),
      position: node.position,
    };
  }

  visitRecordExpression(node: RecordExpression): Expression {
    return {
      record: node.record.map(
        ([name, expr]) =>
          [name, this.visitExpression(expr)] as [NameIdentifier, Expression],
      ),
      position: node.position,
    };
  }

  visitTupleExpression(node: TupleExpression): Expression {
    return {
      tuple: node.tuple.map((e) => this.visitExpression(e)),
      position: node.position,
    };
  }

  visitSelfExpression(node: SelfExpression): Expression {
    return node;
  }

  visitPatternExpression(node: PatternExpression): PatternExpression {
    if ("constr" in node) return this.visitConstructorPatternExpression(node);
    if ("int" in node) return this.visitIntPatternExpression(node);
    if ("float" in node) return this.visitFloatPatternExpression(node);
    if ("string" in node) return this.visitStringPatternExpression(node);
    if ("record" in node) return this.visitRecordPatternExpression(node);
    if ("tuple" in node) return this.visitTuplePatternExpression(node);
    if ("name" in node) return this.visitNameIdentifier(node);
    return node; // NameIdentifier
  }

  visitStringPatternExpression(
    node: StringPatternExpression,
  ): PatternExpression {
    return node;
  }

  visitFloatPatternExpression(node: FloatPatternExpression): PatternExpression {
    return node;
  }

  visitIntPatternExpression(node: IntPatternExpression): PatternExpression {
    return node;
  }

  visitConstructorPatternExpression(
    node: ConstructorPatternExpression,
  ): PatternExpression {
    return {
      constr: {
        type: this.visitTypeIdentifier(node.constr.type),
        patterns: node.constr.patterns.map((p) =>
          this.visitPatternExpression(p),
        ),
      },
      position: node.position,
    };
  }

  visitRecordPatternExpression(
    node: RecordPatternExpression,
  ): PatternExpression {
    return {
      record: node.record.map(
        ([name, pattern]) =>
          [name, this.visitPatternExpression(pattern)] as [
            NameIdentifier,
            PatternExpression,
          ],
      ),
      position: node.position,
    };
  }

  visitTuplePatternExpression(node: TuplePatternExpression): PatternExpression {
    return {
      tuple: node.tuple.map((p) => this.visitPatternExpression(p)),
      position: node.position,
    };
  }

  visitMatchArm(node: MatchArm): MatchArm {
    return {
      pattern: this.visitPatternExpression(node.pattern),
      guard: node.guard && this.visitExpression(node.guard),
      body: this.visitExpression(node.body),
    };
  }

  visitEnumVariant(node: EnumVariant): EnumVariant {
    if ("fields" in node) {
      return {
        fields: {
          id: this.visitTypeIdentifier(node.fields.id),
          fields: node.fields.fields.map((f) =>
            this.visitNamedTypeExpression(f),
          ),
        },
        position: node.position,
      };
    }
    return {
      values: {
        id: this.visitTypeIdentifier(node.values.id),
        values: node.values.values.map((v) => this.visitTypeExpression(v)),
      },
      position: node.position,
    };
  }

  visitImport(node: Import): Import {
    if ("type" in node) return this.visitTypeImport(node);
    if ("fn" in node) return this.visitFnImport(node);
    if ("global" in node) return this.visitGlobalImport(node);
    if ("table" in node) return this.visitTableImport(node);
    if ("star" in node) return this.visitStarImport(node);
    if ("memory" in node) return this.visitMemoryImport(node);
    if ("name" in node) return this.visitNameImport(node);
    if ("trait" in node) return this.visitTraitImport(node);
    if ("constr" in node) return this.visitConstructorImport(node);
    return node;
  }

  visitConstructorImport(node: ConstructorImport): Import {
    return {
      constr: {
        alias: node.constr.alias && this.visitTypeIdentifier(node.constr.alias),
        name: this.visitTypeIdentifier(node.constr.name),
      },
      position: node.position,
    };
  }

  visitTypeImport(node: TypeImport): Import {
    return {
      type: {
        alias: node.type.alias && this.visitTypeIdentifier(node.type.alias),
        name: this.visitTypeIdentifier(node.type.name),
      },
      position: node.position,
    };
  }

  visitFnImport(node: FnImport): Import {
    return {
      fn: {
        alias: node.fn.alias && this.visitNameIdentifier(node.fn.alias),
        name:
          "string" in node.fn.name
            ? this.visitString(node.fn.name)
            : this.visitNameIdentifier(node.fn.name),
        signature: this.visitFnSignature(node.fn.signature),
      },
      position: node.position,
    };
  }
  visitGlobalImport(node: GlobalImport): Import {
    return {
      global: {
        alias: node.global.alias && this.visitNameIdentifier(node.global.alias),
        global_type: this.visitTypeExpression(node.global.global_type),
        mut: node.global.mut,
        name:
          "string" in node.global.name
            ? this.visitString(node.global.name)
            : this.visitNameIdentifier(node.global.name),
      },
      position: node.position,
    };
  }
  visitTableImport(node: TableImport): Import {
    return {
      table: {
        alias: node.table.alias && this.visitNameIdentifier(node.table.alias),
        min: node.table.min && this.visitInt(node.table.min),
        max: node.table.max && this.visitInt(node.table.max),
        name:
          "string" in node.table.name
            ? this.visitString(node.table.name)
            : this.visitNameIdentifier(node.table.name),
        table_type: this.visitTypeExpression(node.table.table_type),
      },
      position: node.position,
    };
  }
  visitStarImport(node: StarImport): Import {
    return {
      star: this.visitTypeIdentifier(node.star),
    };
  }
  visitMemoryImport(node: MemoryImport): Import {
    return {
      memory: {
        alias: node.memory.alias && this.visitNameIdentifier(node.memory.alias),
        min: node.memory.min && this.visitInt(node.memory.min),
        max: node.memory.max && this.visitInt(node.memory.max),
        name:
          "string" in node.memory.name
            ? this.visitString(node.memory.name)
            : this.visitNameIdentifier(node.memory.name),
      },
      position: node.position,
    };
  }
  visitNameImport(node: NameImport): Import {
    return {
      name: {
        name: this.visitNameIdentifier(node.name.name),
        alias: node.name.alias && this.visitNameIdentifier(node.name.alias),
      },
      position: node.position,
    };
  }
  visitTraitImport(node: TraitImport): Import {
    return {
      trait: {
        name: this.visitTypeIdentifier(node.trait.name),
        alias: node.trait.alias && this.visitTypeIdentifier(node.trait.alias),
      },
      position: node.position,
    };
  }

  visitFn(node: Fn): Fn {
    return {
      name: node.name,
      type_params: node.type_params,
      params: node.params.map((p) => this.visitFnParam(p)),
      return_type: node.return_type
        ? this.visitTypeExpression(node.return_type)
        : null,
      body: this.visitExpression(node.body),
      position: node.position,
    };
  }

  visitTraitFn(node: TraitFn): TraitFn {
    return {
      name: node.name,
      type_params: node.type_params.map((p) => this.visitNameIdentifier(p)),
      params: node.params.map((p) => this.visitNamedTypeExpression(p)),
      return_type: this.visitTypeExpression(node.return_type),
      position: node.position,
    };
  }

  visitFnParam(node: FnParam): FnParam {
    return {
      name: this.visitNameIdentifier(node.name),
      guard: node.guard ? this.visitTypeExpression(node.guard) : null,
    };
  }

  visitFnSignature(node: FnSignature): FnSignature {
    return {
      param_types: node.param_types.map((p) => this.visitFnParam(p)),
      return_type: this.visitTypeExpression(node.return_type),
    };
  }

  visitTypeIdentifier(node: TypeToken): TypeToken {
    return node;
  }

  visitNameIdentifier(node: NameToken): NameToken {
    return node;
  }

  visitString(node: StringToken): StringToken {
    return node;
  }
  visitInt(node: IntToken): IntToken {
    return node;
  }
}
