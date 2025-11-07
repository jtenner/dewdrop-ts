import type { TypeToken } from "../lexer.js";
import type {
  EnumDeclaration,
  Expression,
  Fn,
  FnDeclaration,
  ImplDeclaration,
  Import,
  Module,
  NameIdentifier,
  PatternExpression,
  SelfExpression,
  TraitDeclaration,
  TypeDeclaration,
  TypeExpression,
} from "../parser.js";
import type { Scope, ScopeElement } from "../util.js";
import { BaseWalker } from "../visitor.js";

export class ScopesPass extends BaseWalker {
  globalScope: Scope = {
    parent: null,
    terms: new Map(),
    types: new Map(),
  };
  current = this.globalScope;

  scopify(module: Module) {
    this.push();
    this.setScope(module, this.current);
    super.walkModule(module);
    this.pop();
  }

  push() {
    this.current = {
      parent: this.current,
      terms: new Map(),
      types: new Map(),
    };
  }

  pop() {
    if (!this.current.parent)
      throw new Error("Cannot pop stack. Something went wrong.");
    this.current = this.current.parent;
  }

  defineTerm(name: string, term: ScopeElement) {
    this.current.terms.set(name, term);
  }

  defineType(name: string, type: ScopeElement) {
    this.current.types.set(name, type);
  }

  checkTerm(term: string) {
    if (!this.current.terms.has(term))
      this.errors.push({ unbound: { name: term, scope: this.current } });
  }

  checkType(type: string) {
    if (!this.current.types.has(type))
      this.errors.push({ unbound: { name: type, scope: this.current } });
  }

  override walkImport(node: Import) {
    this.setScope(node, this.current);
    super.walkImport(node);
  }

  override walkEnumDeclaration(node: EnumDeclaration): void {
    this.defineType(node.enum.id.type, { enum: node });
    // the variants are in the module scope, not the inner scope
    for (const variant of node.enum.variants)
      this.defineTerm(
        "fields" in variant ? variant.fields.id.type : variant.values.id.type,
        { variant },
      );
    this.push();
    for (const tyvar of node.enum.type_params)
      this.defineType(tyvar.name, { typevar: tyvar });
    super.walkEnumDeclaration(node);
    this.pop();
  }

  override walkTypeDeclaration(node: TypeDeclaration): void {
    this.defineType(node.type_dec.id.type, { type: node });
    this.push();
    for (const tyvar of node.type_dec.params)
      this.defineType(tyvar.name, { typevar: tyvar });
    super.walkTypeDeclaration(node);
    this.pop();
  }

  override walkTraitDeclaration(node: TraitDeclaration): void {
    this.defineType(node.trait.id.type, { trait: node });
    this.push();
    for (const tyvar of node.trait.type_params)
      this.defineType(tyvar.name, { typevar: tyvar });
    super.walkTraitDeclaration(node);
    this.pop();
  }

  override walkFnDeclaration(node: FnDeclaration): void {
    this.defineTerm(node.fn.fn.name!.name, { fn: node.fn.fn });
    super.walkFnDeclaration(node);
  }

  override walkExpression(node: Expression): void {
    this.setScope(node, this.current);
    super.walkExpression(node);
  }

  override walkTypeExpression(node: TypeExpression): void {
    this.setScope(node, this.current);
    super.walkTypeExpression(node);
  }

  override walkPatternExpression(node: PatternExpression): void {
    this.setScope(node, this.current);
    super.walkPatternExpression(node);
  }

  override walkFn(node: Fn): void {
    // enter the next context before defining the function's name in the func
    this.push();
    if (node.name) this.defineTerm(node.name.name, { fn: node });

    for (const tyvar of node.type_params)
      this.defineType(tyvar.name, { typevar: tyvar });

    for (const param of node.params)
      this.defineTerm(param.name.name, { fnparam: param });

    super.walkFn(node);
    this.pop();
  }

  override walkImplDeclaration(node: ImplDeclaration): void {
    this.push();
    this.defineTerm("self", { self: null });
    this.defineType("Self", { selftype: node.impl.for });
    super.walkImplDeclaration(node);
    this.pop();
  }

  override walkSelfExpression(_node: SelfExpression): void {
    this.checkTerm("self");
    this.checkType("Self");
  }

  override walkNameIdentifier(node: NameIdentifier) {
    // names in the pattern position are not checked
    if (this.idMode === "type") this.checkType(node.name);
    else if (this.idMode === "expression") this.checkTerm(node.name);
  }

  override walkTypeIdentifier(node: TypeToken): void {
    // Type identifiers in the type position are located in the type scope
    // Type identifiers in the expression or pattern position are constructor
    // terms.
    if (this.idMode === "type") this.checkType(node.type);
    else this.checkTerm(node.type);
  }
}
