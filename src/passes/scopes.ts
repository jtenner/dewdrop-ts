import type { TypeToken } from "../lexer.js";
import type {
  BuiltinDeclaration,
  ConstructorImport,
  EnumDeclaration,
  EnumImport,
  Expression,
  Fn,
  FnDeclaration,
  FnImport,
  GlobalImport,
  ImplDeclaration,
  Import,
  MatchArm,
  MemoryImport,
  Module,
  NameIdentifier,
  NameImport,
  PatternExpression,
  SelfExpression,
  StarImport,
  TableImport,
  TraitDeclaration,
  TraitFn,
  TraitImport,
  TypeDeclaration,
  TypeExpression,
  TypeImport,
} from "../parser.js";
import type { Scope, ScopeElement } from "../util.js";
import { BaseWalker } from "../visitor.js";

const getTypeUniqueTypeVars = (
  ty: TypeExpression,
  seen = new Set<string>(),
) => {
  if ("app" in ty) {
    for (const arg of ty.app.args) {
      getTypeUniqueTypeVars(arg, seen);
    }
  } else if ("fn" in ty) {
    for (const arg of ty.fn.args) {
      getTypeUniqueTypeVars(arg, seen);
    }
    getTypeUniqueTypeVars(ty.fn.ret, seen);
  } else if ("record" in ty) {
    for (const { ty: ft } of ty.record) {
      getTypeUniqueTypeVars(ft, seen);
    }
  } else if ("select" in ty) {
    getTypeUniqueTypeVars(ty.select.root);
  } else if ("tuple" in ty) {
    for (const t of ty.tuple) {
      getTypeUniqueTypeVars(t, seen);
    }
  } else if ("name" in ty) {
    seen.add(ty.name);
  }
  return seen;
};

export class ScopesPass extends BaseWalker {
  current = this.context.globalScope;

  override walkModule(node: Module): void {
    if (node === this.context.globalModule.module) {
      this.setScope(node, this.context.globalScope);
      super.walkModule(node);
    } else {
      this.current = this.context.globalScope;
      this.push();
      this.setScope(node, this.current);
      super.walkModule(node);
      this.pop();
    }
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
    let next = this.current as Scope | null;

    while (next) {
      if (!next.terms.has(term)) next = next.parent;
      else return;
    }

    this.errors.push({ unbound: { name: term, scope: this.current } });
  }

  checkType(type: string) {
    let next = this.current as Scope | null;

    while (next) {
      if (next.types.has(type)) return;
      next = next.parent;
    }

    this.errors.push({ unbound: { name: type, scope: this.current } });
  }

  override walkBuiltinDeclaration(node: BuiltinDeclaration): void {
    this.defineTerm(node.builtin.alias.name, node);
  }

  override walkImport(node: Import) {
    this.setScope(node, this.current);
    super.walkImport(node);
  }

  override walkMatchArm(node: MatchArm): void {
    this.push();
    super.walkMatchArm(node);
    this.pop();
  }

  override walkFnImport(node: FnImport): void {
    const name =
      "string" in node.fn.name ? node.fn.name.string : node.fn.name.name;
    const alias = node.fn.alias?.name ?? name;
    this.defineTerm(alias, { unresolved: node });
  }

  override walkEnumImport(node: EnumImport): void {
    const name = node.enum.name.type;
    const alias = node.enum.alias?.type ?? name;
    this.defineType(alias, { unresolved: node });
  }

  override walkNameImport(node: NameImport): void {
    const name = node.name.name.name;
    const alias = node.name.alias?.name ?? name;
    this.defineTerm(alias, { unresolved: node });
  }

  override walkConstructorImport(node: ConstructorImport): void {
    const name = node.constr.name.type;
    const alias = node.constr.alias?.type ?? name;
    this.defineTerm(alias, { unresolved: node });
  }

  override walkStarImport(node: StarImport): void {
    const name = node.star.type;
    this.defineTerm(name, { unresolved: node });
  }

  override walkTypeImport(node: TypeImport): void {
    const name = node.type.name.type;
    const alias = node.type.alias?.type ?? name;
    this.defineType(alias, { unresolved: node });
  }

  override walkTableImport(node: TableImport): void {
    const name =
      "string" in node.table.name
        ? node.table.name.string
        : node.table.name.name;
    const alias =
      "string" in node.table.name
        ? node.table.alias?.name
        : (node.table.alias?.name ?? name);

    if (!alias) this.errors.push({ invalid_name: node });
    else this.defineTerm(alias, { unresolved: node });
  }

  override walkTraitFn(node: TraitFn): void {
    this.push();
    for (const tyvar of node.type_params) {
      this.defineType(tyvar.name, { typevar: tyvar });
    }
    super.walkTraitFn(node);
    this.pop();
  }

  override walkTraitImport(node: TraitImport): void {
    const name = node.trait.name.type;
    const alias = node.trait.alias?.type ?? name;
    this.defineType(alias, { unresolved: node });
  }

  override walkMemoryImport(node: MemoryImport): void {
    const name =
      "string" in node.memory.name
        ? node.memory.name.string
        : node.memory.name.name;
    const alias =
      "string" in node.memory.name
        ? node.memory.alias?.name
        : (node.memory.alias?.name ?? name);

    if (!alias) this.errors.push({ invalid_name: node });
    else this.defineTerm(alias, { unresolved: node });
  }

  override walkGlobalImport(node: GlobalImport): void {
    const name =
      "string" in node.global.name
        ? node.global.name.string
        : node.global.name.name;
    const alias =
      "string" in node.global.name
        ? node.global.alias?.name
        : (node.global.alias?.name ?? name);

    if (!alias) this.errors.push({ invalid_name: node });
    else this.defineTerm(alias, { unresolved: node });
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
    // every trait function and the trait itself is hoisted to the current scope
    this.defineType(node.trait.id.type, { trait: node });

    for (const tfn of node.trait.fns) {
      this.defineTerm(tfn.name.name, { trait_fn: tfn });
    }

    this.push();

    this.defineType("Self", { selftype: node.trait.id });
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

    const typeVars = getTypeUniqueTypeVars({
      tuple: [...node.impl.type_params, node.impl.for],
    });

    for (const tyvar of typeVars) {
      const id = { name: tyvar };
      this.setScope(id, this.current);
      this.defineType(tyvar, { typevar: id });
    }

    super.walkImplDeclaration(node);
    this.pop();
  }

  override walkSelfExpression(_node: SelfExpression): void {
    this.checkTerm("self");
    this.checkType("Self");
  }

  override walkNameIdentifier(node: NameIdentifier) {
    // Name identifiers in the type position check for type definitions
    // Name identifiers in the expression position check for term definitions
    // Name identifiers in the ambient and pattern position are declarations
    if (this.idMode === "type") this.checkType(node.name);
    else if (this.idMode === "expression") this.checkTerm(node.name);
    else if (this.idMode === "pattern")
      this.defineTerm(node.name, { var: node });
  }

  override walkTypeIdentifier(node: TypeToken): void {
    // Type identifiers in the type position are located in the type scope
    // Type identifiers in the expression or pattern position are constructor
    // terms.
    // Type identifiers in the ambient position are declarations.
    if (this.idMode === "type") this.checkType(node.type);
    else if (this.idMode === "expression" || this.idMode === "pattern")
      this.checkTerm(node.type);
  }
}
