// ./src/passes/create_scopes.ts
import type { NameToken, TypeToken } from "../lexer.js";
import type {
  BlockExpression,
  BuiltinDeclaration,
  ConstructorImport,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnDeclaration,
  FnImport,
  FnParam,
  GlobalImport,
  Import,
  LetDeclaration,
  MatchArm,
  MemoryImport,
  Module,
  NameIdentifier,
  PatternExpression,
  StarImport,
  TableImport,
  TraitDeclaration,
  TraitFn,
  TypeDeclaration,
  TypeExpression,
  TypeIdentifier,
  TypeImport,
} from "../parser.js";
import type { Builtin } from "../util.js";
import { BaseVisitor } from "../visitor.js";

export type Scope = {
  id: number;
  node: Scopable | null;
  parent: Scope | null;
  children: Scope[];
  term_elements: Map<string, ScopeElement>;
  type_elements: Map<string, ScopeElement>;
};

export function lookup_type(name: string, scope: Scope) {
  let current: Scope | null = scope;
  while (current) {
    const value = current.type_elements.get(name);
    if (value) return value;
    current = current.parent;
  }
  return null;
}

export function lookup_term(name: string, scope: Scope) {
  let current: Scope | null = scope;
  while (current) {
    const value = current.term_elements.get(name);
    if (value) return value;
    current = current.parent;
  }
  return null;
}

export type ScopeElement =
  | { builtin: BuiltinDeclaration }
  | { enum: EnumDeclaration }
  | { fn_import: FnImport }
  | { fn_param: FnParam }
  | { fn: Fn }
  | { global_import: GlobalImport }
  | { memory_import: MemoryImport }
  | { name_pattern: NameIdentifier }
  | { star_import: StarImport }
  | { table_import: TableImport }
  | { trait_fn: TraitFn }
  | { trait_self: TraitDeclaration }
  | { trait: TraitDeclaration }
  | { type_decl: TypeDeclaration }
  | { type_import: TypeImport }
  | { type_param: NameIdentifier }
  | { variant: EnumVariant }
  | { let_decl: LetDeclaration };

export type ScopeError =
  | { unnamed_fn_decl: Fn }
  | { import_has_no_name: Import }
  | { duplicate_definition: { name: NameToken | TypeToken; scope: Scope } };

export type Scopable =
  | Declaration
  | Expression
  | Fn
  | MatchArm
  | MatchArm
  | Module
  | PatternExpression
  | TypeExpression;

export type ScopeIndex = Map<Scopable, Scope>;

export class CreateScopes extends BaseVisitor {
  id = 0;
  current: Scope = {
    children: [],
    term_elements: new Map(),
    type_elements: new Map(),
    id: this.id++,
    node: null,
    parent: null,
  };
  mod_path = "";
  scopes = new Map<Scopable, Scope>();
  module_scopes = new Map<string, Scope>();
  errors = [] as ScopeError[];
  variants = new Map<EnumVariant, EnumDeclaration>();

  constructor(public builtins: Map<string, Builtin>) {
    super();
  }

  useGlobalsFrom(mod: Module) {
    const global_scope = this.scopes.get(mod);
    if (!global_scope)
      throw new Error("Cannot find global scope from given module!");
    this.current = global_scope;
  }

  scopifyModule(mod_path: string, mod: Module) {
    this.mod_path = mod_path;
    this.visitModule(mod);
  }

  override visitModule(node: Module) {
    this.enter(node);
    this.module_scopes.set(this.mod_path, this.current);
    super.visitModule(node);
    this.exit();
    return node;
  }

  override visitExpression(node: Expression): Expression {
    this.scopes.set(node, this.current);
    return super.visitExpression(node);
  }

  override visitTypeExpression(node: TypeExpression): TypeExpression {
    this.scopes.set(node, this.current);
    return super.visitTypeExpression(node);
  }

  override visitPatternExpression(node: PatternExpression): PatternExpression {
    this.scopes.set(node, this.current);
    return super.visitPatternExpression(node);
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    // define the enum in the module's namespace
    this.define_type(node.enum.id, { enum: node });

    for (const variant of node.enum.variants) {
      this.define_type(
        "fields" in variant ? variant.fields.id : variant.values.id,
        {
          variant,
        },
      );
      // define it's parent
      this.variants.set(variant, node);
    }

    this.enter(node);
    this.scopes.set(node, this.current);
    for (const type_param of node.enum.type_params) {
      // TODO: Add trait constraints
      this.define_type(type_param, { type_param });
    }
    super.visitEnumDeclaration(node);
    this.exit();
    return node;
  }

  override visitEnumVariant(node: EnumVariant): EnumVariant {
    const name = "fields" in node ? node.fields.id : node.values.id;
    this.define_type(name, { variant: node });
    super.visitEnumVariant(node);
    return node;
  }

  override visitFnDeclaration(node: FnDeclaration): Declaration {
    if (!node.fn.fn.name) {
      this.errors.push({ unnamed_fn_decl: node.fn.fn });
      return node;
    }
    this.define_term(node.fn.fn.name, { fn: node.fn.fn });
    this.enter(node.fn.fn);
    this.scopes.set(node.fn.fn, this.current);

    // TODO: type parameters should have "names"
    for (const type_param of node.fn.fn.type_params) {
      this.define_type(type_param, { type_param });
    }

    for (const fn_param of node.fn.fn.params) {
      this.define_term(fn_param.name, { fn_param });
    }

    super.visitFn(node.fn.fn);
    this.exit();
    return node;
  }

  override visitFn(node: Fn): Fn {
    this.enter(node);
    if (node.name) this.define_term(node.name, { fn: node });

    // TODO: type parameters should have "names"
    for (const type_param of node.type_params) {
      this.define_type(type_param, { type_param });
    }

    for (const fn_param of node.params) {
      this.define_term(fn_param.name, { fn_param });
    }

    super.visitFn(node);
    this.exit();
    return node;
  }

  override visitLetDeclaration(node: LetDeclaration): Declaration {
    this.define_term(node.let_dec.id, { let_decl: node });
    super.visitLetDeclaration(node);
    return node;
  }

  override visitConstructorImport(node: ConstructorImport): Import {
    this.scopes.set(node.constr.name, this.current);
    return node;
  }

  override visitTypeDeclaration(node: TypeDeclaration): Declaration {
    this.define_type(node.type_dec.id, { type_decl: node });
    this.enter(node);

    // TODO: type parameters should have "names"
    for (const type_param of node.type_dec.params) {
      this.define_type(type_param, { type_param });
    }

    super.visitTypeDeclaration(node);
    this.exit();
    return node;
  }

  override visitBlockExpression(node: BlockExpression): Expression {
    this.enter(node);
    super.visitBlockExpression(node);
    this.exit();
    return node;
  }

  override visitBuiltinDeclaration(node: BuiltinDeclaration): Declaration {
    const name = node.builtin.name;
    if (!this.builtins.has(name.string)) {
      throw new Error(`Cannot find builtin: ${name.string}`);
    }
    this.define_term(node.builtin.alias, { builtin: node });
    super.visitBuiltinDeclaration(node);
    return node;
  }

  override visitFnImport(node: FnImport): Import {
    if (node.fn.alias || "name" in node.fn.name) {
      this.define_term(node.fn.alias ?? (node.fn.name as NameIdentifier), {
        fn_import: node,
      });
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }

  override visitGlobalImport(node: GlobalImport): Import {
    if (node.global.alias || "name" in node.global.name) {
      this.define_term(
        node.global.alias ?? (node.global.name as NameIdentifier),
        {
          global_import: node,
        },
      );
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }

  override visitMatchArm(node: MatchArm): MatchArm {
    this.enter(node);
    super.visitMatchArm(node);
    this.exit();
    return node;
  }

  override visitNamePatternExpression(node: NameIdentifier): PatternExpression {
    this.define_term(node, { name_pattern: node });
    return node;
  }

  override visitMemoryImport(node: MemoryImport): Import {
    if (node.memory.alias || "name" in node.memory.name) {
      this.define_term(
        node.memory.alias ?? (node.memory.name as NameIdentifier),
        {
          memory_import: node,
        },
      );
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }

  override visitStarImport(node: StarImport): Import {
    this.define_type(node.star, { star_import: node });
    return node;
  }

  override visitTableImport(node: TableImport): Import {
    if (node.table.alias || "name" in node.table.name) {
      this.define_term(
        node.table.alias ?? (node.table.name as NameIdentifier),
        {
          table_import: node,
        },
      );
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }
  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    this.define_term({ name: "self" }, { trait_self: node });
    this.define_type(node.trait.id, { trait: node });

    for (const fn of node.trait.fns) {
      this.define_term(fn.name, { trait_fn: fn });
    }

    this.enter(node);

    for (const type_param of node.trait.type_params) {
      this.define_type(type_param, { type_param });
    }
    super.visitTraitDeclaration(node);

    this.exit();
    return node;
  }

  override visitTypeIdentifier(node: TypeToken): TypeToken {
    this.scopes.set(node, this.current);
    return node;
  }

  override visitTypeImport(node: TypeImport): Import {
    this.define_type(node.type.name, { type_import: node });
    return node;
  }

  define_term(name: NameIdentifier, element: ScopeElement) {
    if (this.current.term_elements.has(name.name)) {
      this.errors.push({ duplicate_definition: { name, scope: this.current } });
    }
    this.current.term_elements.set(name.name, element);
    this.scopes.set(name, this.current);
  }

  define_type(name: NameIdentifier | TypeIdentifier, element: ScopeElement) {
    const elem_id = "type" in name ? name.type : name.name;
    if (this.current.type_elements.has(elem_id)) {
      this.errors.push({ duplicate_definition: { name, scope: this.current } });
    }
    this.current.type_elements.set(elem_id, element);
    this.scopes.set(name, this.current);
  }

  getScopeIndex(): ScopeIndex {
    return this.scopes;
  }

  enter(node: Scopable) {
    const next = {
      children: [],
      term_elements: new Map(),
      type_elements: new Map(),
      id: this.id++,
      node,
      parent: this.current,
    } satisfies Scope;
    this.current.children.push(next);
    this.current = next;
    this.scopes.set(node, next);
  }

  exit() {
    const next = this.current.parent;
    if (!next) throw new Error("Something happened");
    this.current = next;
  }
}
