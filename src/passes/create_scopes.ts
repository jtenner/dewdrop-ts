import type { NameToken, TypeToken } from "../lexer.js";
import type {
  BlockExpression,
  BuiltinImport,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnDeclaration,
  FnImport,
  FnParam,
  GlobalImport,
  Identifier,
  Import,
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
  TypeIdentifier,
  TypeImport,
} from "../parser.js";
import type { Builtin } from "../util.js";
import { BaseVisitor } from "../visitor.js";

type ScopedNodeKind =
  | { fn: Fn }
  | { decl: Declaration }
  | { match_arm: MatchArm }
  | { block: BlockExpression }
  | { module: Module };

export type Scope = {
  id: number;
  node: ScopedNodeKind | null;
  parent: Scope | null;
  children: Scope[];
  elements: Map<NameIdentifier | TypeIdentifier, ScopeElement>;
};

export type ScopeElement =
  | { type_decl: TypeDeclaration }
  | { type_param: NameIdentifier }
  | { enum: EnumDeclaration }
  | { fn: Fn }
  | { fn_param: FnParam }
  | { variant: EnumVariant }
  | { builtin: BuiltinImport }
  | { fn_import: FnImport }
  | { global_import: GlobalImport }
  | { name_pattern: NameIdentifier }
  | { memory_import: MemoryImport }
  | { star_import: StarImport }
  | { table_import: TableImport }
  | { trait: TraitDeclaration }
  | { trait_fn: TraitFn }
  | { type_import: TypeImport }
  | { builtin: Builtin };

export type ScopeError =
  | { unnamed_fn_decl: Fn }
  | { import_has_no_name: Import };

export class CreateScopes extends BaseVisitor {
  id = 0;
  current: Scope = {
    children: [],
    elements: new Map(),
    id: this.id++,
    node: null,
    parent: null,
  };
  mod_path = "";
  scopes = new Map<NameIdentifier | Identifier, Scope>();
  module_scopes = new Map<string, Scope>();
  errors = [] as ScopeError[];

  override visitModule(node: Module) {
    this.enter({ module: node });
    this.module_scopes.set(this.mod_path, this.current);
    super.visitModule(node);
    this.exit();
    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    // define the enum in the module's namespace
    this.define(node.enum.id, { enum: node });

    for (const variant of node.enum.variants) {
      this.define("fields" in variant ? variant.fields.id : variant.values.id, {
        variant,
      });
    }

    this.enter({ decl: node });
    for (const type_param of node.enum.type_params) {
      // TODO: Add trait constraints
      this.define(type_param, { type_param });
    }
    super.visitEnumDeclaration(node);
    this.exit();
    return node;
  }

  override visitFnDeclaration(node: FnDeclaration): Declaration {
    if (!node.fn.fn.name) {
      this.errors.push({ unnamed_fn_decl: node.fn.fn });
      return node;
    }
    this.define(node.fn.fn.name, { fn: node.fn.fn });
    this.enter({ fn: node.fn.fn });

    // TODO: type parameters should have "names"
    for (const type_param of node.fn.fn.type_params) {
      this.define(type_param, { type_param });
    }

    for (const fn_param of node.fn.fn.params) {
      this.define(fn_param.name, { fn_param });
    }

    super.visitFn(node.fn.fn);
    this.exit();
    return node;
  }

  override visitFn(node: Fn): Fn {
    this.enter({ fn: node });
    if (node.name) this.define(node.name, { fn: node });

    // TODO: type parameters should have "names"
    for (const type_param of node.type_params) {
      this.define(type_param, { type_param });
    }

    for (const fn_param of node.params) {
      this.define(fn_param.name, { fn_param });
    }

    super.visitFn(node);
    this.exit();
    return node;
  }

  override visitTypeDeclaration(node: TypeDeclaration): Declaration {
    this.define(node.type_dec.id, { type_decl: node });
    this.enter({ decl: node });

    // TODO: type parameters should have "names"
    for (const type_param of node.type_dec.params) {
      this.define(type_param, { type_param });
    }

    super.visitTypeDeclaration(node);
    this.exit();
    return node;
  }

  override visitBlockExpression(node: BlockExpression): Expression {
    this.enter({ block: node });
    super.visitBlockExpression(node);
    this.exit();
    return node;
  }

  override visitBuiltinImport(node: BuiltinImport): Import {
    this.define(node.builtin.alias, { builtin: node });
    return node;
  }

  override visitFnImport(node: FnImport): Import {
    if (node.fn.alias || "name" in node.fn.name) {
      this.define(node.fn.alias ?? (node.fn.name as NameIdentifier), {
        fn_import: node,
      });
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }

  override visitGlobalImport(node: GlobalImport): Import {
    if (node.global.alias || "name" in node.global.name) {
      this.define(node.global.alias ?? (node.global.name as NameIdentifier), {
        global_import: node,
      });
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }

  override visitMatchArm(node: MatchArm): MatchArm {
    this.enter({ match_arm: node });
    super.visitMatchArm(node);
    this.exit();
    return node;
  }

  override visitNamePatternExpression(node: NameIdentifier): PatternExpression {
    this.define(node, { name_pattern: node });
    return node;
  }

  override visitMemoryImport(node: MemoryImport): Import {
    if (node.memory.alias || "name" in node.memory.name) {
      this.define(node.memory.alias ?? (node.memory.name as NameIdentifier), {
        memory_import: node,
      });
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }

  override visitStarImport(node: StarImport): Import {
    this.define(node.star, { star_import: node });
    return node;
  }

  override visitTableImport(node: TableImport): Import {
    if (node.table.alias || "name" in node.table.name) {
      this.define(node.table.alias ?? (node.table.name as NameIdentifier), {
        table_import: node,
      });
      return node;
    }

    this.errors.push({ import_has_no_name: node });
    return node;
  }
  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    this.define(node.trait.id, { trait: node });

    for (const fn of node.trait.fns) {
      this.define(fn.name, { trait_fn: fn });
    }

    this.enter({ decl: node });

    for (const type_param of node.trait.type_params) {
      this.define(type_param, { type_param });
    }
    super.visitTraitDeclaration(node);

    this.exit();
    return node;
  }

  override visitTypeImport(node: TypeImport): Import {
    this.define(node.type.name, { type_import: node });
    return node;
  }

  define(name: NameIdentifier | TypeIdentifier, element: ScopeElement) {
    this.current.elements.set(name, element);
  }

  override visitNameIdentifier(node: NameToken): NameToken {
    this.scopes.set(node, this.current);
    return node;
  }

  override visitTypeIdentifier(node: TypeToken): TypeToken {
    this.scopes.set(node, this.current);
    return node;
  }

  enter(node: ScopedNodeKind) {
    const next = {
      children: [],
      elements: new Map(),
      id: this.id++,
      node,
      parent: this.current,
    };
    this.current.children.push(next);
    this.current = next;
  }

  exit() {
    const next = this.current.parent;
    if (!next) throw new Error("Something happened");
    this.current = next;
  }
}
