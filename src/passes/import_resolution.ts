import type {
  ImportDeclaration,
  Declaration,
  Module,
  FnImport,
  Import,
  NameImport,
  TypeImport,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";
import type { Scope, ScopeIndex } from "./create_scopes.js";
import * as path from "node:path";

type ModuleIndex = Map<string, Module>;

export type ImportError =
  | { scope_not_found: string }
  | { module_not_found: string }
  | { element_not_found: { module: string; element: string } };
export class ImportResolution extends BaseVisitor {
  constructor(
    public module_index: ModuleIndex,
    public scope_index: ScopeIndex,
  ) {
    super();
  }
  errors = [] as ImportError[];
  imported_scope: Scope | null = null;
  current_module: string = "";
  current_scope: Scope | null = null;

  resolveImports(module: string) {
    this.current_module = module;

    const ast = this.module_index.get(module);
    if (!ast) {
      this.errors.push({ module_not_found: module });
      return;
    }

    const scope = this.scope_index.get(ast);
    if (!scope) {
      this.errors.push({ scope_not_found: module });
      return;
    }
    this.current_scope = scope;
    this.visitModule(ast);
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    const import_loc = node.import_dec.import_from;
    if (import_loc.startsWith("@std/")) throw new Error("Not implemeneted");

    const current_folder = path.dirname(this.current_module);
    let relative_file = path.join(current_folder, import_loc);
    if (!relative_file.toLowerCase().endsWith(".dew")) relative_file += ".dew";

    const ast = this.module_index.get(relative_file);
    if (!ast) {
      this.errors.push({ module_not_found: relative_file });
      return node;
    }

    const scope = this.scope_index.get(ast);
    if (!scope) {
      this.errors.push({ scope_not_found: relative_file });
      return node;
    }

    this.imported_scope = scope;
    super.visitImportDeclaration(node);
    return node;
  }

  override visitNameImport(node: NameImport): Import {
    const name = node.name.name.name;
    const alias = node.name.alias?.name ?? name;
    // name imports represent a "term"

    const element = this.imported_scope?.term_elements.get(name);
    if (!element) {
      this.errors.push({
        element_not_found: { module: this.current_module, element: name },
      });
      return node;
    }

    this.current_scope!.term_elements.set(alias, element);
    return node;
  }

  override visitTypeImport(node: TypeImport): Import {
    const name = node.type.name.type;
    const alias = node.type.alias?.type ?? name;

    // type imports should be an imported "type" like an enum
    const element = this.imported_scope?.type_elements.get(name);
    if (!element) {
      this.errors.push({
        element_not_found: { module: this.current_module, element: name },
      });
      return node;
    }

    this.current_scope!.type_elements.set(alias, element);
    return node;
  }
}
