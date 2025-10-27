import * as path from "node:path";

import type {
  BuiltinDeclaration,
  ConstructorImport,
  Declaration,
  FnImport,
  GlobalImport,
  Import,
  ImportDeclaration,
  MemoryImport,
  Module,
  NameIdentifier,
  NameImport,
  TableImport,
  TraitImport,
  TypeIdentifier,
  TypeImport,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";
import type { Scope, ScopeIndex } from "./create_scopes.js";

type ModuleIndex = Map<string, Module>;

export type ImportError =
  | { scope_not_found: string }
  | { module_not_found: string }
  | { element_not_found: { module: string; element: string } }
  | { misnamed_import: { imp: Import } }
  | { element_not_a_variant: { module: string; element: string } };
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

  resolveImports(module_path: string, ast: Module) {
    this.current_module = module_path;

    const scope = this.scope_index.get(ast);
    if (!scope) {
      this.errors.push({ scope_not_found: module_path });
      return;
    }
    this.current_scope = scope;
    this.visitModule(ast);
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    const import_loc = node.import_dec.import_from;

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

  override visitConstructorImport(node: ConstructorImport): Import {
    const name = node.constr.name.type;
    const alias = node.constr.alias?.type ?? name;
    // type imports should be an imported "type" like an enum
    const element = this.imported_scope?.type_elements.get(name);
    if (!element) {
      this.errors.push({
        element_not_found: { module: this.current_module, element: name },
      });
      return node;
    }

    if ("variant" in element) {
      this.current_scope!.type_elements.set(alias, element);
      return node;
    }

    this.errors.push({
      element_not_a_variant: { module: this.current_module, element: name },
    });
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

  override visitFnImport(fn_import: FnImport): Import {
    const { alias, name } = fn_import.fn;
    if (!alias && "string" in name) {
      this.errors.push({ misnamed_import: { imp: fn_import } });
      return fn_import;
    }
    const symbol = alias?.name ?? (name as NameIdentifier).name;
    this.current_scope!.term_elements.set(symbol, { fn_import });
    return fn_import;
  }

  override visitGlobalImport(global_import: GlobalImport): Import {
    const { alias, name } = global_import.global;
    if (!alias && "string" in name) {
      this.errors.push({ misnamed_import: { imp: global_import } });
      return global_import;
    }
    const symbol = alias?.name ?? (name as NameIdentifier).name;
    this.current_scope!.term_elements.set(symbol, { global_import });
    return global_import;
  }

  override visitTableImport(table_import: TableImport): Import {
    const { alias, name } = table_import.table;
    if (!alias && "string" in name) {
      this.errors.push({ misnamed_import: { imp: table_import } });
      return table_import;
    }
    const symbol = alias?.name ?? (name as NameIdentifier).name;
    this.current_scope!.term_elements.set(symbol, { table_import });
    return table_import;
  }

  override visitMemoryImport(memory_import: MemoryImport): Import {
    const { alias, name } = memory_import.memory;
    if (!alias && "string" in name) {
      this.errors.push({ misnamed_import: { imp: memory_import } });
      return memory_import;
    }
    const symbol = alias?.name ?? (name as NameIdentifier).name;
    this.current_scope!.term_elements.set(symbol, { memory_import });
    return memory_import;
  }

  override visitTraitImport(node: TraitImport): Import {
    return node;
  }
}
