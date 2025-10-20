// TODO: This was vibe coded, go over it.

// resolver.ts

import type { StringToken } from "../lexer.js";
import type {
  Module,
  Declaration,
  FnDeclaration,
  LetDeclaration,
  TypeDeclaration,
  TraitDeclaration,
  EnumDeclaration,
  ImportDeclaration,
  Import,
  FnImport,
  GlobalImport,
  TableImport,
  MemoryImport,
  NameImport,
  TypeImport,
  TraitImport,
  EnumImport,
  BuiltinImport,
  NameIdentifier,
  TypeIdentifier,
  Identifier,
  Fn,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";
import type { CreateScopes, Scope, ScopeElement } from "./create_scopes.js";

// Useful: turn tokens into names
const nameOf = (id: StringToken | NameIdentifier | TypeIdentifier): string => {
  if ("name" in id) return id.name;
  if ("type" in id) return id.type;
  if ("string" in id) return id.string;
  return "<unknown>";
};

// What a module publicly exports.
// Keep value-level and type-level separate to avoid collisions.
export type ExportTable = {
  // value-level
  values: Map<string, ScopeElement>;
  // type-level
  types: Map<string, ScopeElement>;
};

export type UnknownModuleError = {
  unknown_module: { from: string };
};
export type UnknownExportError = {
  unknown_export: { from: string; symbol: string; kind: string };
};
export type ImportConflictError = {
  import_conflict: { name: string; first_from: string; second_from: string };
};
export type BadAliasError = {
  bad_alias: { alias: string; reason: string };
};
export type ResolveImportError =
  | UnknownModuleError
  | UnknownExportError
  | ImportConflictError
  | BadAliasError;

// 1) Build public exports from a module's declarations.
// You can also use the module scope from CreateScopes, but walking
// declarations keeps it simple and respects the pub flags you already store.
export function buildExportTable(mod: Module): ExportTable {
  const values = new Map<string, ScopeElement>();
  const types = new Map<string, ScopeElement>();

  for (const decl of mod.module) {
    // Functions
    if ("fn" in decl && decl.fn.pub && decl.fn.fn.name) {
      values.set(nameOf(decl.fn.fn.name), { fn: decl.fn.fn as Fn });
      continue;
    }

    // Let bindings
    if ("let_dec" in decl && decl.let_dec.pub) {
      values.set(nameOf(decl.let_dec.id), { name_pattern: decl.let_dec.id });
      continue;
    }

    // Types
    if ("type_dec" in decl && decl.type_dec.pub) {
      types.set(nameOf(decl.type_dec.id), { type_decl: decl });
      continue;
    }

    // Traits
    if ("trait" in decl && decl.trait.pub) {
      types.set(nameOf(decl.trait.id), { trait: decl });
      // Optionally export trait methods as well if your language exposes them
      continue;
    }

    // Enums and variants (export the enum type; variants are value-level)
    if ("enum" in decl && decl.enum.pub) {
      types.set(nameOf(decl.enum.id), { enum: decl });
      for (const variant of decl.enum.variants) {
        const vName =
          "fields" in variant
            ? nameOf(variant.fields.id)
            : nameOf(variant.values.id);
        values.set(vName, { variant });
      }
      continue;
    }

    // You can support re-exports here if your language has them.
  }

  return { values, types };
}

// 2) Resolve imports: replace imported placeholders with concrete bindings.
export class ResolveImports extends BaseVisitor {
  errors: ResolveImportError[] = [];

  // The module we're resolving, its scope, and the global export index.
  constructor(
    private modulePath: string,
    private mod: Module,
    private scopesPass: CreateScopes,
    private exportIndex: Map<string, ExportTable>,
    // Optional map for builtins by string name
    private builtins?: Map<string, ScopeElement>,
  ) {
    super();
  }

  // Helper: get this module's top scope
  private get moduleScope(): Scope {
    // If you keyed module_scopes by modulePath in CreateScopes, use that:
    const scope = this.scopesPass.module_scopes.get(this.modulePath);
    if (!scope) {
      throw new Error(`No module scope for ${this.modulePath}`);
    }
    return scope;
  }

  // Helper: define or replace an element in the current module scope
  private define(
    name: NameIdentifier | TypeIdentifier,
    element: ScopeElement,
  ): void {
    this.moduleScope.elements.set(name, element);
  }

  // Resolve a specific module path to an ExportTable
  private exportsOf(path: string): ExportTable | null {
    return this.exportIndex.get(path) ?? null;
  }

  override visitModule(node: Module): Module {
    // We only need to intercept import declarations; no recursive walk required
    for (const decl of node.module) {
      if ("import_dec" in decl) {
        this.resolveImportDeclaration(decl);
      }
    }
    return node;
  }

  private resolveImportDeclaration(node: ImportDeclaration) {
    const from = node.import_dec.import_from;
    const ex = this.exportsOf(from);
    if (!ex) {
      this.errors.push({ unknown_module: { from } });
      return;
    }

    for (const imp of node.import_dec.imports) {
      if ("fn" in imp) {
        this.resolveFnImport(ex, imp, from);
      } else if ("global" in imp) {
        this.resolveGlobalImport(imp); // external (WASM), keep as-is or attach info
      } else if ("table" in imp) {
        this.resolveTableImport(imp);
      } else if ("memory" in imp) {
        this.resolveMemoryImport(imp);
      } else if ("name" in imp) {
        this.resolveNameImport(ex, imp, from);
      } else if ("type" in imp) {
        this.resolveTypeImport(ex, imp, from);
      } else if ("trait" in imp) {
        this.resolveTraitImport(ex, imp, from);
      } else if ("enum" in imp) {
        this.resolveEnumImport(ex, imp, from);
      } else if ("star" in imp) {
        this.resolveStarImport(ex, from, imp.star);
      } else if ("builtin" in imp) {
        this.resolveBuiltinImport(imp);
      }
    }
  }

  private resolveFnImport(ex: ExportTable, imp: FnImport, from: string) {
    // Find the requested symbol name (either identifier or string)
    const requested = nameOf(imp.fn.name);
    const found = ex.values.get(requested);

    if (!found || !("fn" in found)) {
      this.errors.push({
        unknown_export: { from, symbol: requested, kind: "fn" },
      });
      return;
    }

    // Determine alias token to bind in this module scope
    const alias: NameIdentifier | null =
      imp.fn.alias ?? (("name" in imp.fn.name && imp.fn.name) || null); // only bind to NameIdentifier

    if (!alias) {
      // Your parser allows string names; CreateScopes requires alias or "name in name".
      // If we get here without NameIdentifier, surface a useful error.
      this.errors.push({
        bad_alias: {
          alias: requested,
          reason:
            "Importing functions by string requires an explicit alias (as name).",
        },
      });
      return;
    }

    // Install the concrete element
    this.define(alias, found);
  }

  private resolveNameImport(ex: ExportTable, imp: NameImport, from: string) {
    const requested = nameOf(imp.name.name);
    const found = ex.values.get(requested) ?? ex.types.get(requested); // allow ambiguous? Prefer values.

    if (!found) {
      this.errors.push({
        unknown_export: { from, symbol: requested, kind: "name" },
      });
      return;
    }

    const alias = imp.name.alias ?? imp.name.name;
    // If alias collides with an existing binding, you may want to emit a conflict.
    this.define(alias, found);
  }

  private resolveTypeImport(ex: ExportTable, imp: TypeImport, from: string) {
    const requested = nameOf(imp.type.name);
    const found = ex.types.get(requested);

    if (!found) {
      this.errors.push({
        unknown_export: { from, symbol: requested, kind: "type" },
      });
      return;
    }

    const alias = imp.type.alias ?? imp.type.name;
    this.define(alias, found);
  }

  private resolveTraitImport(ex: ExportTable, imp: TraitImport, from: string) {
    const requested = nameOf(imp.trait.name);
    const found = ex.types.get(requested);

    if (!found || !("trait" in found)) {
      this.errors.push({
        unknown_export: { from, symbol: requested, kind: "trait" },
      });
      return;
    }

    const alias = imp.trait.alias ?? imp.trait.name;
    this.define(alias, found);
  }

  private resolveEnumImport(ex: ExportTable, imp: EnumImport, from: string) {
    const requested = nameOf(imp.enum.name);
    const found = ex.types.get(requested);

    if (!found || !("enum" in found)) {
      this.errors.push({
        unknown_export: { from, symbol: requested, kind: "enum" },
      });
      return;
    }

    const alias = imp.enum.alias ?? imp.enum.name;
    this.define(alias, found);
  }

  // For external/WASM-y things, you probably keep them as imports but
  // you can also attach validated metadata or put them into scope to be used.
  private resolveGlobalImport(imp: GlobalImport) {
    const alias =
      imp.global.alias ||
      (("name" in imp.global.name && imp.global.name) as NameIdentifier | null);
    if (!alias) {
      this.errors.push({
        bad_alias: {
          alias: nameOf(imp.global.name),
          reason:
            "Global imports by string require an alias (as name) to be usable.",
        },
      });
      return;
    }
    // Keep as an import element or convert into a concrete symbol if you have a representation.
    this.define(alias, { global_import: imp });
  }

  private resolveTableImport(imp: TableImport) {
    const alias =
      imp.table.alias ||
      (("name" in imp.table.name && imp.table.name) as NameIdentifier | null);
    if (!alias) {
      this.errors.push({
        bad_alias: {
          alias: nameOf(imp.table.name),
          reason:
            "Table imports by string require an alias (as name) to be usable.",
        },
      });
      return;
    }
    this.define(alias, { table_import: imp });
  }

  private resolveMemoryImport(imp: MemoryImport) {
    const alias =
      imp.memory.alias ||
      (("name" in imp.memory.name && imp.memory.name) as NameIdentifier | null);
    if (!alias) {
      this.errors.push({
        bad_alias: {
          alias: nameOf(imp.memory.name),
          reason:
            "Memory imports by string require an alias (as name) to be usable.",
        },
      });
      return;
    }
    this.define(alias, { memory_import: imp });
  }

  private resolveBuiltinImport(imp: BuiltinImport) {
    const name = imp.builtin.name.string;
    const alias = imp.builtin.alias;
    if (!this.builtins || !this.builtins.has(name)) {
      // Fall back to keeping it as an import marker
      // or push an error if you want strict mode
      this.define(alias, { builtin: imp });
      return;
    }
    // Replace with resolved builtin element so later passes can use it
    this.define(alias, this.builtins.get(name)!);
  }

  private resolveStarImport(
    ex: ExportTable,
    from: string,
    token: TypeIdentifier,
  ) {
    // Strategy: import all public value-level and type-level names.
    // Detect conflicts with already-defined bindings.
    const local = this.moduleScope;

    // Values
    for (const [k, el] of ex.values) {
      // Create a synthetic NameIdentifier for the alias, or
      // if you prefer, you can disallow star import into synthetic tokens.
      const alias: NameIdentifier = { name: k };
      if (local.elements.has(alias)) {
        this.errors.push({
          import_conflict: {
            name: k,
            first_from: from,
            second_from: from,
          },
        });
        continue;
      }
      this.define(alias, el);
    }

    // Types
    for (const [k, el] of ex.types) {
      const alias: TypeIdentifier = { type: k };
      if (local.elements.has(alias)) {
        this.errors.push({
          import_conflict: {
            name: k,
            first_from: from,
            second_from: from,
          },
        });
        continue;
      }
      this.define(alias, el);
    }

    // Optionally keep a record of the star import token if you still need it
    // for diagnostics. Otherwise, you can ignore node.star in the scope now.
  }
}
