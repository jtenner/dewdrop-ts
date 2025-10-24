// ./src/compiler.ts

import { parse_file } from "./parser.js";
import { ArrowBindSugarPass } from "./passes/arrow_bind_sugar.js";
import { CreateScopes } from "./passes/create_scopes.js";
import { ElaboratePass } from "./passes/elaborate.js";
import { ImportResolution } from "./passes/import_resolution.js";
import { ResolveImports } from "./passes/resolve_imports.js";
import { to_file_entry } from "./util.js";

export type CompilerOptions = {
  basedir: string;
  main: string;
};

export async function compile(options: Partial<CompilerOptions> = {}) {
  const basedir = options.basedir ?? (await process.cwd());
  const main = options.main ?? "./src/main.dew";
  const entry = to_file_entry(main, basedir);

  const to_visit = new Set([entry.relative]);
  const resolver = new ResolveImports(basedir);

  // pass 1: Resolve all the modules
  for (const relative of to_visit) {
    const module = await parse_file(relative);
    resolver.module_path = relative;
    resolver.resolve_imports(relative, module, to_visit);
  }
  console.log(to_visit);

  const module_graph = resolver.graph;
  const modules = module_graph.getDependencyOrder();

  // pass 2: desugar arrow binds
  const arrow_pass = new ArrowBindSugarPass();
  for (const entry of modules) {
    entry.module = arrow_pass.visitModule(entry.module!);
  }

  // pass 3: name indexing
  const scope_pass = new CreateScopes();
  for (const entry of modules) {
    scope_pass.visitModule(entry.module!);
  }

  if (scope_pass.errors.length > 0) {
    for (const error of scope_pass.errors) {
      console.error(error);
    }
    process.exit(1);
  }

  const scopes = scope_pass.getScopeIndex();
  const variants = scope_pass.variants;
  const entries = new Map(modules.map((t) => [t.relativePath, t.module!]));
  const import_resolution = new ImportResolution(entries, scopes);

  // pass 4: import resolution
  for (const entry of modules) {
    console.log("Resolving imports of", entry);
    import_resolution.resolveImports(entry.relativePath, entry.module!);
  }

  if (import_resolution.errors.length > 0) {
    for (const error of import_resolution.errors) {
      console.error(error);
    }
    process.exit(1);
  }

  // pass 5: elaborate each module in dependency order
  const pass = new ElaboratePass(scopes, variants);
  for (const entry of modules) {
    pass.elaborate(entry.module!);
  }
}
