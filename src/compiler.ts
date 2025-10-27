// ./src/compiler.ts

import pervasives from "../std/pervasives.dew" with { type: "text" };
import { parse, parse_file } from "./parser.js";
import { ArrowBindSugarPass } from "./passes/arrow_bind_sugar.js";
import { CreateScopes } from "./passes/create_scopes.js";
import { ElaboratePass } from "./passes/elaborate.js";
import { ImportResolution } from "./passes/import_resolution.js";
import { ResolveImports } from "./passes/resolve_imports.js";
import { TypeChecker } from "./passes/type_checker.js";
import { type Binding, typecheck } from "./types_system_f_omega.js";
import { type Builtin, to_file_entry } from "./util.js";

export type CompilerOptions = {
  basedir: string;
  main: string;
};

export async function compile(options: Partial<CompilerOptions> = {}) {
  const basedir = options.basedir ?? (await process.cwd());
  const main = options.main ?? "./src/main.dew";
  const entry = to_file_entry(main, basedir);

  const to_visit = new Set([entry.relative]);
  const builtins = new Map<string, Builtin>();
  const resolver = new ResolveImports(basedir);

  // pass 1: Resolve all the modules
  for (const relative of to_visit) {
    const module = await parse_file(relative);
    resolver.module_path = relative;
    resolver.resolve_imports(relative, module, to_visit);
  }

  const module_graph = resolver.graph;
  const modules = module_graph.getDependencyOrder();

  // pass 2: desugar arrow binds
  const arrow_pass = new ArrowBindSugarPass();
  for (const entry of modules) {
    entry.module = arrow_pass.visitModule(entry.module!);
  }

  // pass 3: name indexing
  const scope_pass = new CreateScopes(builtins);

  // all the pervasives are globals
  const pervasives_module = await parse(pervasives);
  scope_pass.scopifyModule("@std/pervasives.dew", pervasives_module);
  scope_pass.useGlobalsFrom(pervasives_module);

  // scopify each module
  for (const entry of modules) {
    scope_pass.scopifyModule(entry.relativePath, entry.module!);
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
    import_resolution.resolveImports(entry.relativePath, entry.module!);
  }

  if (import_resolution.errors.length > 0) {
    for (const error of import_resolution.errors) {
      console.error(error);
    }
    process.exit(1);
  }

  // pass 5: elaborate each module in dependency order
  const elaborate = new ElaboratePass(scopes, variants);
  elaborate.elaborate(pervasives_module);
  for (const entry of modules) {
    elaborate.elaborate(entry.module!);
  }
  if (elaborate.errors.length > 0) {
    for (const error of elaborate.errors) {
      console.error(error);
    }
    process.exit(1);
  }

  // type check each module
  const { terms, types } = elaborate;
  const globalScope = scope_pass.scopes.get(pervasives_module)!;
  if (!globalScope) throw new Error("Global scope not generated!");
  const checker = new TypeChecker(terms, types, globalScope);
  checker.check(pervasives_module);
  for (const entry of modules) {
    checker.check(entry.module!);
    if (checker.errors.length > 0) {
      for (const error of checker.errors) {
        console.error(error);
      }
      process.exit(1);
    }
  }
}
