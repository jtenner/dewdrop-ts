// ./src/compiler.ts

import pervasives from "../std/pervasives.dew" with { type: "text" };
import { parse, parse_file } from "./parser.js";
import { DesugarPass } from "./passes/desugar.js";
import { ResolveImports } from "./passes/resolve_imports.js";
import { type Builtin, to_file_entry } from "./util.js";

export type CompilerOptions = {
  basedir: string;
  main: string;
};

// all the pervasives are globals
const pervasives_module = await parse(pervasives);
if (pervasives_module.errors.length > 0) {
  console.error("In pervasives module");
  for (const error of pervasives_module.errors) {
    console.error(error);
  }
  process.exit(1);
}

export async function compile(options: Partial<CompilerOptions> = {}) {
  const basedir = options.basedir ?? (await process.cwd());
  const main = options.main ?? "./src/main.dew";
  const entry = to_file_entry(main, basedir);

  const to_visit = new Set([entry.relative]);
  const builtins = new Map<string, Builtin>([["unreachable", {}]]);
  const resolver = new ResolveImports(basedir);

  // pass 1: Resolve all the modules
  for (const relative of to_visit) {
    const module = await parse_file(relative);
    if (module.errors.length > 0) {
      console.error(`in file: ${relative}`);
      for (const error of module.errors) {
        console.error(error);
      }
      process.exit(1);
    }

    resolver.module_path = relative;
    resolver.resolve_imports(relative, module.module, to_visit);
  }

  const module_graph = resolver.graph;
  const modules = module_graph.getDependencyOrder();

  // pass 2: desugar arrow binds and expression statements
  const desugar = new DesugarPass();
  for (const entry of modules) {
    entry.module = desugar.visitModule(entry.module!);
  }

  // pass 3: name indexing
  // const scope_pass = new CreateScopes(builtins);
}
