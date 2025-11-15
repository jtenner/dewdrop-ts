// ./src/compiler.ts

import pervasives from "../std/pervasives.dew" with { type: "text" };
import { ModuleGraph, type ModuleEntry } from "./graph.js";
import { parse, parse_file } from "./parser.js";
import { DesugarPass } from "./passes/desugar.js";
import { ElaboratePass } from "./passes/elaborate.js";
import { ExportsPass } from "./passes/exports.js";
import { ResolveImports } from "./passes/resolve.js";
import { ScopesPass } from "./passes/scopes.js";
import { TypeCheckPass } from "./passes/typecheck.js";
import { type CompilerContext, to_file_entry as toFileEntry } from "./util.js";

export type CompilerOptions = {
  basedir: string;
  main: string;
};

// all the pervasives are globals
const prevasivesModule = await parse(pervasives);
if (prevasivesModule.errors.length > 0) {
  console.error("In pervasives module");
  for (const error of prevasivesModule.errors) {
    console.error(error);
  }
  process.exit(1);
}

const compilerContext = (basedir: string): CompilerContext => ({
  globalModule: {
    dependencies: new Set(),
    dependents: new Set(),
    module: prevasivesModule.module,
    relativePath: "@std/pervasives",
  } as ModuleEntry,
  globalScope: {
    parent: null,
    terms: new Map(),
    types: new Map(),
  },
  modules: new ModuleGraph(basedir),
  patterns: new Map(),
  scopes: new Map(),
  terms: new Map(),
  types: new Map(),
  builtins: new Map([
    ["unreachable", {}],
    ["debug", {}],
  ]),
  bindings: new Map(),
});

// we need to scopify the global context for later reuse

export async function compile(options: Partial<CompilerOptions> = {}) {
  const basedir = options.basedir ?? (await process.cwd());
  const main = options.main ?? "./src/main.dew";
  const entry = toFileEntry(main, basedir);
  const context = compilerContext(basedir);

  const toVisit = new Set([entry.relative]);

  const resolver = new ResolveImports(context);

  // pass 1: Resolve all the modules
  for (const relative of toVisit) {
    const module = await parse_file(relative);
    if (module.errors.length > 0) {
      console.error(`in file: ${relative}`);
      for (const error of module.errors) {
        console.error(error);
      }
      process.exit(1);
    }

    resolver.modulePath = relative;
    resolver.resolveImports(relative, module.module, toVisit);
  }

  const modules = context.modules.getDependencyOrder();
  modules.unshift(context.globalModule);

  const passes = [
    // pass 2: desugar the syntax
    new DesugarPass(context),
    // pass 3: create scopes
    new ScopesPass(context),
    // pass 4: move imports from other module exports
    new ExportsPass(context),
    // new ResolveImports(context),
    new ElaboratePass(context),
    new TypeCheckPass(context),
  ];

  for (const pass of passes) {
    for (const entry of modules) {
      pass.visit(entry);
      if (pass.errors.length > 0) {
        for (const error of pass.errors) console.error(error);
        process.exit(1);
      }
    }
  }
}
