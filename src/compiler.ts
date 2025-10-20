import {
  type Declaration,
  type ImportDeclaration,
  type Module,
  parse_file,
} from "./parser.js";
import { ArrowBindSugarPass } from "./passes/arrow_bind_sugar.js";
import { CreateScopes } from "./passes/create_scopes.js";
import { FreshIdentifiers } from "./passes/fresh_identifiers.js";
import { ResolveImports } from "./passes/resolve_imports.js";
import { to_file_entry } from "./util.js";
import { BaseVisitor } from "./visitor.js";

export type CompilerOptions = {
  basedir: string;
  main: string;
};

class CycleError extends Error {
  constructor(public cycle: string[]) {
    super(`Circular dependency detected: ${cycle.join(" -> ")}`);
  }
}

function getImports(mod: Module, basedir: string) {
  const refs = new Set<string>();
  class ImportVisitor extends BaseVisitor {
    override visitImportDeclaration(node: ImportDeclaration): Declaration {
      const loc = node.import_dec.import_from;
      if (loc.startsWith("@std")) return node;
      const entry = to_file_entry(loc, basedir, ".dew");
      refs.add(entry.relative);
      return node;
    }
  }
  new ImportVisitor().visitModule(mod);
  return refs;
}

function module_sort(modules: [string, Module][], basedir: string): string[] {
  const moduleMap = new Map(modules);
  const visited = new Set<string>();
  const visiting = new Set<string>();
  const result = [] as string[];

  function visit(id: string, path: string[] = []): void {
    if (visited.has(id)) return;

    if (visiting.has(id)) {
      const cycleStart = path.indexOf(id);
      throw new CycleError([...path.slice(cycleStart), id]);
    }

    visiting.add(id);
    const module = moduleMap.get(id);
    if (module) {
      for (const imported of getImports(module, basedir)) {
        visit(imported, [...path, id]);
      }
    }

    visiting.delete(id);
    visited.add(id);
    result.push(id);
  }

  for (const module of modules) {
    visit(module[0]);
  }

  return result;
}

export async function compile(options: Partial<CompilerOptions>) {
  const basedir = options.basedir ?? (await process.cwd());
  const main = options.main ?? "./src/main.dew";
  const entry = to_file_entry(main, basedir);

  const to_visit = new Set([entry.relative]);
  const resolver = new ResolveImports(basedir, entry.relative, to_visit);
  const modules = new Map<string, Module>();

  // pass 1: Resolve all the modules
  for (const relative of to_visit) {
    const module = await parse_file(relative);
    resolver.module_path = relative;
    resolver.visitModule(module);
    modules.set(relative, module);
  }

  // pass 2: desugar arrow binds
  const arrow_pass = new ArrowBindSugarPass();
  for (const [mod_path, mod] of modules) {
    modules.set(mod_path, arrow_pass.visitModule(mod));
  }

  // pass 3: identifiers must be unique, regenerate "re-used" ones
  const fresh_identifiers = new FreshIdentifiers();
  for (const [mod_path, mod] of modules) {
    modules.set(mod_path, fresh_identifiers.visitModule(mod));
  }

  // pass 4: name indexing
  const sorted = module_sort([...modules], basedir);
  const scope_pass = new CreateScopes();
  for (const mod_path of sorted) {
    const mod = modules.get(mod_path);
    scope_pass.mod_path = mod_path;
    modules.set(mod_path, scope_pass.visitModule(mod!));
  }

  const scopes = scope_pass.scopes;
  const module_scopes = scope_pass.module_scopes;

  // pass 5: import resolution
}
