import * as path from "node:path";
import { type Module, parse_file } from "./parser.js";
import { CollectImportsPass } from "./passes/CollectImportsPass.js";
import { DependencySortPass } from "./passes/DependencySortPass.js";
import {
  ImportResolutionPass,
  type ImportedSymbol,
} from "./passes/ImportResolutionPass.js";
import {
  SymbolResolutionPass,
  type SymbolTable,
} from "./passes/SymbolResolutionPass.js";
import { type FileEntry, to_file_entry } from "./util.js";
import { TypeCheckingPass } from "./passes/TypeCheckingPass.js";
import type { Type } from "./types_system_f_omega.js";

export type CompilerConfig = {
  entries: string[];
  basedir: string;
};

export class Compiler {
  entries: Set<FileEntry>;
  modules = new Map<string, Module>();
  symbolTables = new Map<string, SymbolTable>();
  imports = new Map<string, ImportedSymbol[]>();
  typeCache = new Map<string, Type>();
  sortedModules: string[] = [];

  constructor(config: Partial<CompilerConfig> = {}) {
    const basedir = config.basedir ?? ".";
    const entries = (config.entries ?? ["./src/index.dew"]).map((entry) =>
      to_file_entry(entry, basedir),
    );
    this.entries = new Set(entries);
  }

  async compile() {
    // Phase 1: Parse all files

    await this.parsePhase();

    // Phase 2: Build symbol tables

    this.symbolResolutionPhase();

    // Phase 3: Resolve imports

    const importMap = this.importResolutionPhase();

    // Phase 4: Sort dependencies

    this.dependencySortPhase(importMap);

    // Phase 5: Type checking (next step!)
    console.log("\nPhase 5: Type checking...");
    const typeCheckSuccess = this.typeCheckingPhase();

    if (!typeCheckSuccess) {
      console.error("\n❌ Type checking failed!");
      process.exit(1);
    }

    console.log("\n✅ Compilation successful!");
  }

  private async parsePhase() {
    const queue = new Set(Array.from(this.entries).map((e) => e.absolute));
    const collect = new CollectImportsPass(queue);

    for (const item of queue) {
      console.log("  Parsing:", item);
      const parsed = await parse_file(item);
      this.modules.set(item, parsed);
      collect.collect(item, parsed);
    }
  }

  private symbolResolutionPhase() {
    const resolver = new SymbolResolutionPass(this.symbolTables, this.modules);

    for (const [modulePath, module] of this.modules) {
      console.log("  Resolving symbols in:", modulePath);
      resolver.resolve(modulePath, module);
    }
  }

  private importResolutionPhase(): Map<string, string[]> {
    const importMap = new Map<string, string[]>();

    for (const [modulePath, module] of this.modules) {
      const resolver = new ImportResolutionPass(
        this.symbolTables,
        path.dirname(modulePath),
      );

      console.log("  Resolving imports in:", modulePath);
      resolver.resolve(modulePath, module);

      const imports = resolver.getImports(modulePath);
      this.imports.set(modulePath, imports);

      const importedModules = [
        ...new Set(imports.map((imp) => imp.sourceModule)),
      ];
      importMap.set(modulePath, importedModules);
    }

    return importMap;
  }

  private dependencySortPhase(importMap: Map<string, string[]>) {
    const sorter = new DependencySortPass(this.modules, importMap);
    const entryPoints = Array.from(this.entries).map((e) => e.absolute);
    this.sortedModules = sorter.sort(entryPoints);
  }

  private typeCheckingPhase(): boolean {
    const checker = new TypeCheckingPass(
      this.symbolTables,
      this.imports,
      this.typeCache,
    );

    let allSuccess = true;

    // Type check modules in dependency order
    for (const modulePath of this.sortedModules) {
      const module = this.modules.get(modulePath)!;
      const success = checker.check(modulePath, module);

      if (!success) {
        allSuccess = false;
      }
    }

    const errors = checker.getErrors();
    if (errors.length > 0) {
      console.error("\nType errors found:");
      for (const { module, error } of errors) {
        console.error(`  In ${module}:`);
        console.error(`    ${error}`);
      }
    }

    return allSuccess;
  }
}
