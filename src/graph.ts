import * as path from "node:path";
import type { Module } from "./parser.js";

export interface ModuleEntry {
  relativePath: string;
  dependencies: Set<string>;
  dependents: Set<string>;
  module: Module | null;
}

export class ModuleGraph {
  private entries = new Map<string, ModuleEntry>();
  private baseDir: string;

  constructor(baseDir: string) {
    this.baseDir = path.resolve(baseDir);
  }

  addModule(filePath: string, mod: Module): ModuleEntry {
    const relativePath = this.toRelativePath(filePath);

    let entry = this.entries.get(relativePath);
    if (!entry) {
      entry = {
        relativePath,
        dependencies: new Set(),
        dependents: new Set(),
        module: mod,
      };
      this.entries.set(relativePath, entry);
    } else {
      entry.module = mod;
    }

    return entry;
  }

  addDependency(filePath: string, dependsOn: string): void {
    // `filePath` is the importer, `dependsOn` is a path *relative to that file*.
    const fromRelative = this.toRelativePath(filePath);
    const depRelative = this.resolveRelativeImport(fromRelative, dependsOn);

    // Ensure the importer exists in the graph
    let fromEntry = this.entries.get(fromRelative);
    if (!fromEntry) {
      fromEntry = {
        relativePath: fromRelative,
        dependencies: new Set(),
        dependents: new Set(),
        module: null,
      };
      this.entries.set(fromRelative, fromEntry);
    }

    // Ensure the dependency exists in the graph
    let depEntry = this.entries.get(depRelative);
    if (!depEntry) {
      depEntry = {
        relativePath: depRelative,
        dependencies: new Set(),
        dependents: new Set(),
        module: null,
      };
      this.entries.set(depRelative, depEntry);
    }

    // Wire the relationship
    fromEntry.dependencies.add(depRelative);
    depEntry.dependents.add(fromRelative);
  }

  // Get modules in dependency order (topological sort)
  getDependencyOrder(): ModuleEntry[] {
    const sorted: ModuleEntry[] = [];
    const visited = new Set<string>();
    const visiting = new Set<string>();

    const visit = (modulePath: string): void => {
      if (visited.has(modulePath)) return;

      if (visiting.has(modulePath)) {
        throw new Error(`Circular dependency detected: ${modulePath}`);
      }

      visiting.add(modulePath);
      const module = this.entries.get(modulePath);

      if (!module) {
        throw new Error("This module should exist..");
      }

      for (const dep of module.dependencies) {
        visit(dep);
      }

      visiting.delete(modulePath);
      visited.add(modulePath);
      sorted.push(module);
    };

    for (const modulePath of this.entries.keys()) {
      visit(modulePath);
    }

    return sorted;
  }

  getEntries(): Map<string, Module> {
    return new Map(
      [...this.entries].map(([key, entry]) => [key, entry.module!]),
    );
  }

  // Find a module relative to another module
  findModule(fromPath: string, relativePath: string): string | null {
    const fromRelative = this.toRelativePath(fromPath);
    const targetRelative = this.resolveRelativeImport(
      fromRelative,
      relativePath,
    );

    return this.entries.has(targetRelative) ? targetRelative : null;
  }

  // Get all direct dependencies of a module
  getDependencies(filePath: string): string[] {
    const relativePath = this.toRelativePath(filePath);
    const module = this.entries.get(relativePath);
    return module ? Array.from(module.dependencies) : [];
  }

  // Get all modules that depend on this module
  getDependents(filePath: string): string[] {
    const relativePath = this.toRelativePath(filePath);
    const module = this.entries.get(relativePath);
    return module ? Array.from(module.dependents) : [];
  }

  // Convert a file path to relative path from baseDir
  private toRelativePath(filePath: string): string {
    const absolutePath = path.isAbsolute(filePath)
      ? path.resolve(filePath)
      : path.resolve(this.baseDir, filePath);

    return path.relative(this.baseDir, absolutePath);
  }

  // Resolve an import path relative to the importing module (by module-relative path)
  private resolveRelativeImport(fromPath: string, importPath: string): string {
    const fromDir = path.dirname(fromPath);
    const resolved = path.join(fromDir, importPath);
    return path.normalize(resolved);
  }
}
