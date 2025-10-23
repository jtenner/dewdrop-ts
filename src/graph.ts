import * as path from "path";
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

  // Add a module with its dependencies
  addModule(
    filePath: string,
    module: Module,
    dependencies: string[] = [],
  ): string[] {
    const relativePath = this.toRelativePath(filePath);
    let entry: ModuleEntry;

    if (this.entries.has(relativePath)) {
      entry = this.entries.get(relativePath)!;
      entry.module = module;
    } else {
      entry = {
        relativePath,
        dependencies: new Set(),
        dependents: new Set(),
        module,
      };
      this.entries.set(relativePath, entry);
    }

    const depPaths = [] as string[];
    // Resolve and add dependencies
    for (const dep of dependencies) {
      const depPath = this.resolveRelativeImport(relativePath, dep);
      depPaths.push(depPath);
      // Ensure dependency module exists
      if (!this.entries.has(depPath)) {
        this.entries.set(depPath, {
          relativePath: depPath,
          dependencies: new Set(),
          dependents: new Set(),
          module: null,
        });
      }

      entry.dependencies.add(depPath);
      this.entries.get(depPath)!.dependents.add(relativePath);
    }

    return depPaths;
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

      if (module) {
        for (const dep of module.dependencies) {
          visit(dep);
        }
      } else {
        throw new Error("This module should exist..");
      }

      visiting.delete(modulePath);
      visited.add(modulePath);
      sorted.push(module!);
    };

    for (const modulePath of this.entries.keys()) {
      visit(modulePath);
    }

    return sorted;
  }

  getEntries() {
    return new Map([...this.entries].map((e) => [e[0], e[1].module!]));
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

  // Get all dependencies of a module
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

  // Resolve an import path relative to the importing module
  private resolveRelativeImport(fromPath: string, importPath: string): string {
    const fromDir = path.dirname(fromPath);
    const resolved = path.join(fromDir, importPath);
    return path.normalize(resolved);
  }
}
