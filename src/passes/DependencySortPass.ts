import type { Module } from "../parser.js";

// passes/DependencySortPass.ts
export class DependencySortPass {
  private visited = new Set<string>();
  private sorted: string[] = [];
  private visiting = new Set<string>();

  constructor(
    _modules: Map<string, Module>,
    private imports: Map<string, string[]>, // module -> imported module paths
  ) {}

  sort(entryPoints: string[]): string[] {
    for (const entry of entryPoints) {
      this.visit(entry);
    }
    return this.sorted;
  }

  private visit(modulePath: string) {
    if (this.visited.has(modulePath)) return;

    if (this.visiting.has(modulePath)) {
      throw new Error(`Circular dependency detected involving: ${modulePath}`);
    }

    this.visiting.add(modulePath);

    const deps = this.imports.get(modulePath) ?? [];
    for (const dep of deps) {
      this.visit(dep);
    }

    this.visiting.delete(modulePath);
    this.visited.add(modulePath);
    this.sorted.push(modulePath);
  }
}
