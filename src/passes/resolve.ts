import * as path from "node:path";
import type { Declaration, ImportDeclaration, Module } from "../parser.js";
import { BaseVisitor, BaseWalker } from "../visitor.js";

export class ResolveImports extends BaseWalker {
  resolveImports(path: string, node: Module, modules: Set<string>) {
    this.modulePath = path;
    this.walkModule(node);
    this.context.modules.addModule(this.modulePath, node);

    const paths = this.context.modules.getDependencies(this.modulePath);
    for (const p of paths) {
      modules.add(p);
    }
  }

  override walkImportDeclaration(node: ImportDeclaration): void {
    let desc = node.import_dec.import_from;
    if (desc.startsWith("@std/")) return;

    if (path.extname(desc) !== ".dew") desc += ".dew";
    this.context.modules.addDependency(this.modulePath, desc);
  }
}
