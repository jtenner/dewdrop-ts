import * as path from "node:path";
import type { Declaration, ImportDeclaration, Module } from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export class ResolveImports extends BaseVisitor {
  modulePath: string = "";

  resolveImports(path: string, node: Module, modules: Set<string>) {
    this.modulePath = path;
    this.visitModule(node);
    this.context.modules.addModule(this.modulePath, node);

    const paths = this.context.modules.getDependencies(this.modulePath);
    for (const p of paths) {
      modules.add(p);
    }
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    let desc = node.import_dec.import_from;
    if (desc.startsWith("@std/")) return node;

    if (path.extname(desc) !== ".dew") desc += ".dew";
    this.context.modules.addDependency(this.modulePath, desc);
    return node;
  }
}
