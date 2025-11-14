import * as path from "node:path";
import type { Declaration, ImportDeclaration, Module } from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export class ResolveImports extends BaseVisitor {
  modulePath: string = "";

  private imported = [] as string[];

  resolveImports(path: string, node: Module, modules: Set<string>) {
    this.modulePath = path;
    this.visitModule(node);
    const paths = this.context.modules.addModule(
      this.modulePath,
      node,
      this.imported,
    );
    console.log(paths);
    for (const p of paths) {
      modules.add(p);
    }
  }

  override visitModule(node: Module): Module {
    this.imported = [];
    super.visitModule(node);

    return node;
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    let desc = node.import_dec.import_from;
    if (desc.startsWith("@std")) {
      this.imported.push(desc);
      return node;
    }
    if (path.extname(desc) !== ".dew") desc += ".dew";
    this.imported.push(desc);
    return node;
  }
}
