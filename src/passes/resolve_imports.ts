import * as path from "node:path";
import { ModuleGraph } from "../graph.js";
import type { Declaration, ImportDeclaration, Module } from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export class ResolveImports extends BaseVisitor {
  graph: ModuleGraph;
  module_path: string = "";

  private imported = [] as string[];
  constructor(public basedir: string) {
    super();
    this.graph = new ModuleGraph(basedir);
  }

  resolve_imports(path: string, node: Module, modules: Set<string>) {
    this.module_path = path;
    this.visitModule(node);
    const paths = this.graph.addModule(this.module_path, node, this.imported);
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
    if (desc.startsWith("@std")) return node;
    if (path.extname(desc) !== ".dew") desc += ".dew";
    this.imported.push(desc);
    return node;
  }
}
