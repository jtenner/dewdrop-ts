import * as path from "node:path";
import type { Declaration, ImportDeclaration, Module } from "../parser.js";
import { BaseVisitor } from "../visitor.js";

const stddir = path.resolve(path.join(import.meta.dir, "../../std"));

export class CollectImportsPass extends BaseVisitor {
  private module_dir: string = "";

  constructor(public queue: Set<string>) {
    super();
  }

  collect(module_path: string, module: Module) {
    this.module_dir = path.dirname(module_path);
    this.visitModule(module);
  }

  override visitDeclaration(node: Declaration): Declaration {
    if ("import_dec" in node) return this.visitImportDeclaration(node);
    return node;
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    let { import_from } = node.import_dec;
    if (!import_from.toLowerCase().endsWith(".dew")) import_from += ".dew";
    if (import_from.startsWith("@std/")) {
      this.queue.add(path.join(stddir, import_from.slice("@std/".length)));
    } else if (path.isAbsolute(import_from)) {
      this.queue.add(import_from);
    } else {
      this.queue.add(path.join(this.module_dir, import_from));
    }

    return node;
  }
}
