import type { Declaration, ImportDeclaration } from "../parser.js";
import { to_file_entry } from "../util.js";
import { BaseVisitor } from "../visitor.js";

export class ResolveImports extends BaseVisitor {
  constructor(
    public basedir: string,
    public module_path: string,
    public modules: Set<string>,
  ) {
    super();
  }
  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    const desc = node.import_dec.import_from;
    if (desc.startsWith("@std")) return node;
    const entry = to_file_entry(desc, this.basedir, ".dew");
    this.modules.add(entry.relative);
    return node;
  }
}
