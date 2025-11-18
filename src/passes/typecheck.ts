import { elaborate } from "../elaborator.js";
import type { Module } from "../parser.js";
import type { CompilerContext } from "../util.js";

export class TypeCheckPass {
  constructor(public context: CompilerContext) {}
  visit(module: Module) {
    const worklist = this.context.worklists.get(module);
    if (!worklist) throw new Error(`Worklist not generated for module.`);
  }
}
