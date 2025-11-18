import { elaborate } from "../elaborator.js";
import type { Module } from "../parser.js";
import type { CompilerContext } from "../util.js";

export class ElaboratorPass {
  constructor(public context: CompilerContext) {}
  visit(module: Module) {
    const worklist = elaborate(module);
    this.context.worklists.set(module, worklist);
  }
}
