import type { Module } from "../parser.js";
import { BaseWalker } from "../visitor.js";

export class ImportsPass extends BaseWalker {
  resolveImports(module: Module) {}
}
