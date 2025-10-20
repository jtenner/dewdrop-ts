import type { NameToken, TypeToken } from "../lexer.js";
import { BaseVisitor } from "../visitor.js";

export class FreshIdentifiers extends BaseVisitor {
  seen = new Set<NameToken | TypeToken>();

  override visitNameIdentifier(node: NameToken): NameToken {
    if (this.seen.has(node)) {
      const next = { name: node.name };
      this.seen.add(next);
      return next;
    }

    this.seen.add(node);
    return node;
  }

  override visitTypeIdentifier(node: TypeToken): TypeToken {
    if (this.seen.has(node)) {
      const next = { type: node.type };
      this.seen.add(next);
      return next;
    }

    this.seen.add(node);
    return node;
  }
}
