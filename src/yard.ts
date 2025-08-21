export type Associativity = "left" | "right";
export type BinaryOpCallback<TExpression> = (
  left: TExpression,
  right: TExpression,
) => TExpression;
export type UnaryOpCallback<TExpression> = (expr: TExpression) => TExpression;
export type Operator<TExpression> =
  | { binary: [string, number, Associativity, BinaryOpCallback<TExpression>] }
  | { prefix: [string, UnaryOpCallback<TExpression>] }
  | { postfix: [string, UnaryOpCallback<TExpression>] }
  | { group: null };

export class Yard<TExpression> {
  op_stack: Operator<TExpression>[] = [];
  expr_stack: TExpression[] = [];

  get top_op(): Operator<TExpression> {
    if (this.op_stack.length === 0) throw new Error("OpStack underflow.");
    return this.op_stack.at(-1)!;
  }

  get top_expr(): TExpression {
    if (this.expr_stack.length === 0) throw new Error("ExprStack overflow");
    return this.expr_stack.at(-1)!;
  }

  can_push(op: Operator<TExpression>) {
    if (this.op_stack.length === 0) return true;

    const top = this.top_op;

    if ("binary" in op && "prefix" in top) return false;

    if ("binary" in op && "binary" in top) {
      return (
        // top has greater precidence
        op.binary[1] < top.binary[1] ||
        // top has same precidence but is left associative
        (op.binary[1] === top.binary[1] &&
          op.binary[2] === "left" &&
          top.binary[2] === "left")
      );
    }

    return true;
  }

  push_group() {
    this.op_stack.push({ group: null });
    return this;
  }

  pop_op(): Operator<TExpression> {
    const top = this.top_op;
    this.op_stack.length -= 1;
    return top;
  }

  push_op(op: Operator<TExpression>) {
    while (!this.can_push(op)) {
      this.eval_op();
    }
    this.op_stack.push(op);
  }

  push_expr(expr: TExpression) {
    this.expr_stack.push(expr);
    return this;
  }

  eval_op() {
    const top = this.pop_op();
    if ("prefix" in top) {
      const expr = this.pop_expr();
      const op = top.prefix[1];
      const result = op(expr);
      return this.push_expr(result);
    } else if ("postfix" in top) {
      const expr = this.pop_expr();
      const op = top.postfix[1];
      const result = op(expr);
      return this.push_expr(result);
    } else if ("group" in top) {
      throw new Error("Cannot pop Group operator.");
    } else if ("binary" in top) {
      const right = this.pop_expr();
      const left = this.pop_expr();
      const op = top.binary[3];
      const result = op(left, right);
      return this.push_expr(result);
    }
    throw new Error("Impossible state.");
  }

  pop_expr(): TExpression {
    const top = this.top_expr;
    this.expr_stack.length -= 1;
    return top;
  }

  pop_group() {
    while (true) {
      if (this.op_stack.length === 0) throw new Error("OpStack underflow.");
      if ("group" in this.op_stack.at(-1)!) {
        this.op_stack.length -= 1;
        return this;
      }
      this.eval_op();
    }
  }

  finalize(): TExpression {
    while (this.op_stack.length > 0) {
      this.eval_op();
    }
    if (this.expr_stack.length === 1) return this.expr_stack[0]!;
    throw new Error("Invalid Yard state, expression stack not empty.");
  }
}
