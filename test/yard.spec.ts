import { describe, expect, it } from "bun:test";
import { type Operator, Yard } from "../src/yard";

describe("Yard Algorithm", () => {
  type Expr = number;

  const mkInfix = (
    sym: string,
    prec: number,
    assoc: "left" | "right",
    fn: (a: number, b: number) => number,
  ): Operator<Expr> => ({
    infix: [sym, prec, assoc, fn],
  });

  const mkPrefix = (
    sym: string,
    fn: (a: number) => number,
  ): Operator<Expr> => ({
    prefix: [sym, fn],
  });

  const mkPostfix = (
    sym: string,
    fn: (a: number) => number,
  ): Operator<Expr> => ({
    postfix: [sym, fn],
  });

  it("evaluates simple infix operations", () => {
    const yard = new Yard<Expr>();
    const add = mkInfix("+", 1, "left", (a, b) => a + b);
    const mul = mkInfix("*", 2, "left", (a, b) => a * b);

    yard.push_expr(2);
    yard.push_op(add);
    yard.push_expr(3);
    yard.push_op(mul);
    yard.push_expr(4);

    const result = yard.finalize();
    expect(result).toBe(14); // 2 + 3 * 4 == 14 (checks precedence)
  });

  it("respects parentheses (group)", () => {
    const yard = new Yard<Expr>();
    const add = mkInfix("+", 1, "left", (a, b) => a + b);
    const mul = mkInfix("*", 2, "left", (a, b) => a * b);

    // (2 + 3) * 4
    yard.push_group();
    yard.push_expr(2);
    yard.push_op(add);
    yard.push_expr(3);
    yard.pop_group();
    yard.push_op(mul);
    yard.push_expr(4);

    const result = yard.finalize();
    expect(result).toBe(20);
  });

  it("supports right-associative operators", () => {
    const yard = new Yard<Expr>();
    const pow = mkInfix("^", 3, "right", (a, b) => a ** b);

    yard.push_expr(2);
    yard.push_op(pow);
    yard.push_expr(3);
    yard.push_op(pow);
    yard.push_expr(2);

    const result = yard.finalize();
    expect(result).toBe(512); // 2^(3^2)
  });

  it("handles prefix operators", () => {
    const yard = new Yard<Expr>();
    const neg = mkPrefix("-", (a) => -a);
    const add = mkInfix("+", 1, "left", (a, b) => a + b);

    yard.push_op(neg);
    yard.push_expr(5);
    yard.push_op(add);
    yard.push_expr(3);

    const result = yard.finalize();
    expect(result).toBe(-2);
  });

  it("handles postfix operators", () => {
    const yard = new Yard<Expr>();
    const fact = mkPostfix("!", (a) =>
      Array.from({ length: a }, (_, i) => i + 1).reduce((t, x) => t * x, 1),
    );

    yard.push_expr(4);
    yard.push_op(fact);

    const result = yard.finalize();
    expect(result).toBe(24);
  });

  it("throws on invalid pop of group", () => {
    const yard = new Yard<Expr>();
    yard.push_group();
    expect(() => yard.eval_op()).toThrow("Cannot pop Group operator.");
  });
});

describe("Yard Algorithm (Extended)", () => {
  type Expr = number;

  const mkInfix = (
    sym: string,
    prec: number,
    assoc: "left" | "right",
    fn: (a: number, b: number) => number,
  ): Operator<Expr> => ({
    infix: [sym, prec, assoc, fn],
  });

  const mkPrefix = (
    sym: string,
    fn: (a: number) => number,
  ): Operator<Expr> => ({
    prefix: [sym, fn],
  });

  const mkPostfix = (
    sym: string,
    fn: (a: number) => number,
  ): Operator<Expr> => ({
    postfix: [sym, fn],
  });

  const add = mkInfix("+", 1, "left", (a, b) => a + b);
  const mul = mkInfix("*", 2, "left", (a, b) => a * b);
  const sub = mkInfix("-", 1, "left", (a, b) => a - b);

  // ✅ Basic shallow sanity check
  it("produces null on incomplete expressions", () => {
    expect(() => {
      const yard = new Yard<Expr>();
      yard.push_expr(42);
      yard.push_op(add); // missing RHS
      yard.finalize();
    }).toThrow();
  });

  // ✅ Error handling: popping beyond available expressions
  it("throws on ExprStack underflow when too many operators", () => {
    const yard = new Yard<Expr>();
    yard.push_expr(5);
    yard.push_op(add);
    // There’s no right-hand operand
    expect(() => yard.eval_op()).toThrow("ExprStack overflow");
  });

  // ✅ Error when popping an operator from an empty stack
  it("throws on OpStack underflow when trying to pop without ops", () => {
    const yard = new Yard<Expr>();
    expect(() => yard.pop_op()).toThrow("OpStack underflow.");
  });

  // ✅ Error when evaluating without operators
  it("throws on eval_op when operator stack is empty", () => {
    const yard = new Yard<Expr>();
    yard.push_expr(1);
    expect(() => yard.eval_op()).toThrow("OpStack underflow.");
  });

  // ✅ Ensures group push/pop counters are balanced
  it("throws on unbalanced pop_group", () => {
    const yard = new Yard<Expr>();
    expect(() => yard.pop_group()).toThrow("OpStack underflow.");
  });

  it("restores nested state after group", () => {
    const yard = new Yard<Expr>();
    expect(yard.is_nested).toBe(false);
    yard.push_group();
    expect(yard.is_nested).toBe(true);
    yard.push_expr(5);
    yard.pop_group();
    expect(yard.is_nested).toBe(false);
  });

  // Detects impossible operator state (sanity guard)
  it("throws on impossible operator variant", () => {
    const yard = new Yard<Expr>();
    // biome-ignore lint/suspicious/noExplicitAny: testing a bogus input
    yard.op_stack.push({ bogus: true } as any);
    expect(() => yard.eval_op()).toThrow("Impossible state.");
  });

  // Postfix after prefix sequence handling (algorithmic edge)
  it("handles chained prefix and postfix correctly", () => {
    const neg = mkPrefix("-", (a) => -a);
    const fact = mkPostfix("!", (a) =>
      Array.from({ length: a }, (_, i) => i + 1).reduce((t, x) => t * x, 1),
    );

    const yard = new Yard<Expr>();
    yard.push_op(neg);
    yard.push_expr(4);
    yard.push_op(fact);

    const result = yard.finalize();
    // -4! = -(4!) = -24
    expect(result).toBe(-24);
  });

  // ✅ Respects chaining and precedence in negative compound expressions
  it("evaluates complex combined expression", () => {
    const neg = mkPrefix("-", (a) => -a);

    // -3 + (2 * 5) - 4
    const yard = new Yard<Expr>();
    yard.push_op(neg);
    yard.push_expr(3);
    yard.push_op(add);
    yard.push_group();
    yard.push_expr(2);
    yard.push_op(mul);
    yard.push_expr(5);
    yard.pop_group();
    yard.push_op(sub);
    yard.push_expr(4);

    const result = yard.finalize();
    expect(result).toBe(3); // -3 + 10 - 4 == 3
  });
});
