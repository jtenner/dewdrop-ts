import { expect, test } from "bun:test";
import { lex } from "../src/lexer.js";
import { chars } from "../src/chars.js";
import { take_expression } from "../src/parser.js";

test.each([
  { expr: "1" },
  { expr: "a" },
  { expr: `"testing"` },
  { expr: "a + b * c - d" },
  { expr: "a + b + c + d" },
  { expr: "a ** b ** c ** d" },
  { expr: "(a + b) * c" },
  { expr: "foo()" },
  { expr: "foo(1, 2, bar)" },
  { expr: "obj.field" },
  { expr: "obj.method(x, y)" },

  // Constructor/type literal
  { expr: "MyType" }, // triggers take_type if capitalized
  { expr: "MyType()" }, // type constructor call

  // Tuples / records
  { expr: "#(1, 2, 3)" },
  { expr: "#{x: 1, y: 2}" },

  // Blocks
  { expr: "{ a; b; c }" },
  { expr: "{ let x = 1; x + 2 }" },

  // If expressions
  { expr: "if a { b } else { c }" },
  { expr: "if a { b }" },

  // Match expressions
  {
    expr: `match x { A => 1, B => 2, _ => 3 }`,
  },

  // Nested combinations
  {
    expr: "foo(1).bar(2, 3).baz",
  },
  {
    expr: "if match x { A => true, _ => false } { 1 } else { 0 }",
  },
])("$expr matches snapshot", async ({ expr }) => {
  const [, expression] = await take_expression(null, lex(chars(expr)));

  expect(expression).toMatchSnapshot(`Expression: ${expr}`);
});
