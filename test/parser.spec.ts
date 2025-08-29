import { expect, test } from "bun:test";
import { chars } from "../src/chars.js";
import { lex } from "../src/lexer.js";
import {
  take_expression,
  take_pattern_expression,
  take_type_expression,
} from "../src/parser.js";

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

  // arrow bind
  {
    expr: "{ a <- try(explosion()) }",
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

test.each([
  // Identifier pattern
  { expr: "x" }, // name pattern
  { expr: "_wild" }, // name pattern with underscore

  // Constructor patterns
  { expr: "Some" }, // type constructor no args
  { expr: "Some(42)" }, // constructor with single int
  { expr: "Pair(1, x)" }, // constructor with multiple args and nested pattern

  // Literal patterns
  { expr: "123" },
  { expr: "3.14" },
  { expr: `"hello"` },
  { expr: "nan" },
  { expr: "inf" },

  // Tuple patterns
  { expr: "#(x, y)" }, // tuple with names
  { expr: "#(1, 2, 3)" }, // tuple with literals
  { expr: "#()" }, // empty tuple (should parse as tuple with empty array)

  // Record patterns
  { expr: "#{foo: x, bar: 42}" }, // record destructuring
  { expr: "#{nested: Pair(x, y)}" }, // nested constructor inside record

  // Nested combos
  { expr: "Some(#(x, #{y: 1}))" }, // constructor -> tuple -> record
])("$expr matches snapshot", async ({ expr }) => {
  const [, pattern] = await take_pattern_expression(null, lex(chars(expr)));
  expect(pattern).toMatchSnapshot(`PatternExpression: ${expr}`);
});

test.each([
  // Simple names
  { expr: "Int" }, // basic named type
  { expr: "MyNamespace.Type" }, // dotted select

  // Function types
  { expr: "(Int) -> Bool" }, // single arg function
  { expr: "(Int, String) -> Bool" }, // multiple args
  { expr: "() -> Void" }, // no args function

  // Tuple types
  { expr: "#(Int, Bool, String)" }, // tuple with multiple types
  { expr: "#()" }, // empty tuple

  // Record types
  { expr: "#{x: Int, y: Bool}" },
  { expr: "#{nested: #(Int, String), flag: Bool}" },

  // Chained record select
  { expr: "#{a: Int}.a" }, // chain record then select

  // Type application (generics)
  { expr: "List<Int>" }, // single generic arg
  { expr: "Map<String, Int>" }, // multiple generic args
  { expr: "Outer.Inner<Nested<T>>" }, // nested application and select

  // Combined constructs
  { expr: "(Int, List<String>) -> Map<String, Int>" }, // function with generics
  { expr: "(#(Int, Bool)) -> #{ok: String, err: String}" }, // function returning record
])("$expr matches snapshot", async ({ expr }) => {
  const [, typeExpr] = await take_type_expression(null, lex(chars(expr)));
  expect(typeExpr).toMatchSnapshot(`TypeExpression: ${expr}`);
});

// body expressions
