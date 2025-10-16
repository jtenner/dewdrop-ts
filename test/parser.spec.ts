import { expect, test } from "bun:test";
import { chars } from "../src/chars.js";
import { lex } from "../src/lexer.js";
import {
  take_body_expression,
  take_declaration,
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
test.each([
  { expr: "a <- b" },
  { expr: "{ a; b; c }"},
  { expr: "let a = b" },
  { expr: "let Some(a) = b" },
  { expr: "let assert Some(a) = b" },
  { expr: "d = a + b" },
])("$expr matches snapshot", async ({ expr }) => {
  const [, bodyExpr] = await take_body_expression(null, lex(chars(expr)));
  expect(bodyExpr).toMatchSnapshot(`BodyExpression: ${expr}`);
})

test.each([
  { decl: `fn add(x: I32, y: I32): I32 { x + y }` },
  { decl: `pub fn main() { 42 }` },
  { decl: `fn identity<t>(x: t): t { x }` },
  { decl: `let x: i32 = 42` },
  { decl: `pub let pi: f64 = 3.14159` },
  { decl: `let y = true` },
  { decl: `type MyInt = I32` },
  { decl: `pub enum Result<t, e> { Ok(t), Err(e) }` },
  { decl: `enum Color { Red, Green(I32), Blue { shade: f32 } }` },
  { decl: `pub enum Direction { Up, Down, Left, Right }` },
  { decl: `pub trait Eq<t> { fn eq(other: t) : bool }` },
  { decl: `trait Drawable { fn draw(): Void fn bounds(): Rect }` },
  { decl: `import "std" { fn print as out }` },
  { decl: `import "std" { memory "memory": 1 as mem }` },
  { decl: `import "std" { global mut count: i32 as cnt }` },
  { decl: `import "wasm" { table "table": I32 0 0 as tbl }` },
  { decl: `import "wasm" { type Option as Maybe }` },
  { decl: `import "wasm" { trait Semiring }` },
  { decl: `import "wasm" { builtin "i32.add" as add }` },
  { decl: `import "wasm" { item }` },
  { decl: `import "wasm" { item as blah }` },
])("$decl matches snapshot", async ({ decl }) => {
  const result = await take_declaration(null, lex(chars(decl)));
  expect(result).toMatchSnapshot();
});
