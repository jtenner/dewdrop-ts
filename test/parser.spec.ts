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

// Add these tests to your existing test file

// More complex expression tests
test.each([
  // Self expressions
  { expr: "self" },
  { expr: "self.field" },
  { expr: "self.method()" },

  // Anonymous functions
  { expr: "fn (x: Int) => { x + 1 }" },
  { expr: "fn (x, y) => { x + y }" },
  // { expr: "fn<t>(x: t): t { x }" },
  
  // Prefix operators
  { expr: "!x" },
  { expr: "-42" },
  { expr: "~bits" },
  { expr: ".!x" },
  { expr: "!.x" },
  { expr: ".~y" },
  { expr: "~.y" },
  { expr: ".-z" },
  { expr: "-.z" },
  
  // Postfix operators
  { expr: "x!" },
  { expr: "x?" },
  { expr: "x.!" },
  { expr: "x!." },
  { expr: "x.?" },
  { expr: "x?." },
  
  // Dotted operators (floating point variants)
  { expr: "a .+ b" },
  { expr: "a +. b" },
  { expr: "a .* b" },
  { expr: "a *. b" },
  { expr: "a ./ b" },
  { expr: "a /. b" },
  { expr: "a .** b" },
  { expr: "a **. b" },
  { expr: "a .- b" },
  { expr: "a -. b" },
  { expr: "a .% b" },
  { expr: "a %. b" },
  
  // Bitwise operators
  { expr: "a & b" },
  { expr: "a | b" },
  { expr: "a ^ b" },
  { expr: "a << b" },
  { expr: "a >> b" },
  { expr: "a .& b" },
  { expr: "a &. b" },
  { expr: "a .| b" },
  { expr: "a |. b" },
  { expr: "a .^ b" },
  { expr: "a ^. b" },
  { expr: "a .<< b" },
  { expr: "a <<. b" },
  { expr: "a .>> b" },
  { expr: "a >>. b" },
  
  // Comparison operators
  { expr: "a < b" },
  { expr: "a > b" },
  { expr: "a <= b" },
  { expr: "a >= b" },
  { expr: "a == b" },
  { expr: "a != b" },
  { expr: "a .< b" },
  { expr: "a <. b" },
  { expr: "a .> b" },
  { expr: "a >. b" },
  { expr: "a .<= b" },
  { expr: "a <=. b" },
  { expr: "a .>= b" },
  { expr: "a >=. b" },
  { expr: "a .== b" },
  { expr: "a ==. b" },
  { expr: "a .!= b" },
  { expr: "a !=. b" },
  
  // Logical operators
  { expr: "a && b" },
  { expr: "a || b" },
  { expr: "a ^^ b" },
  { expr: "a .&& b" },
  { expr: "a &&. b" },
  { expr: "a .|| b" },
  { expr: "a ||. b" },
  { expr: "a .^^ b" },
  { expr: "a ^^. b" },
  
  // Special operators
  { expr: "a ?? b" },
  { expr: "a .?? b" },
  { expr: "a ??. b" },
  { expr: "a .. b" },
  { expr: "a |> b" },
  { expr: "a <> b" },
  { expr: "a .<> b" },
  { expr: "a <>. b" },
  
  // Complex precedence
  { expr: "a + b * c ** d" },
  { expr: "a || b && c" },
  { expr: "!a && b || c" },
  { expr: "a + b < c * d" },
  { expr: "a |> b |> c" },
  { expr: "a ?? b ?? c" },
  
  // Chained method calls
  { expr: "obj.foo().bar().baz" },
  { expr: "list.map(fn(x) { x + 1 }).filter(fn(x) { x > 0 })" },
  
  // Complex nested structures
  { expr: "fn(x) { match x { Some(y) => y, None => 0 } }" },
  { expr: "if a { #{x: 1} } else { #{x: 2} }" },
  { expr: "{ let x = 1; let y = 2; x + y }" },
  
  // Inf and NaN
  { expr: "inf" },
  { expr: "nan" },
  { expr: "inf + 1" },
  { expr: "nan * 2" },
])("Expression: $expr matches snapshot", async ({ expr }) => {
  const [, expression] = await take_expression(null, lex(chars(expr)));
  expect(expression).toMatchSnapshot(`Expression: ${expr}`);
});

// More pattern expression tests
test.each([
  // Nested constructors
  { expr: "Some(Pair(x, y))" },
  { expr: "Result(Ok(value))" },
  
  // Complex record patterns
  { expr: "#{}" },
  { expr: "#{x, y, z}" },
  { expr: "#{point: #(x, y)}" },
  
  // Mixed patterns
  { expr: "Wrapper(#{inner: value})" },
  { expr: "Nested(#(1, #{x: y}))" },
])("Pattern: $expr matches snapshot", async ({ expr }) => {
  const [, pattern] = await take_pattern_expression(null, lex(chars(expr)));
  expect(pattern).toMatchSnapshot(`PatternExpression: ${expr}`);
});

// More type expression tests  
test.each([
  // Empty constructs
  { expr: "#()" },
  { expr: "#{}" },
  { expr: "() -> Void" },
  
  // Complex chains
  { expr: "A.B.C.D" },
  { expr: "Container<Inner<T>>.Value" },
  
  // Multiple type parameters
  { expr: "Map<K, V>" },
  { expr: "Triple<A, B, C>" },
  { expr: "Fn<(A, B), C>" },
  
  // Nested function types
  { expr: "((Int) -> Int) -> Int" },
  { expr: "(Int, (String) -> Bool) -> Result" },
  
  // Complex record types
  { expr: "#{pos: #(f64, f64), vel: #(f64, f64)}" },
])("Type: $expr matches snapshot", async ({ expr }) => {
  const [, typeExpr] = await take_type_expression(null, lex(chars(expr)));
  expect(typeExpr).toMatchSnapshot(`TypeExpression: ${expr}`);
});

// More body expression tests
test.each([
  // Complex let patterns
  { expr: "let #(x, y) = point" },
  { expr: "let #{name, age} = person" },
  { expr: "let Some(value) = option" },
  
  // Nested blocks
  { expr: "{ { a; b }; c }" },
  
  // Mixed body expressions
  { expr: "{ let x = 1; y <- compute(); x + y }" },
])("Body: $expr matches snapshot", async ({ expr }) => {
  const [, bodyExpr] = await take_body_expression(null, lex(chars(expr)));
  expect(bodyExpr).toMatchSnapshot(`BodyExpression: ${expr}`);
});

// More declaration tests
test.each([
  // Impl declarations
  { decl: `impl Show for Int { fn show(): String { "int" } }` },
  { decl: `impl Eq<T> for Option<T> { fn eq(other: Option<T>): Bool { true } }` },
  { decl: `impl<t> Default for Vec<t> { fn default(): Vec<t> { Vec() } }` },
  
  // Complex function declarations
  { decl: `fn compose<a, b, c>(f: (b) -> c, g: (a) -> b): (a) -> c { fn(x: a): c { f(g(x)) } }` },
  { decl: `pub fn fold<t, acc>(list: List<t>, init: acc, f: (acc, t) -> acc): acc { init }` },
  
  // Multi-parameter type declarations
  { decl: `enum Result<t, e> { Ok(t), Err(e) }` },
  { decl: `pub type Map<k, v> = #{entries: List<#(k, v)>}` },
  
  // Complex enum variants
  { decl: `enum Tree<t> { Leaf, Node { value: t, left: Tree<t>, right: Tree<t> } }` },
  { decl: `pub enum Message { Quit, Move { x: i32, y: i32 }, Write(String), ChangeColor(i32, i32, i32) }` },
  
  // Multiple trait methods
  { decl: `trait Numeric { fn add(other: Self): Self fn mul(other: Self): Self fn zero(): Self }` },
  
  // Complex imports
  { decl: `import "std" { fn print, fn println, type String, memory "mem": 1 10 as mem }` },
  { decl: `import "core" { trait Eq, trait Ord, enum Option, enum Result }` },
  
  // Functions with no return type annotation
  { decl: `fn side_effect(x: Int) { print(x) }` },
  
  // Functions with complex bodies
  { decl: `fn factorial(n: Int): Int { if n <= 1 { 1 } else { n * factorial(n - 1) } }` },
  { decl: `fn process(data: List<Int>): Int { let sum = data.fold(0, fn(acc, x) { acc + x }); sum / data.length() }` },
])("Declaration: $decl matches snapshot", async ({ decl }) => {
  const result = await take_declaration(null, lex(chars(decl)));
  expect(result).toMatchSnapshot(`Declaration: ${decl}`);
});

// Test edge cases and special scenarios
test.each([
  // Empty structures
  { expr: "foo()" },
  { expr: "#()" },
  { expr: "#{}" },
  { expr: "MyEnum" },
  
  // Deeply nested
  { expr: "a(b(c(d(e()))))" },
  { expr: "((((a))))" },
  
  // Mixed operators and calls
  { expr: "foo().bar + baz().qux * 2" },
  { expr: "(a + b)(c, d)" },
])("Edge case: $expr matches snapshot", async ({ expr }) => {
  const [, expression] = await take_expression(null, lex(chars(expr)));
  expect(expression).toMatchSnapshot(`EdgeCase: ${expr}`);
});

// Test match expressions with guards
test.each([
  { expr: "match x { Some(y) if y > 0 => y, _ => 0 }" },
  { expr: "match point { #(x, y) if x == y => x, #(x, _) => x, _ => 0 }" },
  { expr: "match value { n if n < 0 => -n, n if n > 100 => 100, n => n }" },
])("Match with guard: $expr matches snapshot", async ({ expr }) => {
  const [, expression] = await take_expression(null, lex(chars(expr)));
  expect(expression).toMatchSnapshot(`MatchWithGuard: ${expr}`);
});
