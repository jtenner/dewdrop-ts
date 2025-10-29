import { take_declaration, take_expression } from "./src/parser";
import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
const result = await take_declaration(null, lex(chars("builtin \"i32.add\" as i32_add(a: I32, b: I32): I32")));

console.log(result);