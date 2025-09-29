import { chars } from "./src/chars.js";
import { lex } from "./src/lexer.js";
import { take_declaration, take_pattern_expression } from "./src/parser.js";

console.log(await take_declaration(null, lex(chars(`import "wasm" { type Option as Maybe }`))));
