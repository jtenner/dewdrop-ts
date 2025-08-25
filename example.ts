import { chars } from "./src/chars.js";
import { lex } from "./src/lexer.js";
import { take_expression } from "./src/parser.js";

console.log(await take_expression(null, lex(chars("foo(1, 2, bar)"))));
