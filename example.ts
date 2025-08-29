import { chars } from "./src/chars.js";
import { lex } from "./src/lexer.js";
import { take_pattern_expression } from "./src/parser.js";

console.log(await take_pattern_expression(null, lex(chars("#(1, 2, a)"))));
