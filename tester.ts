import { take_expression } from "./src/parser";
import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
const result = await take_expression(null, lex(chars("(x, y) => { x + y }")));

console.log(result);