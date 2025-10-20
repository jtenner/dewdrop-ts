import { take_expression } from "./src/parser";
import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
const result = await take_expression(null, lex(chars("match x { Some(y) if (y > 0) => y, _ => 0 }")));

console.log(result);