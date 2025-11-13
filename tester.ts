import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
import { take_declaration } from "./src/parser.ts";

const input = `impl Show for Int { fn show(): String { "int" } }`
const result = await take_declaration(null, lex(chars(input)));
console.log(result);
