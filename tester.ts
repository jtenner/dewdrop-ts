import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
import { take_declaration } from "./src/parser.ts";

const input = `impl<t> Eq for Option<t> { fn eq(other: Option<t>): Bool { true } }`
const result = await take_declaration(null, lex(chars(input)));
console.log(result);
