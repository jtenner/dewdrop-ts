import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";

console.log(await Array.fromAsync(lex(chars(`"\\x00"`))));