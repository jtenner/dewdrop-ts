import { take_declaration, take_expression, take_type_expression } from "./src/parser";
import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
const result = await take_declaration(null, lex(chars(`trait Map<t, u> {
  fn map<t, u>(cb: (t) => u): Self<u>
}`)));

console.log(result[1].err.expected);