import { take_declaration, take_expression, take_type_expression } from "./src/parser";
import { lex } from "./src/lexer.ts";
import { chars } from "./src/chars.ts";
const result = await take_declaration(null, lex(chars(`
  enum rec List<t_item> { Nil, Cons(t_item, List<t_item>) }
}`)));

console.log(result);