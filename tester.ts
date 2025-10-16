import { parse } from "./src/parser";
const result = await parse("pub trait Eq<t> { fn eq(other: t) -> Bool }");

console.log(result);