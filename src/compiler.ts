import type { Env } from "./env.js";

type Compiler = {
  env: Env;
};

const _compile = (_c: Compiler) => {
  // Pass 1: Collect data declarations and add them to Data constructors
  // Pass 2: Collect fn signatures, add them to term_vars for recursion
  // Pass 3: Collect fns and data constructors, add them to term defs
  // pass 4: foreach term def and data constructor, compile a function
};
