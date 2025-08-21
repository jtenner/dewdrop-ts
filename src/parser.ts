import { chars } from "./chars.js";
import { lex, type Token } from "./lexer.js";
import { Yard } from "./yard.js";

type TokenIter = ReturnType<typeof lex>;

export type Module = {
  type: "module";
  declarations: Declaration[];
};

export type TypeExpression =
  | { named: TypeIdentifier }
  | { select: [TypeExpression, NameIdentifier] }
  | { application: [TypeExpression, TypeExpression[]] }
  | { fn: [TypeExpression[], TypeExpression] }
  | { record: [NameIdentifier, TypeExpression][] }
  | { tuple: TypeExpression[] };

export type Expression =
  | { name: string }
  | { constr: string }
  | { call: [Expression, Expression[]] }
  | { block: Expression[] }
  | { let_bind: [NameIdentifier, Expression] }
  | { if_expr: [Expression, Expression, Expression] }
  | { select: [Expression, NameIdentifier] }
  | { match: [Expression, MatchArm[]] }
  | { float: { value: number; size: number } }
  | { int: { value: bigint; size: number } }
  | { prefix: { op: string; operand: Expression } }
  | { binary: { op: string; left: Expression; right: Expression } }
  | { string: string }
  | {
      fn: {
        params: FnParam[];
        return_type: TypeExpression | null;
        body: Expression;
      };
    }
  | { record: [NameIdentifier, Expression][] }
  | { tuple: Expression[] };

export type MatchArm = {
  pattern: MatchExpression;
  guard: Expression | null;
  body: Expression;
};

export type MatchExpression =
  | { named: NameIdentifier }
  | { constructor: [TypeIdentifier, MatchExpression[]] }
  | { int: { value: bigint; size: number } }
  | { float: { value: number; size: number } }
  | { string: string }
  | { record: [NameIdentifier, MatchExpression][] }
  | { tuple: MatchExpression[] };

export type TypeIdentifier = { type_id: string };
export type NameIdentifier = { name: string };
export type Identifier = TypeIdentifier | NameIdentifier;

export type Import =
  | { ty: { name: TypeIdentifier; alias: TypeIdentifier | null } }
  | { fn: { name: NameIdentifier; alias: NameIdentifier | null } }
  | { name: { name: NameIdentifier; alias: NameIdentifier | null } }
  | { trait: { name: TypeIdentifier; alias: TypeIdentifier | null } };

export type TraitFn = {
  name: NameIdentifier;
  params: FnParam[];
  return_type: TypeExpression;
};

export type Fn = {
  name: NameIdentifier | null;
  params: FnParam[];
  return_type: TypeExpression | null;
  body: Expression;
};

export type FnParam = { name: NameIdentifier; guard: TypeExpression | null };

export type Declaration =
  | { import_dec: { path: string; name: TypeIdentifier; imports: Import[] } }
  | {
      fn: {
        pub: boolean;
        fn: Fn;
      };
    }
  | {
      extern: {
        pub: boolean;
        id: NameIdentifier;
        params: FnParam[];
        return_type: TypeExpression | null;
      };
    }
  | {
      type_dec: {
        pub: boolean;
        params: NameIdentifier[];
        value: TypeExpression;
      };
    }
  | { builtin: { pub: boolean; id: NameIdentifier[]; kind: string } }
  | {
      let_dec: {
        pub: boolean;
        id: NameIdentifier;
        guard: TypeExpression | null;
        value: Expression;
      };
    }
  | {
      trait: {
        pub: boolean;
        id: TypeIdentifier;
        type_params: NameIdentifier[];
        fns: TraitFn[];
      };
    }
  | {
      impl: {
        id: TypeIdentifier;
        type_params: TypeExpression[];
        for_type: TypeExpression;
        fn: Fn[];
      };
    };

const next = async (tokens: TokenIter, skip_whitespace = true) => {
  while (true) {
    const next_token = await tokens.next();
    if (next_token.done) return null;
    if (skip_whitespace && "whitespace" in next_token.value) continue;
    return next_token.value;
  }
};

const is_pub = (token: Token) => "keyword" in token && token.keyword === "pub";

type DeclarationResult = [Token | null, Declaration | null];

const operators = new Set(Array.from("!%^&*-=+/<>:.?~|"));

const is_operator = (token: Token): token is { symbol: string } => {
  return "symbol" in token && operators.has(token.symbol);
};

export const take_operator = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, string | null]> => {
  next_token ??= await next(tokens);
  if (!next_token) return [null, null];
  let acc = "";
  if (is_operator(next_token)) {
    acc += next_token.symbol;
  } else return [next_token, null];

  while (true) {
    next_token = await next(tokens, false);
    if (!next_token) return [null, acc];

    if (is_operator(next_token)) {
      acc = next_token.symbol;
      continue;
    }

    if ("whitespace" in next_token) next_token = await next(tokens);
    return [next_token, acc];
  }
};

export const take_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
) => {
  let combine_state = false;
  const yard = new Yard<Expression>();

  while (true) {
    next_token ??= await next(tokens);
    if (combine_state) {
      // looking for binary operators and right unarys
    } else {
      // when looking for a value, there must be a token
      if (!next_token) return [null, null];

      if ("name" in next_token) {
        // identifier
        yard.push_expr(next_token);
        combine_state = true;
        continue;
      }
      if ("type" in next_token) {
        // adt constructor
        yard.push_expr({ constr: next_token.type });
        combine_state = true;
        continue;
      }
      if ("int" in next_token) {
        // integer
        yard.push_expr({ int: { size: 32, value: next_token.int } });
        combine_state = true;
        continue;
      }
      if ("float" in next_token) {
        // float
        yard.push_expr({ float: { size: 64, value: next_token.float } });
        combine_state = true;
        continue;
      }
      if ("string" in next_token) {
        // string value
        yard.push_expr(next_token);
        combine_state = true;
        continue;
      }
      if ("symbol" in next_token) {
        if (next_token.symbol === "(") {
          // open group
          yard.push_group();
          continue;
        }

        let op: string | null = null;
        // left unary operator possible in this position
        [next_token, op] = await take_operator(next_token, tokens);
        if (!op) return [next_token, null];

        yard.push_op({
          prefix: [op, (operand) => ({ prefix: { op, operand } })],
        });
        continue;
      }
      if ("keyword" in next_token) {
        switch (next_token.keyword) {
          case "inf": {
            yard.push_expr({ float: { size: 64, value: Infinity } });
            combine_state = true;
            continue;
          }
          case "nan": {
            yard.push_expr({ float: { size: 64, value: NaN } });
            combine_state = true;
            continue;
          }
          default:
            return [next_token, null];
        }
      }

      return [next_token, null];
    }
  }
};

export const take_list = async <T>(
  next_token: Token | null,
  tokens: TokenIter,
  fn: (
    next_token: Token | null,
    tokens: TokenIter,
  ) => Promise<[Token | null, T | null]>,
  sep: string,
  term: string,
): Promise<[Token | null, T[] | null]> => {
  const results = [] as T[];
  let item: T | null = null;
  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  if ("symbol" in next_token && next_token.symbol === term)
    return [null, results];

  while (true) {
    // expect a parsed item
    [next_token, item] = await fn(next_token, tokens);

    if (!item) return [next_token, null];

    results.push(item);
    next_token ??= await next(tokens);

    if (!next_token) return [null, null];
    if ("symbol" in next_token && next_token.symbol === term)
      return [null, results];
    if ("symbol" in next_token && next_token.symbol === sep) continue;

    // parse failed, pass the token on
    return [next_token, null];
  }
};

export const take_fn_param = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, FnParam | null]> => {
  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  let name: NameIdentifier | null = null;

  if ("name" in next_token) {
    name = next_token;
  } else return [next_token, null];

  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  let guard: TypeExpression | null = null;
  if ("symbol" in next_token && next_token.symbol === ":") {
    [next_token, guard] = await take_type_expression(null, tokens);
    if (!guard) return [next_token, null];
  }

  return [next_token, { name, guard }];
};

export const take_fn = async (
  tokens: TokenIter,
): Promise<[Token | null, Fn | null]> => {
  let next_token: Token | null = await next(tokens);
  if (!next_token) return [null, null];
  let name = null as { name: string } | null;
  if ("name" in next_token) {
    name = next_token;

    next_token = await next(tokens);
  }

  if (!next_token) return [null, null];
  const start_params = "symbol" in next_token && next_token.symbol === "(";
  if (!start_params) return [null, null];

  let params: FnParam[] | null = null;

  [next_token, params] = await take_list(null, tokens, take_fn_param, ",", ")");
  if (!params) return [next_token, null];

  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  let return_type: TypeExpression | null = null;
  if ("symbol" in next_token && next_token.symbol === ":") {
    [next_token, return_type] = await take_type_expression(null, tokens);

    next_token = next_token ?? (await next(tokens));
    if (!next_token) return [null, null];
  }

  let body: Expression | null = null;
  [next_token, body] = await take_expression(next_token, tokens);

  if (!body) return [next_token, null];

  return [next_token, { body, name, params, return_type }];
};

const take_pub = async (tokens: TokenIter): Promise<DeclarationResult> => {
  const next_token = await next(tokens);
  if (!next_token) return [null, null];
  if ("keyword" in next_token) {
    if (next_token.keyword === "fn") {
      const [next_token, fn] = await take_fn(tokens);
      if (fn) {
        return [next_token, { fn: { pub: true, fn } }];
      }
      return [next_token, null];
    }

    if (next_token.keyword === "extern") {
      const [next_token, extern] = await take_extern_declaration(tokens);
      if (extern) {
        extern.pub = true;
        return [next_token, extern];
      }
      return [next_token, null];
    }

    if (next_token.keyword === "builtin") {
      const [next_token, builtin] = await take_builtin_declaration(tokens);
      if (builtin) {
        builtin.pub = true;
        return [next_token, builtin];
      }
      return [next_token, null];
    }

    if (next_token.keyword === "type") {
      const [next_token, type_dec] = await take_type_declaration(tokens);
      if (type_dec) {
        type_dec.pub = true;
        return [next_token, type_dec];
      }
      return [next_token, null];
    }

    if (next_token.keyword === "let") {
      const [next_token, let_dec] = await take_let_declaration(tokens);
      if (let_dec) {
        let_dec.pub = true;
        return [next_token, let_dec];
      }
      return [next_token, null];
    }

    if (next_token.keyword === "trait") {
      const [next_token, trait_dec] = await take_trait_declaration(tokens);
      if (trait_dec) {
        trait_dec.pub = true;
        return [next_token, trait_dec];
      }
      return [next_token, null];
    }
  }

  return [consume_until_keyword_or_semicolon(tokens), null];
};

export const parse = async (text: string) => {
  const iter = chars(text);
  const tokens = lex(iter, [
    "pub",
    "fn",
    "extern",
    "builtin",
    "type",
    "let",
    "trait",
    "inf",
    "nan",
  ]);
  const declarations = [] as Declaration[];
  let next_token: Token | null = null;
  let declaration: Declaration | null = null;

  while (true) {
    if (!next_token) {
      const result = await next(tokens);
      if (!result) return { module: declarations };
      next_token = result;
    }

    [next_token, declaration] = is_pub(next_token)
      ? await take_pub(tokens)
      : [null, null];
  }
};

export const parse_file = async (file: string) => {};
