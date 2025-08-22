import type { ImportDeclaration } from "typescript";
import { chars } from "./chars.js";
import { lex, type Token } from "./lexer.js";
import { type Operator, Yard } from "./yard.js";

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

export type BodyExpression =
  | { arrow_bind: [NameIdentifier, Expression] }
  | { let_bind: [MatchExpression, Expression] }
  | { assign: [NameIdentifier, Expression] }
  | { expression: Expression };

export type Expression =
  | { name: string }
  | { constr: string }
  | { call: [Expression, Expression[]] }
  | { block: BodyExpression[] }
  | { let_bind: [NameIdentifier, Expression] }
  | { if_expr: [Expression, Expression, Expression] }
  | { select: [Expression, NameIdentifier] }
  | { match: [Expression, MatchArm[]] }
  | { float: { value: number; size: number } }
  | { int: { value: bigint; size: number } }
  | { prefix: { op: PrefixOp; operand: Expression } }
  | { postfix: { op: PostfixOp; operand: Expression } }
  | { infix: { op: InfixOp; left: Expression; right: Expression } }
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

export type TypeIdentifier = { type: string };
export type NameIdentifier = { name: string };
export type Identifier = TypeIdentifier | NameIdentifier;

type WasmName = { string: string };

export type FnSignature = {
  param_types: TypeExpression[];
  return_type: TypeExpression;
};

export type Int = { int: bigint };

export type Import =
  | { type: { name: TypeIdentifier; alias: TypeIdentifier | null } }
  | {
      fn: {
        name: NameIdentifier | WasmName;
        signature: FnSignature | null;
        alias: NameIdentifier | null;
      };
    }
  | {
      global: {
        mut: boolean;
        name: NameIdentifier | WasmName;
        global_type: TypeExpression;
        alias: NameIdentifier | null;
      };
    }
  | {
      table: {
        name: NameIdentifier | WasmName;
        table_type: TypeExpression;
        min: bigint | null;
        max: bigint | null;
        alias: NameIdentifier | null;
      };
    }
  | { star: TypeIdentifier }
  | {
      memory: {
        name: NameIdentifier | WasmName;
        min: bigint | null;
        max: bigint | null;
        alias: NameIdentifier | null;
      };
    }
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
  | {
      import_dec: {
        import_from: string;
        imports: Import[];
      };
    }
  | {
      fn: {
        pub: boolean;
        fn: Fn;
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

const infix_ops = {
  "**": [15, "right"] as const,
  ".**": [15, "right"] as const,
  "**.": [15, "right"] as const,
  "*": [14, "left"] as const,
  ".*": [14, "left"] as const,
  "*.": [14, "left"] as const,
  "/": [14, "left"] as const,
  "./": [14, "left"] as const,
  "/.": [14, "left"] as const,
  "%": [14, "left"] as const,
  ".%": [14, "left"] as const,
  "%.": [14, "left"] as const,
  "+": [13, "left"] as const,
  ".+": [13, "left"] as const,
  "+.": [13, "left"] as const,
  "-": [13, "left"] as const,
  ".-": [13, "left"] as const,
  "-.": [13, "left"] as const,
  "<>": [13, "left"] as const,
  ".<>": [13, "left"] as const,
  "<>.": [13, "left"] as const,
  "<<": [12, "left"] as const,
  ".<<": [12, "left"] as const,
  "<<.": [12, "left"] as const,
  ">>": [12, "left"] as const,
  ".>>": [12, "left"] as const,
  ">>.": [12, "left"] as const,
  "&": [11, "left"] as const,
  ".&": [11, "left"] as const,
  "&.": [11, "left"] as const,
  "^": [10, "left"] as const,
  ".^": [10, "left"] as const,
  "^.": [10, "left"] as const,
  "|": [9, "left"] as const,
  ".|": [9, "left"] as const,
  "|.": [9, "left"] as const,
  "<": [8, "left"] as const,
  ".<": [8, "left"] as const,
  "<.": [8, "left"] as const,
  ">": [8, "left"] as const,
  ".>": [8, "left"] as const,
  ">.": [8, "left"] as const,
  "<=": [8, "left"] as const,
  ".<=": [8, "left"] as const,
  "<=.": [8, "left"] as const,
  ">=": [8, "left"] as const,
  ".>=": [8, "left"] as const,
  ">=.": [8, "left"] as const,
  "==": [7, "left"] as const,
  ".==": [7, "left"] as const,
  "==.": [7, "left"] as const,
  "!=": [7, "left"] as const,
  ".!=": [7, "left"] as const,
  "!=.": [7, "left"] as const,
  "&&": [6, "left"] as const,
  ".&&": [6, "left"] as const,
  "&&.": [6, "left"] as const,
  "^^": [5, "left"] as const,
  ".^^": [5, "left"] as const,
  "^^.": [5, "left"] as const,
  "||": [4, "left"] as const,
  ".||": [4, "left"] as const,
  "||.": [4, "left"] as const,
  "??": [3, "right"] as const,
  ".??": [3, "right"] as const,
  "??.": [3, "right"] as const,
  "..": [2, "left"] as const,
  "|>": [1, "left"] as const,
} as const satisfies Record<string, [number, "left" | "right"]>;

const get_infix_op = (op: string): Operator<Expression> | null =>
  op in infix_ops
    ? ({
        infix: [
          op,
          infix_ops[op as keyof typeof infix_ops][0],
          infix_ops[op as keyof typeof infix_ops][1],
          (left: Expression, right: Expression) => ({
            infix: { op: op as InfixOp, left, right },
          }),
        ],
      } satisfies Operator<Expression>)
    : null;

const prefix_ops = ["!", "~", "-"] as const;
const postfix_ops = ["!", "?"] as const;

type PrefixOp = (typeof prefix_ops)[number];
type PostfixOp = (typeof postfix_ops)[number];
type InfixOp = keyof typeof infix_ops;

const get_prefix_op = (op: string): Operator<Expression> | null =>
  prefix_ops.includes(op as PrefixOp)
    ? {
        prefix: [
          op,
          (operand) => ({ prefix: { op: op as PrefixOp, operand } }),
        ],
      }
    : null;
const get_postfix_op = (op: string): Operator<Expression> | null =>
  postfix_ops.includes(op as PostfixOp)
    ? {
        prefix: [
          op,
          (operand) => ({ postfix: { op: op as PostfixOp, operand } }),
        ],
      }
    : null;

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

// {
//   let a = 10;
//   bound <- Expression;
//   variable "="() Expression;
//   Expression
// }

export const take_body_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, BodyExpression | null]> => {
  let expression: Expression | null = null;
  let match_expression: MatchExpression | null = null;
  let op: string | null = null;

  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  if ("name" in next_token) {
    const name = next_token.name;

    next_token = await next(tokens);
    // if nothing else is parsed, it's an expression of type "name"
    if (!next_token) return [null, { expression: { name } }];

    [next_token, op] = await take_operator(next_token, tokens);
    if (!op) {
      const yard = new Yard<Expression>();
      yard.push_expr({ name });

      [next_token, expression] = await take_expression(
        next_token,
        tokens,
        yard,
        true,
      );

      if (expression) return [next_token, { expression }];
      return [next_token, null];
    }

    if (op === "<-") {
      // arrow bind!
      [next_token, expression] = await take_expression(next_token, tokens);

      if (expression)
        return [next_token, { arrow_bind: [{ name }, expression] }];
    }

    if (op.startsWith("=")) {
      // assignment
      const next_op = op.slice(1);
      if (next_op === "") {
        // simple assignment
        [next_token, expression] = await take_expression(next_token, tokens);

        if (expression) return [next_token, { assign: [{ name }, expression] }];
      }

      if (next_op in infix_ops) {
        [next_token, expression] = await take_expression(next_token, tokens);
        if (expression) {
          const assign_expr = {
            infix: {
              left: { name },
              op: next_op as InfixOp,
              right: expression,
            },
          } satisfies Expression;
          const out = {
            assign: [{ name }, assign_expr],
          } satisfies BodyExpression;
          return [next_token, out];
        }
      }
    }

    const infix_op = get_infix_op(op);
    if (infix_op) {
      const yard = new Yard<Expression>();
      yard.push_expr({ name });
      yard.push_op(infix_op);
      [next_token, expression] = await take_expression(
        next_token,
        tokens,
        yard,
        false,
      );

      if (expression) return [next_token, { expression }];
      return [next_token, null];
    }

    const postfix_op = get_postfix_op(op);
    if (postfix_op) {
      const yard = new Yard<Expression>();
      yard.push_expr({ name });
      yard.push_op(postfix_op);
      [next_token, expression] = await take_expression(
        next_token,
        tokens,
        yard,
        true,
      );
      if (expression) return [next_token, { expression }];
      return [next_token, null];
    }

    return [next_token, null];
  }

  if ("keyword" in next_token && next_token.keyword === "let") {
    next_token = await next(tokens);
    if (!next_token) return [null, null];

    [next_token, match_expression] = await take_match_expression(
      next_token,
      tokens,
    );
    if (match_expression) {
      next_token ??= await next(tokens);
      if (!next_token) return [null, null];

      if ("symbol" in next_token && next_token.symbol === "=") {
        [next_token, expression] = await take_expression(null, tokens);
        if (expression)
          return [next_token, { let_bind: [match_expression, expression] }];
      }
    }
    return [next_token, null];
  }

  [next_token, expression] = await take_expression(next_token, tokens);
  if (expression) return [next_token, { expression }];

  return [next_token, null];
};

export const take_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
  yard = new Yard<Expression>(),
  combine_state = false,
): Promise<[Token | null, Expression | null]> => {
  while (true) {
    next_token ??= await next(tokens);
    if (combine_state) {
      // no more tokens means the expression can be done
      if (!next_token) return [null, yard.finalize()];

      if ("symbol" in next_token) {
        if (next_token.symbol === "(") {
          // function calls are a postfix expressions
          // consume the "("
          const [next_token, params] = await take_list(
            null,
            tokens,
            take_expression,
            ",",
            ")",
          );
          if (params) {
            yard.push_op({
              postfix: [
                "call",
                (callee) => ({
                  call: [callee, params],
                }),
              ],
            });
            combine_state = true;
            continue;
          }

          return [next_token, null];
        }

        if (next_token.symbol === ")" && yard.is_nested) {
          yard.pop_group();
          combine_state = true;
          continue;
        }
        let op: string | null = null;

        [next_token, op] = await take_operator(next_token, tokens);

        // if an operator is taken, it must be infix or postfix in this position
        if (op) {
          // property access
          if (op === ".") {
            // next_token was mutated, fill it
            next_token ??= await next(tokens);
            if (!next_token) return [null, null];

            if ("name" in next_token) {
              const name = next_token.name;
              yard.push_op({
                postfix: ["select", (expr) => ({ select: [expr, { name }] })],
              });
              combine_state = true;
              continue;
            }

            return [next_token, null];
          }

          const infix_op = get_infix_op(op);
          if (infix_op) {
            yard.push_op(infix_op);
            combine_state = false;
            continue;
          }

          const postfix_op = get_postfix_op(op);
          if (postfix_op) {
            yard.push_op(postfix_op);
            combine_state = true;
            continue;
          }

          // a valid operator token that isn't supported is an error
          return [next_token, null];
        }
        // without a valid operator, it might just be a seperator, which is still valid
      }

      return [next_token, yard.finalize()];
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
        // "(" in the "value" position is an expression grouping
        if (next_token.symbol === "(") {
          yard.push_group();
          combine_state = false;
          continue;
        }

        if (next_token.symbol === "{") {
          // consume the token
          let body_exprs: BodyExpression[] | null = null;
          [next_token, body_exprs] = await take_list(
            null,
            tokens,
            take_body_expression,
            ";",
            "}",
          );

          if (body_exprs) {
            yard.push_expr({ block: body_exprs });
            combine_state = true;
            continue;
          }

          return [next_token, null];
        }

        let op: string | null = null;
        // left unary operator possible in this position
        [next_token, op] = await take_operator(next_token, tokens);
        if (!op) return [next_token, null];

        const op_def = get_prefix_op(op);
        if (!op_def) return [next_token, null];

        yard.push_op(op_def);
        combine_state = false;
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

export const take_import = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, Import | null]> => {
  let type_expression: TypeExpression | null = null;

  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  // TODO: fn keyword
  if ("keyword" in next_token && next_token.keyword === "fn") {
    return [next_token, null];
  }

  // TODO: memory
  if ("keyword" in next_token && next_token.keyword === "memory") {
    return [next_token, null];
  }

  // TODO: global
  if ("keyword" in next_token && next_token.keyword === "global") {
    return [next_token, null];
  }

  if ("keyword" in next_token && next_token.keyword === "table") {
    let min: bigint | null = 0n;
    let max: bigint | null = 0n;
    next_token = await next(tokens);
    if (!next_token) return [null, null];
    const name =
      "string" in next_token || "name" in next_token ? next_token : null;
    if (!name) return [next_token, null];

    next_token = await next(tokens);
    if (!next_token || !("symbol" in next_token && next_token.symbol === ":"))
      return [next_token, null];

    [next_token, type_expression] = await take_type_expression(null, tokens);
    if (!type_expression) return [next_token, null];

    next_token ??= await next(tokens);
    if (next_token && "int" in next_token) {
      min = next_token.int;
      next_token = await next(tokens);
    }

    if (next_token && "int" in next_token) {
      max = next_token.int;
      next_token = await next(tokens);
    }

    if (next_token && "keyword" in next_token && next_token.keyword === "as") {
      next_token = await next(tokens);
      if (next_token && "name" in next_token)
        return [
          null,
          {
            table: {
              name,
              min,
              max,
              table_type: type_expression,
              alias: { name: next_token.name },
            },
          },
        ];
      return [next_token, null];
    }

    return [
      next_token,
      {
        table: { name, min, max, table_type: type_expression, alias: null },
      },
    ];
  }

  if ("keyword" in next_token && next_token.keyword === "type") {
    next_token = await next(tokens);
    if (!next_token) return [null, null];
    if (!("type" in next_token)) return [next_token, null];
    const type = next_token.type;

    // optional as
    next_token = await next(tokens);
    if (!next_token)
      return [next_token, { type: { name: { type }, alias: null } }];

    if ("keyword" in next_token && next_token.keyword === "as") {
      next_token = await next(tokens);
      if (next_token && "type" in next_token)
        return [
          null,
          { type: { name: { type }, alias: { type: next_token.type } } },
        ];
      return [next_token, null];
    }

    return [next_token, { type: { name: { type }, alias: null } }];
  }

  if ("keyword" in next_token && next_token.keyword === "trait") {
    next_token = await next(tokens);
    if (!next_token) return [null, null];
    if (!("type" in next_token)) return [next_token, null];
    const type = next_token.type;

    // optional as
    next_token = await next(tokens);
    if (!next_token)
      return [next_token, { trait: { name: { type }, alias: null } }];

    if ("keyword" in next_token && next_token.keyword === "as") {
      next_token = await next(tokens);
      if (next_token && "type" in next_token)
        return [
          null,
          { trait: { name: { type }, alias: { type: next_token.type } } },
        ];
      return [next_token, null];
    }

    return [next_token, { trait: { name: { type }, alias: null } }];
  }

  if ("name" in next_token) {
    const name = { name: next_token.name };
    // optional as
    next_token = await next(tokens);
    if (!next_token) return [next_token, { name: { name, alias: null } }];

    if ("keyword" in next_token && next_token.keyword === "as") {
      next_token = await next(tokens);
      if (next_token && "name" in next_token)
        return [null, { name: { name, alias: { name: next_token.name } } }];
      return [next_token, null];
    }

    return [next_token, { name: { name, alias: null } }];
  }

  return [next_token, null];
};

export const take_import_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, Declaration | null]> => {
  // "import" String "{" List(ImportDesc, ",", "}")
  next_token ??= await next(tokens);
  if (!next_token) return [null, null];

  // import keyword already found
  // if (!("keyword" in next_token && next_token.keyword === "import"))
  //   return [next_token, null];

  next_token = await next(tokens);
  if (!next_token || !("string" in next_token)) return [next_token, null];
  const import_from = next_token.string;

  next_token = await next(tokens);
  if (!next_token || !("symbol" in next_token && next_token.symbol === "{"))
    return [next_token, null];

  let imports: Import[] | null = null;
  [next_token, imports] = await take_list(null, tokens, take_import, ",", "}");

  if (imports) return [next_token, { import_dec: { import_from, imports } }];
  return [next_token, null];
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
    "builtin",
    "type",
    "let",
    "trait",
    "inf",
    "nan",
    "import",
    "memory",
    "global",
    "as",
  ]);
  const declarations = [] as Declaration[];
  let next_token: Token | null = null;
  let _declaration: Declaration | null = null;

  while (true) {
    if (!next_token) {
      const result = await next(tokens);
      if (!result) return { module: declarations };
      next_token = result;
    }

    [next_token, _declaration] = is_pub(next_token)
      ? await take_pub(tokens)
      : [null, null];
  }
};

export const parse_file = async (_file: string) => {};
