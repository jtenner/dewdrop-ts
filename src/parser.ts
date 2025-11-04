import { chars, chars_from } from "./chars.js";
import {
  type FloatToken,
  type IntToken,
  lex,
  type NameToken,
  type StringToken,
  type SymbolToken,
  type Token,
  type TypeToken,
} from "./lexer.js";
import type { Result } from "./util.js";
import { type Operator, Yard } from "./yard.js";

type TokenIter = ReturnType<typeof lex>;

// AST Types
export type Module = {
  module: Declaration[];
};

export type ExpectedParseError = { expected: { kind: string; found: Token } };

export type ParseResult<T> = [Token | null, Result<ParseError, T>];
export type ParseError = ExpectedParseError;

export const ok = <T>(ok: T) => ({ ok });

export const expected = (kind: string, found: Token | null) =>
  ({
    err: { expected: { kind, found: found ?? { unknown: null } } },
  }) satisfies Result<ExpectedParseError, Token>;

export type NamedTypeExpression = { name: NameIdentifier; ty: TypeExpression };

export type NameTypeExpression = { name: string };
export type SelectTypeExpression = {
  select: { root: TypeExpression; name: Identifier };
};
export type ApplicationTypeExpression = {
  app: { callee: TypeExpression; args: TypeExpression[] };
};
export type FnTypeExpression = {
  fn: { args: TypeExpression[]; ret: TypeExpression };
};
export type RecordTypeExpression = { record: NamedTypeExpression[] };
export type TupleTypeExpression = { tuple: TypeExpression[] };
export type TypeExpression =
  | NameIdentifier
  | TypeIdentifier
  | SelectTypeExpression
  | ApplicationTypeExpression
  | FnTypeExpression
  | RecordTypeExpression
  | TupleTypeExpression;

export type ArrowKindBodyExpression = {
  arrow_bind: { name: NameIdentifier; expression: Expression };
};
export type LetBindBodyExpression = {
  let_bind: {
    pattern: PatternExpression;
    assert: boolean;
    expression: Expression;
  };
};
export type AssignBodyExpression = {
  assign: { name: NameIdentifier; expression: Expression };
};
export type ExpressionBodyExpression = { expression: Expression };
export type BodyExpression =
  | ArrowKindBodyExpression // a <- expr
  | LetBindBodyExpression // let a = expr
  | AssignBodyExpression
  | ExpressionBodyExpression; // expr

export type TypeConstructorExpression = TypeIdentifier;
export type CallExpression = { call: [Expression, Expression[]] };
export type BlockExpression = { block: BodyExpression[] };
export type IfExpression = {
  if_expr: {
    cond: Expression;
    if_body: Expression;
    else_body: Expression | null;
  };
};
export type SelectExpression = { select: [Expression, NameIdentifier] };
export type MatchExpression = { match: [Expression, MatchArm[]] };
export type FloatExpression = { float: { value: number; size: number } };
export type IntExpression = { int: { value: bigint; size: number } };
export type BoolExpression = { bool: boolean };
export type PrefixExpression = {
  prefix: { op: PrefixOp; operand: Expression };
};
export type PostfixExpression = {
  postfix: { op: PostfixOp; operand: Expression };
};
export type InfixExpression = {
  infix: { op: InfixOp; left: Expression; right: Expression };
};
export type FnExpression = {
  fn: Fn;
};
export type RecordExpression = { record: [NameIdentifier, Expression][] };
export type TupleExpression = { tuple: Expression[] };
export type SelfExpression = { self: null };
export type Expression =
  | NameIdentifier
  | TypeConstructorExpression
  | CallExpression
  | BlockExpression
  | IfExpression
  | SelectExpression // record.field
  | MatchExpression // match (val) { ... }
  | FloatExpression
  | IntExpression
  | BoolExpression
  | PrefixExpression // (op) expression
  | PostfixExpression // expression (op)
  | InfixExpression // left (op) right
  | StringToken
  | FnExpression
  | RecordExpression
  | TupleExpression
  | SelfExpression;

export type MatchArm = {
  pattern: PatternExpression;
  guard: Expression | null;
  body: Expression;
};

const isName = (expr: Expression | TypeExpression | PatternExpression) =>
  "name" in expr;
const isType = (expr: Expression | TypeExpression) => "type" in expr;
const isString = (expr: Expression) => "string" in expr;

export type ConstructorPatternExpression = {
  constr: { type: TypeIdentifier; patterns: PatternExpression[] };
};
export type IntPatternExpression = { int: { value: bigint; size: number } };
export type FloatPatternExpression = { float: { value: number; size: number } };
export type StringPatternExpression = { string: string };
export type RecordPatternExpression = {
  record: [NameIdentifier, PatternExpression][];
};
export type TuplePatternExpression = { tuple: PatternExpression[] };
export type PatternExpression =
  | NameIdentifier
  | ConstructorPatternExpression
  | IntPatternExpression
  | FloatPatternExpression
  | StringPatternExpression
  | RecordPatternExpression
  | TuplePatternExpression;

export type TypeIdentifier = TypeToken;
export type NameIdentifier = NameToken;
export type Identifier = TypeIdentifier | NameIdentifier;

export type WasmName = StringToken | NameToken;

export type FnSignature = {
  param_types: FnParam[];
  return_type: TypeExpression;
};

export type Int = { int: bigint };
export type TypeImport = {
  type: { name: TypeIdentifier; alias: TypeIdentifier | null };
};
export type FnImport = {
  fn: {
    name: NameIdentifier | WasmName;
    signature: FnSignature;
    alias: NameIdentifier | null;
  };
};
export type GlobalImport = {
  global: {
    mut: boolean;
    name: NameIdentifier | WasmName;
    global_type: TypeExpression;
    alias: NameIdentifier | null;
  };
};
export type TableImport = {
  table: {
    name: NameIdentifier | WasmName;
    table_type: TypeExpression;
    min: IntToken | null;
    max: IntToken | null;
    alias: NameIdentifier | null;
  };
};
export type StarImport = { star: TypeIdentifier };
export type MemoryImport = {
  memory: {
    name: NameIdentifier | WasmName;
    min: IntToken | null;
    max: IntToken | null;
    alias: NameIdentifier | null;
  };
};
export type NameImport = {
  name: { name: NameIdentifier; alias: NameIdentifier | null };
};
export type TraitImport = {
  trait: { name: TypeIdentifier; alias: TypeIdentifier | null };
};
export type EnumImport = {
  enum: { name: TypeIdentifier; alias: TypeIdentifier | null };
};
export type ConstructorImport = {
  constr: { name: TypeIdentifier; alias: TypeIdentifier | null };
};
export type Import =
  | ConstructorImport
  | EnumImport
  | FnImport
  | GlobalImport
  | MemoryImport
  | NameImport
  | StarImport
  | TableImport
  | TraitImport
  | TypeImport;

export type TraitFn = {
  name: NameIdentifier;
  params: NamedTypeExpression[];
  type_params: NameIdentifier[];
  return_type: TypeExpression;
};

export type Fn = {
  name: NameIdentifier | null;
  type_params: NameIdentifier[];
  params: FnParam[];
  return_type: TypeExpression | null;
  body: Expression;
};

export type FnParam = { name: NameIdentifier; guard: TypeExpression | null };

export type TypeDeclaration = {
  type_dec: {
    pub: boolean;
    id: TypeIdentifier;
    params: NameIdentifier[];
    value: TypeExpression;
  };
};

export type LetDeclaration = {
  let_dec: {
    pub: boolean;
    id: NameIdentifier;
    guard: TypeExpression | null;
    value: Expression;
  };
};

export type TraitDeclaration = {
  trait: {
    pub: boolean;
    id: TypeIdentifier;
    type_params: NameIdentifier[];
    fns: TraitFn[];
  };
};

export type ImplDeclaration = {
  impl: {
    name: TypeIdentifier;
    type_params: TypeExpression[];
    for: TypeExpression;
    fns: Fn[];
  };
};

export type ImportDeclaration = {
  import_dec: {
    import_from: string;
    imports: Import[];
  };
};

export type EnumVariant =
  | {
      fields: {
        id: TypeIdentifier;
        fields: NamedTypeExpression[];
      };
    }
  | { values: { id: TypeIdentifier; values: TypeExpression[] } };

export type EnumDeclaration = {
  enum: {
    pub: boolean;
    id: TypeIdentifier;
    type_params: NameIdentifier[];
    variants: EnumVariant[];
  };
};

export type FnDeclaration = { fn: { pub: boolean; fn: Fn } };
export type BuiltinDeclaration = {
  builtin: {
    name: StringToken;
    alias: NameIdentifier;
    params: FnParam[];
    return_type: TypeExpression;
  };
};
export type Declaration =
  | BuiltinDeclaration
  | FnDeclaration
  | EnumDeclaration
  | ImportDeclaration
  | TypeDeclaration
  | LetDeclaration
  | TraitDeclaration
  | ImplDeclaration;

const next = async (tokens: TokenIter, skip_whitespace = true) => {
  while (true) {
    const next_token = await tokens.next();
    if (next_token.done) return null;
    if (skip_whitespace && "whitespace" in next_token.value) continue;
    return next_token.value;
  }
};

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

const prefix_ops = ["!", ".!", "!.", "~", ".~", "~.", "-", ".-", "-."] as const;
const postfix_ops = ["!", "?", ".!", "!.", ".?", "?."] as const;

export type PrefixOp = (typeof prefix_ops)[number];
export type PostfixOp = (typeof postfix_ops)[number];
export type InfixOp = keyof typeof infix_ops;

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
        postfix: [
          op,
          (operand) => ({ postfix: { op: op as PostfixOp, operand } }),
        ],
      }
    : null;

export const take_key_value_pair_type_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<NamedTypeExpression>> => {
  let name: Result<ParseError, NameToken>;
  let success_token: Result<ParseError, unknown>;
  let ty: Result<ParseError, TypeExpression>;

  [next_token, name] = await take_name(next_token, tokens);
  if ("err" in name) return [next_token, name];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, ty] = await take_type_expression(next_token, tokens);
  if ("err" in ty) return [next_token, expected("type expression", next_token)];

  return [next_token, ok({ name: name.ok, ty: ty.ok })];
};

export const take_type_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<TypeExpression>> => {
  let name: Result<ParseError, TypeExpression>;
  let acc: Result<ParseError, TypeExpression> | null = null;
  let success_token: Result<ParseError, Token>;
  [next_token, name] = await take_name(next_token, tokens);
  if ("ok" in name) return [next_token, name];

  // function type `(...types) -> TypeExpression`
  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if ("ok" in success_token) {
    let args: Result<ParseError, TypeExpression[]>;
    [next_token, args] = await take_list(
      next_token,
      tokens,
      take_type_expression,
      ",",
      ")",
    );
    if ("err" in args) return [next_token, args];

    [next_token, success_token] = await take_symbol(next_token, tokens, "=");
    if ("err" in success_token) return [next_token, success_token];

    next_token ??= await next(tokens, false);
    if (next_token && "symbol" in next_token && next_token.symbol === ">") {
      let ret: Result<ParseError, TypeExpression>;
      [next_token, ret] = await take_type_expression(null, tokens);
      if ("ok" in ret)
        return [next_token, ok({ fn: { args: args.ok, ret: ret.ok } })];
    }

    const actual = next_token;
    while (next_token && "whitespace" in next_token)
      next_token = await next(tokens);
    return [next_token, expected("->", actual)];
  }

  [next_token, success_token] = await take_symbol(next_token, tokens, "#");
  if (success_token) {
    next_token ??= await next(tokens, false);
    // record types can also be chained
    if (next_token && "symbol" in next_token && next_token.symbol === "{") {
      let record: Result<ParseError, NamedTypeExpression[]>;
      [next_token, record] = await take_list(
        null,
        tokens,
        take_key_value_pair_type_expression,
        ",",
        "}",
      );
      if ("err" in record) return [next_token, record];
      acc = ok({ record: record.ok });
    } else if (
      next_token &&
      "symbol" in next_token &&
      next_token.symbol === "("
    ) {
      // tuple type cannot be chained
      let tuple: Result<ParseError, TypeExpression[]>;
      [next_token, tuple] = await take_list(
        null,
        tokens,
        take_type_expression,
        ",",
        ")",
      );

      if ("err" in tuple) return [next_token, tuple];
      return [next_token, ok({ tuple: tuple.ok })];
    }
  }

  // Named Expression
  if (!acc || "err" in acc)
    [next_token, acc] = await take_type(next_token, tokens);

  while (true) {
    if (!acc || "err" in acc) return [next_token, acc!];

    // Select type/name
    [next_token, success_token] = await take_symbol(next_token, tokens, ".");
    if ("ok" in success_token) {
      let name: Result<ParseError, Identifier>;
      [next_token, name] = await take_name(next_token, tokens);
      if ("err" in name)
        [next_token, name] = await take_type(next_token, tokens);
      if ("err" in name) return [next_token, name];

      acc = ok({ select: { root: acc!.ok, name: name.ok } });
      continue;
    }

    // Type Application
    [next_token, success_token] = await take_symbol(next_token, tokens, "<");
    if ("ok" in success_token) {
      let args: Result<ParseError, TypeExpression[]>;
      [next_token, args] = await take_list(
        next_token,
        tokens,
        take_type_expression,
        ",",
        ">",
      );

      if ("err" in args) return [next_token, args];

      acc = ok({ app: { callee: acc.ok, args: args.ok } });
      continue;
    }

    return [next_token, acc];
  }
};

export const take_record_pattern_arm = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<[NameIdentifier, PatternExpression]>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, NameIdentifier>;
  let pattern: Result<ParseError, PatternExpression>;

  [next_token, name] = await take_name(next_token, tokens);
  if ("err" in name) return [next_token, name];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("ok" in success_token) {
    [next_token, pattern] = await take_pattern_expression(next_token, tokens);
    if ("ok" in pattern) return [next_token, ok([name.ok, pattern.ok])];
    return [next_token, pattern];
  } else return [next_token, ok([name.ok, name.ok])];
};

export const take_pattern_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<PatternExpression>> => {
  let name: Result<ParseError, NameIdentifier>;
  let type: Result<ParseError, TypeIdentifier>;
  let success_token: Result<ParseError, Token>;
  let int: Result<ParseError, IntToken>;
  let float: Result<ParseError, FloatToken>;
  let string: Result<ParseError, StringToken>;
  let patterns: Result<ParseError, PatternExpression[]>;

  [next_token, name] = await take_name(next_token, tokens);
  if ("ok" in name) return [next_token, name];

  [next_token, type] = await take_type(next_token, tokens);
  if ("ok" in type) {
    [next_token, success_token] = await take_symbol(next_token, tokens, "(");

    if ("ok" in success_token) {
      [next_token, patterns] = await take_list(
        next_token,
        tokens,
        take_pattern_expression,
        ",",
        ")",
      );
      if ("err" in patterns) return [next_token, patterns];
      return [
        next_token,
        ok({ constr: { type: type.ok, patterns: patterns.ok } }),
      ];
    } else return [next_token, ok({ constr: { type: type.ok, patterns: [] } })];
  }

  [next_token, int] = await take_int(next_token, tokens);
  if ("ok" in int)
    return [next_token, ok({ int: { size: 32, value: int.ok.int } })];

  [next_token, float] = await take_float(next_token, tokens);
  if ("ok" in float)
    return [next_token, ok({ float: { size: 64, value: float.ok.float } })];

  [next_token, string] = await take_string(next_token, tokens);
  if ("ok" in string) return [next_token, string];

  [next_token, success_token] = await take_keyword(next_token, tokens, "inf");
  if ("ok" in success_token)
    return [next_token, ok({ float: { size: 64, value: Infinity } })];

  [next_token, success_token] = await take_keyword(next_token, tokens, "nan");
  if ("ok" in success_token)
    return [next_token, ok({ float: { size: 64, value: NaN } })];

  [next_token, success_token] = await take_symbol(next_token, tokens, "#");
  if ("ok" in success_token) {
    next_token ??= await next(tokens, false);
    if (next_token && "symbol" in next_token && next_token.symbol === "(") {
      [next_token, patterns] = await take_list(
        null,
        tokens,
        take_pattern_expression,
        ",",
        ")",
      );
      if ("ok" in patterns) return [next_token, ok({ tuple: patterns.ok })];
      return [next_token, patterns];
    }

    if (next_token && "symbol" in next_token && next_token.symbol === "{") {
      let record: Result<ParseError, [NameIdentifier, PatternExpression][]>;
      [next_token, record] = await take_list(
        null,
        tokens,
        take_record_pattern_arm,
        ",",
        "}",
      );
      if ("ok" in record) return [next_token, ok({ record: record.ok })];
      return [next_token, record];
    }
    const found = next_token;
    if (next_token && "whitespace" in next_token)
      next_token = await next(tokens);
    return [next_token, expected('symbol("{")', found)];
  }

  return [next_token, expected("symbol(#)", next_token!)];
};

export const take_operator = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<string>> => {
  next_token ??= await next(tokens);
  if (!next_token) return [null, expected("token", null)];
  let acc = "";
  if (is_operator(next_token)) {
    acc += next_token.symbol;
  } else return [next_token, expected("operator", next_token)];

  while (true) {
    next_token = await next(tokens, false);
    if (!next_token) return [null, ok(acc)];

    if (is_operator(next_token)) {
      acc += next_token.symbol;
      continue;
    }

    while (next_token && "whitespace" in next_token)
      next_token = await next(tokens);
    return [next_token, ok(acc)];
  }
};

export const take_body_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<BodyExpression>> => {
  let success_token: Result<ParseError, Token>;
  let expression: Result<ParseError, Expression>;
  let pattern: Result<ParseError, PatternExpression>;
  let name: Result<ParseError, NameIdentifier>;
  let op: Result<ParseError, string>;

  [next_token, name] = await take_name(next_token, tokens);
  if ("ok" in name) {
    // name
    next_token = await next(tokens);
    // if nothing else is parsed, it's an expression of type "name"
    if (!next_token) return [null, ok({ expression: name.ok })];

    [next_token, op] = await take_operator(next_token, tokens);
    if ("err" in op) {
      const yard = new Yard<Expression>();
      yard.push_expr(name.ok);

      [next_token, expression] = await take_expression(
        next_token,
        tokens,
        yard,
        true,
      );

      if ("ok" in expression)
        return [next_token, ok({ expression: expression.ok })];
      return [next_token, expected("expression", next_token)];
    }

    if (op.ok === "<-") {
      // arrow bind!
      [next_token, expression] = await take_expression(next_token, tokens);

      if ("ok" in expression)
        return [
          next_token,
          ok({ arrow_bind: { name: name.ok, expression: expression.ok } }),
        ];
    }

    if (op.ok.startsWith("=")) {
      // assignment
      const next_op = op.ok.slice(1);
      if (next_op === "") {
        // simple assignment
        [next_token, expression] = await take_expression(next_token, tokens);

        if ("ok" in expression)
          return [
            next_token,
            ok({ assign: { name: name.ok, expression: expression.ok } }),
          ];
      }

      if (next_op in infix_ops) {
        [next_token, expression] = await take_expression(next_token, tokens);
        if ("ok" in expression) {
          expression = ok({
            infix: {
              left: name.ok,
              op: next_op as InfixOp,
              right: expression.ok,
            },
          } satisfies Expression);

          return [
            next_token,
            ok({
              assign: { name: name.ok, expression: expression.ok },
            } satisfies BodyExpression),
          ];
        }
      }
    }

    const infix_op = get_infix_op(op.ok);
    if (infix_op) {
      const yard = new Yard<Expression>();
      yard.push_expr(name.ok);
      yard.push_op(infix_op);
      [next_token, expression] = await take_expression(
        next_token,
        tokens,
        yard,
        false,
      );

      if ("ok" in expression)
        return [next_token, ok({ expression: expression.ok })];
      return [next_token, expression];
    }

    const postfix_op = get_postfix_op(op.ok);
    if (postfix_op) {
      const yard = new Yard<Expression>();
      yard.push_expr(name.ok);
      yard.push_op(postfix_op);
      [next_token, expression] = await take_expression(
        next_token,
        tokens,
        yard,
        true,
      );
      if ("ok" in expression)
        return [next_token, ok({ expression: expression.ok })];
      return [next_token, expression];
    }

    return [next_token, expected("expression", next_token)];
  }

  [next_token, success_token] = await take_keyword(next_token, tokens, "let");
  if ("ok" in success_token) {
    [next_token, success_token] = await take_keyword(
      next_token,
      tokens,
      "assert",
    );
    const assert = "ok" in success_token;
    [next_token, pattern] = await take_pattern_expression(next_token, tokens);
    if ("ok" in pattern) {
      [next_token, success_token] = await take_symbol(next_token, tokens, "=");
      if ("ok" in success_token) {
        [next_token, expression] = await take_expression(null, tokens);
        if ("ok" in expression)
          return [
            next_token,
            ok({
              let_bind: {
                pattern: pattern.ok,
                assert,
                expression: expression.ok,
              },
            }),
          ];
      }
    }
    return [next_token, expected("body expression", next_token)];
  }

  [next_token, expression] = await take_expression(next_token, tokens);
  if ("ok" in expression)
    return [next_token, ok({ expression: expression.ok })];

  return [next_token, expression];
};

export const take_key_value_pair = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<[NameIdentifier, Expression]>> => {
  let key: Result<ParseError, NameIdentifier>;
  let value: Result<ParseError, Expression>;
  let token_success: Result<ParseError, Token>;

  [next_token, key] = await take_name(next_token, tokens);
  if ("err" in key) return [next_token, key];

  [next_token, token_success] = await take_symbol(next_token, tokens, ":");
  if ("ok" in token_success) {
    [next_token, value] = await take_expression(next_token, tokens);
    if ("ok" in value) return [next_token, ok([key.ok, value.ok])];
    return [next_token, value];
  } else {
    return [next_token, ok([key.ok, key.ok])];
  }
};

const take_match_arm = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<MatchArm>> => {
  let token_success: Result<ParseError, Token>;
  let op: Result<ParseError, string>;
  let pattern: Result<ParseError, PatternExpression>;
  let guard: Result<ParseError, Expression> | null = null;
  let body: Result<ParseError, Expression>;

  [next_token, pattern] = await take_pattern_expression(next_token, tokens);
  if ("err" in pattern) return [next_token, pattern];

  [next_token, token_success] = await take_keyword(next_token, tokens, "if");
  if ("ok" in token_success) {
    [next_token, token_success] = await take_symbol(next_token, tokens, "(");
    if ("err" in token_success) return [next_token, token_success];

    [next_token, guard] = await take_expression(next_token, tokens);
    if ("err" in guard) return [next_token, guard];

    [next_token, token_success] = await take_symbol(next_token, tokens, ")");
    if ("err" in token_success) return [next_token, token_success];
  }

  // we must find `=>`
  [next_token, op] = await take_operator(next_token, tokens);
  if ("ok" in op && op.ok === "=>") {
    [next_token, body] = await take_expression(next_token, tokens);
    if ("err" in body) return [next_token, body];
    return [
      next_token,
      ok({ body: body.ok, guard: guard?.ok ?? null, pattern: pattern.ok }),
    ];
  } else if ("err" in op) return [next_token, op];
  else return [next_token, expected("symbol(=>)", next_token)];
};

export const take_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
  yard = new Yard<Expression>(),
  combine_state = false,
): Promise<ParseResult<Expression>> => {
  let name: Result<ParseError, NameIdentifier>;
  let type: Result<ParseError, TypeIdentifier>;
  let success_token: Result<ParseError, Token>;
  let op: Result<ParseError, string>;
  let int: Result<ParseError, IntToken>;
  let float: Result<ParseError, FloatToken>;
  let string: Result<ParseError, StringToken>;

  // match
  // prefix
  // fn
  while (true) {
    if (combine_state) {
      // function call
      [next_token, success_token] = await take_symbol(next_token, tokens, "(");
      if ("ok" in success_token) {
        let args: Result<ParseError, Expression[]>;
        [next_token, args] = await take_list(
          next_token,
          tokens,
          take_expression,
          ",",
          ")",
        );
        if ("err" in args) return [next_token, args];
        const value = args.ok;
        yard.push_op({
          postfix: ["call", (expr) => ({ call: [expr, value] })],
        });
        combine_state = true;
        continue;
      }

      // end groups if nested, continue combining
      if (yard.nested) {
        [next_token, success_token] = await take_symbol(
          next_token,
          tokens,
          ")",
        );
        if (success_token) {
          yard.pop_group();
          combine_state = true;
          continue;
        }
      }

      [next_token, op] = await take_operator(next_token, tokens);
      if ("err" in op) {
        const expr = yard.finalize();
        return [
          next_token,
          expr ? ok(expr) : expected("expression", next_token),
        ];
      }

      // select expression
      if (op.ok === ".") {
        let name: Result<ParseError, NameIdentifier>;
        [next_token, name] = await take_name(next_token, tokens);
        if ("err" in name) return [next_token, name];
        const value = name.ok;
        yard.push_op({
          postfix: ["select", (expr) => ({ select: [expr, value] })],
        });
        combine_state = true;
        continue;
      }

      // for binary expressions, switch back to combine = false
      const infix_op = get_infix_op(op.ok);
      if (infix_op) {
        yard.push_op(infix_op);
        combine_state = false;
        continue;
      }

      // for postfix expressions, keep combining
      const postfix_op = get_postfix_op(op.ok);
      if (postfix_op) {
        yard.push_op(postfix_op);
        combine_state = true;
        continue;
      }

      // invalid operator, something went wrong
      return [
        next_token,
        expected(`".", prefix, or postfix operator`, { symbol: op.ok }),
      ];
    } else {
      // name
      [next_token, name] = await take_name(next_token, tokens);
      if ("ok" in name) {
        yard.push_expr(name.ok);
        combine_state = true;
        continue;
      }

      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "self",
      );
      if ("ok" in success_token) {
        yard.push_expr({ self: null });
        combine_state = true;
        continue;
      }

      // constructor
      [next_token, type] = await take_type(next_token, tokens);
      if ("ok" in type) {
        yard.push_expr(type.ok);
        combine_state = true;
        continue;
      }

      // fn expression
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "fn",
      );
      if ("ok" in success_token) {
        let fn: Result<ParseError, Fn>;
        // pass the fn token into the parser so that it "reads" the fn token
        [next_token, fn] = await take_fn(success_token.ok, tokens);
        if ("err" in fn) return [next_token, fn];

        yard.push_expr(fn_expr(fn.ok));
        combine_state = true;
        continue;
      }

      // group expression
      [next_token, success_token] = await take_symbol(next_token, tokens, "(");
      if ("ok" in success_token) {
        yard.push_group();
        combine_state = false;
        continue;
      }

      // block expression
      [next_token, success_token] = await take_symbol(next_token, tokens, "{");
      if ("ok" in success_token) {
        let block: Result<ParseError, BodyExpression[]>;
        [next_token, block] = await take_list(
          next_token,
          tokens,
          take_body_expression,
          ";",
          "}",
        );
        if ("err" in block) return [next_token, block];
        yard.push_expr({ block: block.ok });
        combine_state = true;
        continue;
      }

      // match expression
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "match",
      );
      if ("ok" in success_token) {
        let scrutinee: Result<ParseError, Expression>;
        let match_arms: Result<ParseError, MatchArm[]>;
        [next_token, scrutinee] = await take_expression(next_token, tokens);
        if ("err" in scrutinee) return [next_token, scrutinee];

        // expect "{"
        [next_token, success_token] = await take_symbol(
          next_token,
          tokens,
          "{",
        );
        if ("err" in success_token) return [next_token, success_token];

        [next_token, match_arms] = await take_list(
          next_token,
          tokens,
          take_match_arm,
          ",",
          "}",
        );

        if ("err" in match_arms) return [next_token, match_arms];
        if (match_arms.ok.length === 0)
          return [next_token, expected("non-zero match arms", next_token)];

        // match success, now combine
        yard.push_expr({ match: [scrutinee.ok, match_arms.ok] });
        combine_state = true;
        continue;
      }

      // Infinity
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "inf",
      );
      if ("ok" in success_token) {
        yard.push_expr({ float: { size: 64, value: Infinity } });
        combine_state = true;
        continue;
      }

      // NaN
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "nan",
      );
      if ("ok" in success_token) {
        yard.push_expr({ float: { size: 64, value: NaN } });
        combine_state = true;
        continue;
      }

      // true
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "true",
      );
      if ("ok" in success_token) {
        yard.push_expr({ bool: true });
        combine_state = true;
        continue;
      }

      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "false",
      );
      if ("ok" in success_token) {
        yard.push_expr({ bool: false });
        combine_state = true;
        continue;
      }

      // if expression
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "if",
      );
      if ("ok" in success_token) {
        let cond: Result<ParseError, Expression>;
        [next_token, cond] = await take_expression(next_token, tokens);
        if ("err" in cond) return [next_token, cond];

        let if_body: Result<ParseError, Expression>;
        let else_body: Result<ParseError, Expression> | null = null;
        [next_token, if_body] = await take_expression(next_token, tokens);
        if ("err" in if_body) return [next_token, if_body];

        [next_token, success_token] = await take_keyword(
          next_token,
          tokens,
          "else",
        );
        if ("ok" in success_token) {
          [next_token, else_body] = await take_expression(next_token, tokens);
          if ("err" in else_body) return [next_token, else_body];
        }

        return [
          next_token,
          ok({
            if_expr: {
              cond: cond.ok,
              if_body: if_body.ok,
              else_body: else_body?.ok ?? null,
            },
          }),
        ];
      }

      [next_token, int] = await take_int(next_token, tokens);
      if ("ok" in int) {
        yard.push_expr({ int: { size: 32, value: int.ok.int } });
        combine_state = true;
        continue;
      }

      [next_token, float] = await take_float(next_token, tokens);
      if ("ok" in float) {
        yard.push_expr({ float: { size: 64, value: float.ok.float } });
        combine_state = true;
        continue;
      }

      [next_token, string] = await take_string(next_token, tokens);
      if ("ok" in string) {
        yard.push_expr(string.ok);
        combine_state = true;
        continue;
      }

      // Record or Tuple
      [next_token, success_token] = await take_symbol(next_token, tokens, "#");
      if ("ok" in success_token) {
        next_token = await next(tokens, false);
        if (!next_token) return [null, expected("record or tuple", null)];

        // Record term
        if ("symbol" in next_token && next_token.symbol === "{") {
          let record: Result<ParseError, [NameIdentifier, Expression][]>;
          [next_token, record] = await take_list(
            null,
            tokens,
            take_key_value_pair,
            ",",
            "}",
          );
          if ("err" in record) return [next_token, record];
          combine_state = true;
          yard.push_expr({ record: record.ok });
          continue;
        }

        // tuple
        if ("symbol" in next_token && next_token.symbol === "(") {
          let tuple: Result<ParseError, Expression[]>;
          [next_token, tuple] = await take_list(
            null,
            tokens,
            take_expression,
            ",",
            ")",
          );
          if ("err" in tuple) return [next_token, tuple];

          yard.push_expr({ tuple: tuple.ok });
          combine_state = true;
          continue;
        }

        return [next_token, expected("expression", next_token)];
      }

      [next_token, op] = await take_operator(next_token, tokens);
      if ("ok" in op) {
        const prefix_op = get_prefix_op(op.ok);
        if (prefix_op) {
          yard.push_op(prefix_op);
          combine_state = false;
          continue;
        }

        return [next_token, expected("operator", { symbol: op.ok })];
      }

      return [next_token, expected("expression", next_token)];
    }
  }
};

export const take_list = async <T>(
  next_token: Token | null,
  tokens: TokenIter,
  fn: (next_token: Token | null, tokens: TokenIter) => Promise<ParseResult<T>>,
  sep: string,
  term: string,
): Promise<ParseResult<T[]>> => {
  const results = [] as T[];
  let item: Result<ParseError, T>;
  next_token ??= await next(tokens);
  if (!next_token) return [null, expected("token", null)];

  if ("symbol" in next_token && next_token.symbol === term)
    return [null, ok(results)];

  while (true) {
    // expect a parsed item
    [next_token, item] = await fn(next_token, tokens);
    if ("err" in item) return [next_token, item];

    results.push(item.ok);
    next_token ??= await next(tokens);

    if (!next_token) return [null, expected("token", null)];
    if ("symbol" in next_token && next_token.symbol === term)
      return [null, ok(results)];
    if ("symbol" in next_token && next_token.symbol === sep) {
      next_token = null;
      continue;
    }

    // parse failed, pass the token on
    return [next_token, expected("list", next_token)];
  }
};

export const take_fn_param = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<FnParam>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, NameIdentifier>;
  let guard: Result<ParseError, TypeExpression>;

  [next_token, name] = await take_name(next_token, tokens);
  if ("err" in name) return [next_token, name];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("ok" in success_token) {
    [next_token, guard] = await take_type_expression(next_token, tokens);
    if ("err" in guard) return [next_token, guard];
    return [next_token, ok({ name: name.ok, guard: guard.ok })];
  }

  return [next_token, ok({ name: name.ok, guard: null })];
};

export const take_import = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<Import>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, WasmName>;
  let type: Result<ParseError, TypeIdentifier>;
  let alias: Result<ParseError, NameIdentifier> | null = null;
  let type_alias: Result<ParseError, TypeIdentifier> | null = null;
  let type_expression: Result<ParseError, TypeExpression>;
  let min: Result<ParseError, IntToken> | null = null;
  let max: Result<ParseError, IntToken> | null = null;

  // fn import
  [next_token, success_token] = await take_keyword(next_token, tokens, "fn");
  if ("ok" in success_token) {
    let param_types: Result<ParseError, FnParam[]>;

    [next_token, name] = await take_name(next_token, tokens);
    if ("err" in name)
      [next_token, name] = await take_string(next_token, tokens);
    if ("err" in name)
      return [next_token, expected("web assembly name", next_token)];

    // fn ("name"|name) ("(" ...params ")" -> ReturnType):?
    [next_token, success_token] = await take_symbol(next_token, tokens, "(");
    if ("err" in success_token)
      return [next_token, expected(`symbol("(")`, next_token)];

    // full signature is now required
    [next_token, param_types] = await take_list(
      next_token,
      tokens,
      take_fn_param,
      ",",
      ")",
    );
    if ("err" in param_types) return [next_token, param_types];

    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if ("err" in success_token) return [next_token, success_token];

    [next_token, type_expression] = await take_type_expression(
      next_token,
      tokens,
    );
    if ("err" in type_expression) return [next_token, type_expression];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      // alias required now
      [next_token, alias] = await take_name(next_token, tokens);
      if ("err" in alias) return [next_token, alias];
    }

    return [
      next_token,
      ok({
        fn: {
          name: name.ok,
          signature: {
            param_types: param_types.ok,
            return_type: type_expression.ok,
          },
          alias: alias?.ok ?? null,
        },
      }),
    ];
  }

  // memory import
  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "memory",
  );
  if ("ok" in success_token) {
    [next_token, name] = await take_name(next_token, tokens);
    if ("err" in name)
      [next_token, name] = await take_string(next_token, tokens);
    if ("err" in name) return [next_token, name];

    // we expect ":"
    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if ("err" in success_token) return [next_token, success_token];
    // take one or two integers
    [next_token, min] = await take_int(next_token, tokens);
    if ("err" in min) return [next_token, min];

    // this is optional.
    [next_token, max] = await take_int(next_token, tokens);
    if ("err" in max) max = null;

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      [next_token, alias] = await take_name(next_token, tokens);
      if ("err" in alias) return [next_token, alias];
    }

    return [
      next_token,
      ok({
        memory: {
          name: name.ok,
          min: min.ok,
          max: max?.ok ?? null,
          alias: alias?.ok ?? null,
        },
      }),
    ];
  }

  // global import
  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "global",
  );
  if ("ok" in success_token) {
    let mut = false;
    [next_token, success_token] = await take_keyword(next_token, tokens, "mut");
    mut = "ok" in success_token;

    [next_token, name] = await take_name(next_token, tokens);
    if ("err" in name)
      [next_token, name] = await take_string(next_token, tokens);
    if ("err" in name) return [next_token, expected("wasm name", next_token)];

    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if ("err" in success_token) return [next_token, success_token];

    [next_token, type_expression] = await take_type_expression(
      next_token,
      tokens,
    );
    if ("err" in type_expression) return [next_token, type_expression];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      // "as" expects an alias of some kind
      [next_token, alias] = await take_name(next_token, tokens);
      if ("err" in alias) return [next_token, alias];
    }

    return [
      next_token,
      ok({
        global: {
          alias: alias?.ok ?? null,
          global_type: type_expression.ok,
          mut,
          name: name.ok,
        },
      }),
    ];
  }

  // table import
  [next_token, success_token] = await take_keyword(next_token, tokens, "table");
  if ("ok" in success_token) {
    [next_token, name] = await take_name(next_token, tokens);
    if ("err" in name)
      [next_token, name] = await take_string(next_token, tokens);
    if ("err" in name) return [next_token, expected("wasm name", next_token)];

    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if ("err" in success_token) return [next_token, success_token];

    // later check to make sure it's a "Table"
    [next_token, type_expression] = await take_type_expression(
      next_token,
      tokens,
    );
    if ("err" in type_expression) return [next_token, type_expression];

    [next_token, min] = await take_int(next_token, tokens);
    if ("err" in min) return [next_token, min];

    // optional
    [next_token, max] = await take_int(next_token, tokens);
    if ("err" in max) max = null;

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      // required alias now
      [next_token, alias] = await take_name(next_token, tokens);
      if ("err" in alias) return [next_token, alias];
    }

    return [
      next_token,
      ok({
        table: {
          alias: alias?.ok ?? null,
          max: max?.ok ?? null,
          min: min.ok,
          name: name.ok,
          table_type: type_expression.ok,
        },
      }),
    ];
  }

  // type import
  [next_token, success_token] = await take_keyword(next_token, tokens, "type");
  if ("ok" in success_token) {
    [next_token, type] = await take_type(next_token, tokens);
    if ("err" in type) return [next_token, type];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      [next_token, type_alias] = await take_type(next_token, tokens);
      if ("err" in type_alias) return [next_token, type_alias];
    }

    return [
      next_token,
      ok({
        type: {
          name: type.ok,
          alias: type_alias?.ok ?? null,
        },
      }),
    ];
  }

  // trait import
  [next_token, success_token] = await take_keyword(next_token, tokens, "trait");
  if ("ok" in success_token) {
    [next_token, type] = await take_type(next_token, tokens);
    if ("err" in type) return [next_token, type];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      [next_token, type_alias] = await take_type(next_token, tokens);
      if ("err" in type_alias) return [next_token, type_alias];
    }

    return [
      next_token,
      ok({
        trait: {
          name: type.ok,
          alias: type_alias?.ok ?? null,
        },
      }),
    ];
  }

  [next_token, success_token] = await take_keyword(next_token, tokens, "enum");
  if ("ok" in success_token) {
    [next_token, type] = await take_type(next_token, tokens);
    if ("err" in type) return [next_token, type];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if ("ok" in success_token) {
      [next_token, type_alias] = await take_type(next_token, tokens);
      if ("err" in type_alias) return [next_token, type_alias];
    }

    return [
      next_token,
      ok({ enum: { name: type.ok, alias: type_alias?.ok ?? null } }),
    ];
  }

  // named import
  [next_token, name] = await take_name(next_token, tokens);
  if ("ok" in name) {
    next_token = await next(tokens);
    if (!next_token) {
      return [
        next_token,
        ok({ name: { alias: null, name: name.ok as NameToken } }),
      ];
    }

    // optional as
    if ("keyword" in next_token && next_token.keyword === "as") {
      next_token = await next(tokens);
      if (next_token && "name" in next_token)
        return [
          null,
          ok({
            name: {
              name: name.ok as NameToken,
              alias: next_token,
            },
          }),
        ];
      return [next_token, expected("alias name", next_token)];
    }

    return [
      next_token,
      ok({ name: { name: name.ok as NameToken, alias: null } }),
    ];
  }

  [next_token, type] = await take_type(next_token, tokens);
  if ("ok" in type) {
    // optional as
    next_token = await next(tokens);

    if (next_token && "keyword" in next_token && next_token.keyword === "as") {
      next_token = await next(tokens);
      if (next_token && "type" in next_token)
        return [null, ok({ constr: { name: type.ok, alias: next_token } })];

      return [next_token, expected("type", next_token)];
    }

    // import still succeeds
    return [next_token, ok({ constr: { name: type.ok, alias: null } })];
  }

  // failed import
  return [next_token, expected("import", next_token)];
};

export const take_builtin_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<Declaration>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, StringToken>;
  let alias: Result<ParseError, NameIdentifier>;
  let params: Result<ParseError, FnParam[]>;
  let return_type: Result<ParseError, TypeExpression>;

  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "builtin",
  );
  if ("err" in success_token) return [next_token, success_token];

  [next_token, name] = await take_string(next_token, tokens);
  if ("err" in name) return [next_token, name];

  [next_token, success_token] = await take_keyword(next_token, tokens, "as");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, alias] = await take_name(next_token, tokens);
  if ("err" in alias) return [next_token, alias];

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, params] = await take_list(
    next_token,
    tokens,
    take_fn_param,
    ",",
    ")",
  );
  if ("err" in params) return [next_token, params];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, return_type] = await take_type_expression(next_token, tokens);
  if ("err" in return_type) return [next_token, return_type];

  return [
    next_token,
    ok({
      builtin: {
        name: name.ok,
        alias: alias.ok,
        params: params.ok,
        return_type: return_type.ok,
      },
    }),
  ];
};

export const take_import_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<Declaration>> => {
  let success_token: Result<ParseError, Token>;
  let import_from: Result<ParseError, StringToken>;
  let imports: Result<ParseError, Import[]>;

  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "import",
  );
  if ("err" in success_token) return [next_token, success_token];

  [next_token, import_from] = await take_string(next_token, tokens);
  if ("err" in import_from) return [next_token, import_from];

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, imports] = await take_list(null, tokens, take_import, ",", "}");
  if ("err" in imports) return [next_token, imports];
  return [
    next_token,
    ok({
      import_dec: { import_from: import_from.ok.string, imports: imports.ok },
    }),
  ];
};

export const take_fn = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<Fn>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, NameIdentifier> | null;
  let type_params: Result<ParseError, NameIdentifier[]>;
  let params: Result<ParseError, FnParam[]>;
  let return_type: Result<ParseError, TypeExpression> | null;
  let body: Result<ParseError, Expression>;
  [next_token, success_token] = await take_keyword(next_token, tokens, "fn");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, name] = await take_name(next_token, tokens);
  if ("err" in name) name = null;

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if ("ok" in success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if ("err" in type_params) return [next_token, type_params];
  } else type_params = { ok: [] };

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, params] = await take_list(
    next_token,
    tokens,
    take_fn_param,
    ",",
    ")",
  );
  if ("err" in params) return [next_token, params];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("ok" in success_token) {
    [next_token, return_type] = await take_type_expression(null, tokens);
    if ("err" in return_type) return [next_token, return_type];
  } else return_type = null;

  [next_token, body] = await take_expression(next_token, tokens);
  if ("err" in body) return [next_token, body];

  return [
    next_token,
    ok({
      body: body.ok,
      name: name?.ok ?? null,
      type_params: type_params.ok,
      params: params.ok,
      return_type: return_type?.ok ?? null,
    }),
  ];
};

const consume_until_keyword_or_semicolon = async (tokens: TokenIter) => {
  while (true) {
    const token = await next(tokens);
    if (!token) return null;
    // consume the semicolon and advance
    if ("symbol" in token && token.symbol === ";") return null;
    if (
      "keyword" in token &&
      (token.keyword === "builtin" ||
        token.keyword === "type" ||
        token.keyword === "let" ||
        token.keyword === "trait" ||
        token.keyword === "pub" ||
        token.keyword === "fn" ||
        token.keyword === "impl" ||
        token.keyword === "enum")
    ) {
      return token;
    }
  }
};

const take_int = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<IntToken>> => {
  next_token ??= await next(tokens);
  if (next_token && "int" in next_token) return [null, ok(next_token)];
  return [next_token, expected("int", next_token)];
};

const take_float = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<FloatToken>> => {
  next_token ??= await next(tokens);
  if (next_token && "float" in next_token) return [null, ok(next_token)];
  return [next_token, expected("float", next_token)];
};
const take_symbol = async (
  next_token: Token | null,
  tokens: TokenIter,
  kind: string | null = null,
): Promise<ParseResult<SymbolToken>> => {
  next_token ??= await next(tokens);
  if (next_token && "symbol" in next_token) {
    if (!kind) return [null, expected(`symbol(${kind})`, next_token)];
    if (next_token.symbol === kind) return [null, ok(next_token)];
  }

  return [next_token, expected(`symbol(${kind})`, next_token)];
};

const take_keyword = async (
  next_token: Token | null,
  tokens: TokenIter,
  keyword: string | null = null,
): Promise<ParseResult<Token>> => {
  next_token ??= await next(tokens);
  if (next_token && "keyword" in next_token && next_token.keyword === keyword)
    return [null, ok(next_token)];
  return [next_token, expected(`keyword(${keyword})`, next_token)];
};

const take_name = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<NameIdentifier>> => {
  next_token ??= await next(tokens);
  if (next_token && "name" in next_token) return [null, ok(next_token)];
  return [next_token, expected("name", next_token)];
};

const take_type = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<TypeIdentifier>> => {
  next_token ??= await next(tokens);
  if (next_token && "type" in next_token) return [null, ok(next_token)];
  return [next_token, expected("type", next_token)];
};

const take_string = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<StringToken>> => {
  next_token ??= await next(tokens);
  if (next_token && "string" in next_token) return [null, ok(next_token)];
  return [next_token, expected("string", next_token)];
};

const take_type_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<TypeDeclaration>> => {
  let success_token: Result<ParseError, Token>;
  let id: Result<ParseError, TypeIdentifier>;
  let params: Result<ParseError, NameIdentifier[]>;
  let symbol: Result<ParseError, Token>;
  let value: Result<ParseError, TypeExpression>;

  [next_token, success_token] = await take_keyword(next_token, tokens, "type");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, id] = await take_type(next_token, tokens);
  if ("err" in id) return [next_token, id];

  [next_token, symbol] = await take_symbol(next_token, tokens, "<");
  if ("ok" in symbol) {
    [next_token, params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if ("err" in params) return [next_token, params];
  } else params = { ok: [] };

  [next_token, symbol] = await take_symbol(next_token, tokens, "=");
  if ("err" in symbol) return [next_token, symbol];

  [next_token, value] = await take_type_expression(next_token, tokens);
  if ("err" in value) return [next_token, value];

  return [
    next_token,
    ok({
      type_dec: { pub: false, id: id.ok, params: params.ok, value: value.ok },
    }),
  ];
};

const take_enum_variant = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<EnumVariant>> => {
  let success_token: Result<ParseError, Token>;
  let id: Result<ParseError, TypeIdentifier>;

  [next_token, id] = await take_type(next_token, tokens);
  if ("err" in id) return [next_token, id];

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if ("ok" in success_token) {
    let values: Result<ParseError, TypeExpression[]>;
    [next_token, values] = await take_list(
      next_token,
      tokens,
      take_type_expression,
      ",",
      ")",
    );
    if ("err" in values) return [next_token, values];
    return [next_token, ok({ values: { id: id.ok, values: values.ok } })];
  }

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if ("ok" in success_token) {
    let fields: Result<ParseError, NamedTypeExpression[]>;
    [next_token, fields] = await take_list(
      next_token,
      tokens,
      take_key_value_pair_type_expression,
      ",",
      "}",
    );
    if ("err" in fields) return [next_token, fields];

    return [next_token, ok({ fields: { id: id.ok, fields: fields.ok } })];
  }

  return [next_token, ok({ values: { id: id.ok, values: [] } })];
};

// enum Option<t> { Some(t), None }
const take_enum_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<EnumDeclaration>> => {
  let success_token: Result<ParseError, Token>;
  let id: Result<ParseError, TypeIdentifier>;
  let type_params: Result<ParseError, NameIdentifier[]>;
  let variants: Result<ParseError, EnumVariant[]>;

  [next_token, success_token] = await take_keyword(next_token, tokens, "enum");
  if ("err" in success_token) return [next_token, success_token];

  // enum type name
  [next_token, id] = await take_type(next_token, tokens);
  if ("err" in id) return [next_token, id];

  // type parameters
  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if ("ok" in success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if ("err" in type_params) return [next_token, type_params];
  } else type_params = { ok: [] };

  // expect "{"
  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, variants] = await take_list(
    next_token,
    tokens,
    take_enum_variant,
    ",",
    "}",
  );
  if ("err" in variants) return [next_token, variants];

  return [
    next_token,
    ok({
      enum: {
        id: id.ok,
        pub: false,
        type_params: type_params.ok,
        variants: variants.ok,
      },
    }),
  ];
};

const take_many = async <T>(
  next_token: Token | null,
  tokens: TokenIter,
  take: (
    next_token: Token | null,
    tokens: TokenIter,
  ) => Promise<ParseResult<T>>,
  until: string,
): Promise<ParseResult<T[]>> => {
  let success_token: Result<ParseError, Token>;
  const results = [] as T[];
  let value: Result<ParseError, T>;
  while (true) {
    [next_token, success_token] = await take_symbol(next_token, tokens, until);
    if ("ok" in success_token) return [next_token, ok(results)];

    [next_token, value] = await take(next_token, tokens);
    if ("ok" in value) {
      results.push(value.ok);
      continue;
    }

    return [next_token, value];
  }
};

const take_trait_fn = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<TraitFn>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, NameIdentifier>;
  let type_params: Result<ParseError, NameIdentifier[]>;
  let params: Result<ParseError, NamedTypeExpression[]>;
  let return_type: Result<ParseError, TypeExpression>;

  [next_token, success_token] = await take_keyword(next_token, tokens, "fn");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, name] = await take_name(next_token, tokens);
  if ("err" in name) return [next_token, name];

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if ("ok" in success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if ("err" in type_params) return [next_token, type_params];
  } else type_params = { ok: [] };

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, params] = await take_list(
    next_token,
    tokens,
    take_key_value_pair_type_expression,
    ",",
    ")",
  );
  if ("err" in params) return [next_token, params];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("err" in success_token) return [next_token, success_token];

  if ("ok" in success_token) {
    [next_token, return_type] = await take_type_expression(next_token, tokens);
    if ("err" in return_type) return [next_token, return_type];

    return [
      next_token,
      ok({
        name: name.ok,
        params: params.ok,
        type_params: type_params.ok,
        return_type: return_type.ok,
      }),
    ];
  }

  if (next_token && "whitespace" in next_token)
    return [null, expected("trait function", next_token)];
  return [next_token, expected("trait function", next_token)];
};

const take_impl_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<ImplDeclaration>> => {
  let success_token: Result<ParseError, Token>;
  let name: Result<ParseError, TypeIdentifier>;
  let type_params: Result<ParseError, TypeExpression[]>;
  let impl_for: Result<ParseError, TypeExpression>;
  let fns: Result<ParseError, Fn[]>;

  [next_token, success_token] = await take_keyword(next_token, tokens, "impl");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, name] = await take_type(next_token, tokens);
  if ("err" in name) return [next_token, name];

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if ("ok" in success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_type_expression,
      ",",
      ">",
    );
    if ("err" in type_params) return [next_token, type_params];
  } else type_params = { ok: [] };

  [next_token, success_token] = await take_keyword(next_token, tokens, "for");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, impl_for] = await take_type_expression(next_token, tokens);
  if ("err" in impl_for) return [next_token, impl_for];

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, fns] = await take_many(next_token, tokens, take_fn, "}");
  if ("err" in fns) return [next_token, fns];
  const impl = {
    impl: {
      for: impl_for.ok,
      name: name.ok,
      type_params: type_params.ok,
      fns: fns.ok,
    },
  };
  console.log(showDeclaration(impl), JSON.stringify(impl, null, 2));
  return [next_token, ok(impl)];
};

const take_trait_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<TraitDeclaration>> => {
  let success_token: Result<ParseError, Token>;
  let id: Result<ParseError, TypeIdentifier>;
  let type_params: Result<ParseError, NameIdentifier[]>;
  let fns: Result<ParseError, TraitFn[]>;

  [next_token, success_token] = await take_keyword(next_token, tokens, "trait");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, id] = await take_type(next_token, tokens);
  if ("err" in id) return [next_token, id];

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if ("ok" in success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if ("err" in type_params) return [next_token, type_params];
  } else type_params = { ok: [] };

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, fns] = await take_many(next_token, tokens, take_trait_fn, "}");
  if ("err" in fns) return [next_token, fns];

  return [
    next_token,
    ok({
      trait: {
        fns: fns.ok,
        id: id.ok,
        pub: false,
        type_params: type_params.ok,
      },
    }),
  ];
};

const take_let_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<LetDeclaration>> => {
  let success_token: Result<ParseError, Token>;
  let id: Result<ParseError, NameIdentifier>;
  let guard: Result<ParseError, TypeExpression> | null = null;
  let value: Result<ParseError, Expression>;

  [next_token, success_token] = await take_keyword(next_token, tokens, "let");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, id] = await take_name(next_token, tokens);
  if ("err" in id) return [next_token, id];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if ("ok" in success_token) {
    [next_token, guard] = await take_type_expression(next_token, tokens);
    if ("err" in guard) return [next_token, guard];
  }

  [next_token, success_token] = await take_symbol(next_token, tokens, "=");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, value] = await take_expression(next_token, tokens);
  if ("err" in value) return [next_token, value];

  return [
    next_token,
    ok({
      let_dec: {
        guard: guard?.ok ?? null,
        id: id.ok,
        pub: false,
        value: value.ok,
      },
    }),
  ];
};

const take_fn_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<FnDeclaration>> => {
  let fn: Result<ParseError, Fn>;

  [next_token, fn] = await take_fn(next_token, tokens);
  if ("err" in fn) return [next_token, fn];

  if (!fn.ok.name)
    return [next_token, expected("function with a name", { unknown: null })];

  return [next_token, ok({ fn: { pub: false, fn: fn.ok } })];
};

const take_pub = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<Declaration>> => {
  let success_token: Result<ParseError, Token>;
  let decl: Result<ParseError, Declaration>;
  [next_token, success_token] = await take_keyword(next_token, tokens, "pub");
  if ("err" in success_token) return [next_token, success_token];

  [next_token, decl] = await take_fn_declaration(next_token, tokens);
  if ("ok" in decl) {
    (decl.ok as FnDeclaration).fn.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_type_declaration(next_token, tokens);
  if ("ok" in decl) {
    (decl.ok as TypeDeclaration).type_dec.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_let_declaration(next_token, tokens);
  if ("ok" in decl) {
    (decl.ok as LetDeclaration).let_dec.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_enum_declaration(next_token, tokens);
  if ("ok" in decl) {
    (decl.ok as EnumDeclaration).enum.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_trait_declaration(next_token, tokens);
  if ("ok" in decl) {
    (decl.ok as TraitDeclaration).trait.pub = true;
    return [next_token, decl];
  }

  return [await consume_until_keyword_or_semicolon(tokens), decl];
};

export const parse = async (text: string) =>
  module_from(parse_tokens_streaming(lex(chars(text))));

export const parse_file = async (file: string) =>
  module_from(parse_tokens_streaming(lex(chars_from(file))));

type DeclarationIterator = AsyncGenerator<
  Result<ParseError, Declaration>,
  void,
  unknown
>;
export const module_from = async (decls: DeclarationIterator) => {
  const module = [] as Declaration[];
  const errors = [] as ParseError[];
  for await (const decl of decls) {
    if ("ok" in decl) module.push(decl.ok);
    else errors.push(decl.err);
  }
  return { module: { module }, errors };
};

const parse_tokens_streaming = async function* (tokens: TokenIter) {
  let next_token: Token | null = null;
  let declaration: Result<ParseError, Declaration>;

  while (true) {
    if (!next_token) {
      const result = await next(tokens);
      if (!result) return;
      next_token = result;
    }

    [next_token, declaration] = await take_declaration(next_token, tokens);
    yield declaration;
  }
};

export const parse_streaming = (text: string) =>
  parse_tokens_streaming(lex(chars(text)));
export const parse_file_streaming = (file: string) =>
  parse_tokens_streaming(lex(chars_from(file)));

export async function take_declaration(
  next_token: Token | null,
  tokens: TokenIter,
): Promise<ParseResult<Declaration>> {
  let decl: Result<ParseError, Declaration>;

  [next_token, decl] = await take_pub(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_fn_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_enum_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_type_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_let_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_trait_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_impl_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_import_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  [next_token, decl] = await take_builtin_declaration(next_token, tokens);
  if ("ok" in decl) return [next_token, decl];

  next_token = await consume_until_keyword_or_semicolon(tokens);
  return [next_token, decl];
}

export const name_expr = (name: string): NameIdentifier => ({ name });
export const call_expr = (
  fn: Expression,
  args: Expression[],
): CallExpression => ({
  call: [fn, args],
});
export const block_expr = (block: BodyExpression[]): BlockExpression => ({
  block,
});
export const if_expr = (
  cond: Expression,
  if_body: Expression,
  else_body: Expression | null,
): IfExpression => ({
  if_expr: {
    cond,
    if_body,
    else_body,
  },
});
export const select_expr = (
  record: Expression,
  name: string,
): SelectExpression => ({
  select: [record, name_expr(name)],
});
export const match_expr = (
  scrutinee: Expression,
  arms: MatchArm[],
): MatchExpression => ({
  match: [scrutinee, arms],
});
export const float_expr = (value: number, size: 32 | 64): FloatExpression => ({
  float: { value, size },
});
export const int_expr = (
  value: number | bigint,
  size: 8 | 16 | 32 | 64,
): IntExpression => ({
  int: { value: BigInt(value), size },
});
export const bool_expr = (bool: boolean): BoolExpression => ({ bool });
export const prefix_expr = (
  op: PrefixOp,
  operand: Expression,
): PrefixExpression => ({
  prefix: { op, operand },
});
export const postfix_expr = (
  operand: Expression,
  op: PostfixOp,
): PostfixExpression => ({
  postfix: { op, operand },
});
export const infix_expr = (
  left: Expression,
  op: InfixOp,
  right: Expression,
): InfixExpression => ({
  infix: { left, op, right },
});
export const string_expr = (string: string): StringToken => ({ string });
export const fn_expr = (fn: Fn): FnExpression => ({ fn });
export const record_expr = (
  record: [string, Expression][],
): RecordExpression => ({
  record: record.map(([k, v]) => [name_expr(k), v]),
});
export const tuple_expr = (tuple: Expression[]): TupleExpression => ({ tuple });
export const self_expr = (): SelfExpression => ({ self: null });

export const arrow_kind_body_expr = (
  name: string,
  expression: Expression,
): ArrowKindBodyExpression => ({
  arrow_bind: {
    name: name_expr(name),
    expression,
  },
});
export const let_bind_body_expr = (
  assert: boolean,
  pattern: PatternExpression,
  expression: Expression,
): LetBindBodyExpression => ({
  let_bind: {
    assert,
    pattern,
    expression,
  },
});
export const assign_body_expr = (
  name: string,
  expression: Expression,
): AssignBodyExpression => ({
  assign: {
    name: name_expr(name),
    expression,
  },
});
export const expression_body_expr = (
  expression: Expression,
): ExpressionBodyExpression => ({
  expression,
});
export const fn_param = (
  name: string,
  guard: TypeExpression | null,
): FnParam => ({
  name: name_expr(name),
  guard,
});
// Show methods for AST stringification

export function showModule(mod: Module): string {
  return mod.module.map(showDeclaration).join("\n\n");
}

export function showDeclaration(decl: Declaration): string {
  if ("builtin" in decl) {
    const { name, alias, params, return_type } = decl.builtin;
    return `builtin ${showToken(name)} as ${showToken(alias)}(${params.map(showFnParam).join(", ")}): ${showTypeExpression(return_type)}`;
  }
  if ("fn" in decl) {
    const { pub, fn } = decl.fn;
    return `${pub ? "pub " : ""}${showFn(fn)}`;
  }
  if ("enum" in decl) {
    const { pub, id, type_params, variants } = decl.enum;
    const params =
      type_params.length > 0
        ? `<${type_params.map(showToken).join(", ")}>`
        : "";
    const vars = variants.map((v) => `\n  ${showEnumVariant(v)}`).join("");
    return `${pub ? "pub " : ""}enum ${showToken(id)}${params} {${vars}\n}`;
  }
  if ("import_dec" in decl) {
    const { import_from, imports } = decl.import_dec;
    return `import { ${imports.map(showImport).join(", ")} } from "${import_from}"`;
  }
  if ("type_dec" in decl) {
    const { pub, id, params, value } = decl.type_dec;
    const p = params.length > 0 ? `<${params.map(showToken).join(", ")}>` : "";
    return `${pub ? "pub " : ""}type ${showToken(id)}${p} = ${showTypeExpression(value)}`;
  }
  if ("let_dec" in decl) {
    const { pub, id, guard, value } = decl.let_dec;
    const g = guard ? `: ${showTypeExpression(guard)}` : "";
    return `${pub ? "pub " : ""}let ${showToken(id)}${g} = ${showExpression(value)}`;
  }
  if ("trait" in decl) {
    const { pub, id, type_params, fns } = decl.trait;
    const params =
      type_params.length > 0
        ? `<${type_params.map(showToken).join(", ")}>`
        : "";
    const methods = fns.map((f) => `\n  ${showTraitFn(f)}`).join("");
    return `${pub ? "pub " : ""}trait ${showToken(id)}${params} {${methods}\n}`;
  }
  if ("impl" in decl) {
    const { name, type_params, for: forType, fns } = decl.impl;
    const params =
      type_params.length > 0
        ? `<${type_params.map(showTypeExpression).join(", ")}>`
        : "";
    const methods = fns.map((f) => `\n  ${showFn(f)}`).join("");
    return `impl ${showToken(name)}${params} for ${showTypeExpression(forType)} {${methods}\n}`;
  }
  return "";
}

export function showEnumVariant(variant: EnumVariant): string {
  if ("fields" in variant) {
    const { id, fields } = variant.fields;
    return `${showToken(id)} { ${fields.map(showNamedTypeExpression).join(", ")} }`;
  }
  if ("values" in variant) {
    const { id, values } = variant.values;
    return `${showToken(id)}(${values.map(showTypeExpression).join(", ")})`;
  }
  return "";
}

export function showTraitFn(fn: TraitFn): string {
  const { name, params, return_type } = fn;
  return `fn ${showToken(name)}(${params.map(showNamedTypeExpression).join(", ")}): ${showTypeExpression(return_type)}`;
}

export function showFn(fn: Fn): string {
  const { name, type_params, params, return_type, body } = fn;
  const n = name ? showToken(name) : "";
  const tp =
    type_params.length > 0 ? `<${type_params.map(showToken).join(", ")}>` : "";
  const ret = return_type ? `: ${showTypeExpression(return_type)}` : "";
  return `fn ${n}${tp}(${params.map(showFnParam).join(", ")})${ret} ${showExpression(body)}`;
}

export function showFnParam(param: FnParam): string {
  const { name, guard } = param;
  return guard
    ? `${showToken(name)}: ${showTypeExpression(guard)}`
    : showToken(name);
}

export function showImport(imp: Import): string {
  if ("constr" in imp) {
    const { name, alias } = imp.constr;
    return alias
      ? `${showToken(name)} as ${showToken(alias)}`
      : showToken(name);
  }
  if ("enum" in imp) {
    const { name, alias } = imp.enum;
    return alias
      ? `${showToken(name)} as ${showToken(alias)}`
      : showToken(name);
  }
  if ("fn" in imp) {
    const { name, signature, alias } = imp.fn;
    const n = showWasmName(name);
    const a = alias ? ` as ${showToken(alias)}` : "";
    return `fn ${n}${showFnSignature(signature)}${a}`;
  }
  if ("global" in imp) {
    const { mut, name, global_type, alias } = imp.global;
    const m = mut ? "mut " : "";
    const a = alias ? ` as ${showToken(alias)}` : "";
    return `${m}global ${showWasmName(name)}: ${showTypeExpression(global_type)}${a}`;
  }
  if ("memory" in imp) {
    const { name, min, max, alias } = imp.memory;
    const limits = [min!, max!].filter(Boolean).map(showToken).join(", ");
    const a = alias ? ` as ${showToken(alias)}` : "";
    return `memory ${showWasmName(name)}[${limits}]${a}`;
  }
  if ("name" in imp) {
    const { name, alias } = imp.name;
    return alias
      ? `${showToken(name)} as ${showToken(alias)}`
      : showToken(name);
  }
  if ("star" in imp) {
    return `* as ${showToken(imp.star)}`;
  }
  if ("table" in imp) {
    const { name, table_type, min, max, alias } = imp.table;
    const limits = [min!, max!].filter(Boolean).map(showToken).join(", ");
    const a = alias ? ` as ${showToken(alias)}` : "";
    return `table ${showWasmName(name)}: ${showTypeExpression(table_type)}[${limits}]${a}`;
  }
  if ("trait" in imp) {
    const { name, alias } = imp.trait;
    return alias
      ? `${showToken(name)} as ${showToken(alias)}`
      : showToken(name);
  }
  if ("type" in imp) {
    const { name, alias } = imp.type;
    return alias
      ? `${showToken(name)} as ${showToken(alias)}`
      : showToken(name);
  }
  return "";
}

export function showFnSignature(sig: FnSignature): string {
  const { param_types, return_type } = sig;
  return `(${param_types.map(showFnParam).join(", ")}): ${showTypeExpression(return_type)}`;
}

export function showWasmName(name: WasmName): string {
  return showToken(name);
}

export function showTypeExpression(te: TypeExpression): string {
  if (isName(te) || isType(te)) {
    return showToken(te);
  }
  if ("select" in te) {
    const { root, name } = te.select;
    return `${showTypeExpression(root)}.${showToken(name)}`;
  }
  if ("app" in te) {
    const { callee, args } = te.app;
    return `${showTypeExpression(callee)}<${args.map(showTypeExpression).join(", ")}>`;
  }
  if ("fn" in te) {
    const { args, ret } = te.fn;
    return `(${args.map(showTypeExpression).join(", ")}) -> ${showTypeExpression(ret)}`;
  }
  if ("record" in te) {
    return `{ ${te.record.map(showNamedTypeExpression).join(", ")} }`;
  }
  if ("tuple" in te) {
    return `(${te.tuple.map(showTypeExpression).join(", ")})`;
  }
  console.error(te);
  throw new Error("Something is wrong");
}

export function showNamedTypeExpression(nte: NamedTypeExpression): string {
  return `${showToken(nte.name)}: ${showTypeExpression(nte.ty)}`;
}

export function showExpression(expr: Expression): string {
  if (isName(expr) || isType(expr) || isString(expr)) {
    return showToken(expr);
  }
  if ("call" in expr) {
    const [callee, args] = expr.call;
    return `${showExpression(callee)}(${args.map(showExpression).join(", ")})`;
  }
  if ("block" in expr) {
    const body = expr.block.map((b) => `\n  ${showBodyExpression(b)}`).join("");
    return `{${body}\n}`;
  }
  if ("if_expr" in expr) {
    const { cond, if_body, else_body } = expr.if_expr;
    const els = else_body ? ` else ${showExpression(else_body)}` : "";
    return `if ${showExpression(cond)} ${showExpression(if_body)}${els}`;
  }
  if ("select" in expr) {
    const [obj, field] = expr.select;
    return `${showExpression(obj)}.${showToken(field)}`;
  }
  if ("match" in expr) {
    const [val, arms] = expr.match;
    const a = arms.map((arm) => `\n  ${showMatchArm(arm)}`).join("");
    return `match ${showExpression(val)} {${a}\n}`;
  }
  if ("float" in expr) {
    return `${expr.float.value}f${expr.float.size}`;
  }
  if ("int" in expr) {
    return `${expr.int.value}i${expr.int.size}`;
  }
  if ("bool" in expr) {
    return expr.bool.toString();
  }
  if ("prefix" in expr) {
    return `${expr.prefix.op}${showExpression(expr.prefix.operand)}`;
  }
  if ("postfix" in expr) {
    return `${showExpression(expr.postfix.operand)}${expr.postfix.op}`;
  }
  if ("infix" in expr) {
    const { op, left, right } = expr.infix;
    return `${showExpression(left)} ${op} ${showExpression(right)}`;
  }
  if ("fn" in expr) {
    return showFn(expr.fn);
  }
  if ("record" in expr) {
    const fields = expr.record
      .map(([k, v]) => `${showToken(k)}: ${showExpression(v)}`)
      .join(", ");
    return `{ ${fields} }`;
  }
  if ("tuple" in expr) {
    return `(${expr.tuple.map(showExpression).join(", ")})`;
  }
  if ("self" in expr) {
    return "self";
  }
  return "";
}

export function showBodyExpression(body: BodyExpression): string {
  if ("arrow_bind" in body) {
    const { name, expression } = body.arrow_bind;
    return `${showToken(name)} <- ${showExpression(expression)}`;
  }
  if ("let_bind" in body) {
    const { pattern, assert, expression } = body.let_bind;
    const a = assert ? "!" : "";
    return `let${a} ${showPatternExpression(pattern)} = ${showExpression(expression)}`;
  }
  if ("assign" in body) {
    const { name, expression } = body.assign;
    return `${showToken(name)} = ${showExpression(expression)}`;
  }
  if ("expression" in body) {
    return showExpression(body.expression);
  }
  return "";
}

export function showMatchArm(arm: MatchArm): string {
  const { pattern, guard, body } = arm;
  const g = guard ? ` if ${showExpression(guard)}` : "";
  return `${showPatternExpression(pattern)}${g} => ${showExpression(body)}`;
}

export function showPatternExpression(pat: PatternExpression): string {
  if (isName(pat)) {
    return showToken(pat);
  }
  if ("constr" in pat) {
    const { type, patterns } = pat.constr;
    return `${showToken(type)}(${patterns.map(showPatternExpression).join(", ")})`;
  }
  if ("int" in pat) {
    return `${pat.int.value}i${pat.int.size}`;
  }
  if ("float" in pat) {
    return `${pat.float.value}f${pat.float.size}`;
  }
  if ("string" in pat) {
    return `"${pat.string}"`;
  }
  if ("record" in pat) {
    const fields = pat.record
      .map(([k, v]) => `${showToken(k)}: ${showPatternExpression(v)}`)
      .join(", ");
    return `{ ${fields} }`;
  }
  if ("tuple" in pat) {
    return `(${pat.tuple.map(showPatternExpression).join(", ")})`;
  }
  return "";
}

// Token helper functions
export function showToken(
  token: NameToken | TypeToken | StringToken | IntToken,
): string {
  if (isNameToken(token)) return token.name;
  if (isTypeToken(token)) return token.type;
  if (isStringToken(token)) return `"${token.string}"`;
  if (isIntToken(token)) return token.int.toString();
  return "";
}

function isNameToken(t: Token): t is NameToken {
  return t && "name" in t;
}

function isTypeToken(t: Token): t is TypeToken {
  return t && "type" in t;
}

function isStringToken(t: Token): t is StringToken {
  return t && "string" in t;
}

function isIntToken(t: Token): t is IntToken {
  return t && "int" in t;
}
