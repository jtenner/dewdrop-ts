import { chars, chars_from } from "./chars.js";
import {
  type FloatToken,
  type IntToken,
  lex,
  type NameToken,
  type StringToken,
  type Token,
  type TypeToken,
} from "./lexer.js";
import { type Operator, Yard } from "./yard.js";

type TokenIter = ReturnType<typeof lex>;

export type Module = {
  module: Declaration[];
};

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
    signature: FnSignature | null;
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
export type BuiltinImport = {
  builtin: { name: StringToken; alias: NameIdentifier };
};
export type EnumImport = {
  enum: { name: TypeIdentifier; alias: TypeIdentifier | null };
};
export type Import =
  | TypeImport
  | FnImport
  | GlobalImport
  | EnumImport
  | TableImport
  | StarImport
  | MemoryImport
  | NameImport
  | TraitImport
  | BuiltinImport;

export type TraitFn = {
  name: NameIdentifier;
  params: NamedTypeExpression[];
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

export type Declaration =
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
): Promise<[Token | null, NamedTypeExpression | null]> => {
  let name: NameIdentifier | null = null;
  let success_token: Token | null = null;
  let ty: TypeExpression | null = null;

  [next_token, name] = await take_name(next_token, tokens);
  if (!name) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if (!success_token) return [next_token, null];

  [next_token, ty] = await take_type_expression(next_token, tokens);
  if (!ty) return [next_token, null];

  return [next_token, { name, ty }];
};

export const take_type_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, TypeExpression | null]> => {
  let acc: TypeExpression | null = null;
  let name: NameIdentifier | null = null;
  let success_token: Token | null = null;
  [next_token, name] = await take_name(next_token, tokens);
  if (name) return [next_token, name];

  // function type `(...types) -> TypeExpression`
  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if (success_token) {
    let args: TypeExpression[] | null = null;
    [next_token, args] = await take_list(
      next_token,
      tokens,
      take_type_expression,
      ",",
      ")",
    );
    if (!args) return [next_token, null];

    [next_token, success_token] = await take_symbol(next_token, tokens, "-");
    if (!success_token) return [next_token, null];

    next_token ??= await next(tokens, false);
    if (next_token && "symbol" in next_token && next_token.symbol === ">") {
      let ret: TypeExpression | null = null;
      [next_token, ret] = await take_type_expression(null, tokens);
      if (ret) return [next_token, { fn: { args, ret } }];
    }

    while (next_token && "whitespace" in next_token)
      next_token = await next(tokens);
    return [next_token, null];
  }

  [next_token, success_token] = await take_symbol(next_token, tokens, "#");
  if (success_token) {
    next_token ??= await next(tokens, false);
    // record types can also be chained
    if (next_token && "symbol" in next_token && next_token.symbol === "{") {
      let record: NamedTypeExpression[] | null = null;
      [next_token, record] = await take_list(
        null,
        tokens,
        take_key_value_pair_type_expression,
        ",",
        "}",
      );
      if (!record) return [next_token, null];
      acc = { record };
    } else if (
      next_token &&
      "symbol" in next_token &&
      next_token.symbol === "("
    ) {
      // tuple type cannot be chained
      let tuple: TypeExpression[] | null = null;
      [next_token, tuple] = await take_list(
        null,
        tokens,
        take_type_expression,
        ",",
        ")",
      );

      if (!tuple) return [next_token, null];
      return [next_token, { tuple }];
    }
  }

  // Named Expression
  if (!acc) [next_token, acc] = await take_type(next_token, tokens);
  if (!acc) return [next_token, null];

  while (true) {
    // Select type/name
    [next_token, success_token] = await take_symbol(next_token, tokens, ".");
    if (success_token) {
      let name: Identifier | null = null;
      [next_token, name] = await take_name(next_token, tokens);
      if (!name) [next_token, name] = await take_type(next_token, tokens);
      if (!name) return [next_token, null];

      acc = { select: { root: acc!, name } };
      continue;
    }

    // Type Application
    [next_token, success_token] = await take_symbol(next_token, tokens, "<");
    if (success_token) {
      let args: TypeExpression[] | null = null;
      [next_token, args] = await take_list(
        next_token,
        tokens,
        take_type_expression,
        ",",
        ">",
      );

      if (!args) return [next_token, null];

      acc = { app: { callee: acc, args } };
      continue;
    }

    return [next_token, acc];
  }
};

export const take_record_pattern_arm = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, [NameIdentifier, PatternExpression] | null]> => {
  let success_token: Token | null = null;
  let name: NameIdentifier | null = null;
  let pattern: PatternExpression | null = null;

  [next_token, name] = await take_name(next_token, tokens);
  if (!name) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if (success_token) {
    [next_token, pattern] = await take_pattern_expression(next_token, tokens);
    if (pattern) return [next_token, [name, pattern]];
    return [next_token, null];
  } else return [next_token, [name, name]];
};

export const take_pattern_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, PatternExpression | null]> => {
  let name: NameIdentifier | null = null;
  let type: TypeIdentifier | null = null;
  let success_token: Token | null = null;
  let int: IntToken | null = null;
  let float: FloatToken | null = null;
  let string: StringToken | null = null;
  let patterns: PatternExpression[] | null = null;

  [next_token, name] = await take_name(next_token, tokens);
  if (name) return [next_token, name];

  [next_token, type] = await take_type(next_token, tokens);
  if (type) {
    [next_token, success_token] = await take_symbol(next_token, tokens, "(");

    if (success_token) {
      [next_token, patterns] = await take_list(
        next_token,
        tokens,
        take_pattern_expression,
        ",",
        ")",
      );
      if (!patterns) return [next_token, null];
      return [next_token, { constr: { type, patterns } }];
    } else return [next_token, { constr: { type, patterns: [] } }];
  }

  [next_token, int] = await take_int(next_token, tokens);
  if (int) return [next_token, { int: { size: 32, value: int.int } }];

  [next_token, float] = await take_float(next_token, tokens);
  if (float) return [next_token, { float: { size: 64, value: float.float } }];

  [next_token, string] = await take_string(next_token, tokens);
  if (string) return [next_token, string];

  [next_token, success_token] = await take_keyword(next_token, tokens, "inf");
  if (success_token)
    return [next_token, { float: { size: 64, value: Infinity } }];

  [next_token, success_token] = await take_keyword(next_token, tokens, "nan");
  if (success_token) return [next_token, { float: { size: 64, value: NaN } }];

  [next_token, success_token] = await take_symbol(next_token, tokens, "#");
  if (success_token) {
    next_token ??= await next(tokens, false);
    if (next_token && "symbol" in next_token && next_token.symbol === "(") {
      [next_token, patterns] = await take_list(
        null,
        tokens,
        take_pattern_expression,
        ",",
        ")",
      );
      if (patterns) return [next_token, { tuple: patterns }];
      return [next_token, null];
    }

    if (next_token && "symbol" in next_token && next_token.symbol === "{") {
      let record: [NameIdentifier, PatternExpression][] | null = null;
      [next_token, record] = await take_list(
        null,
        tokens,
        take_record_pattern_arm,
        ",",
        "}",
      );
      if (record) return [next_token, { record }];
      return [next_token, null];
    }

    if (next_token && "whitespace" in next_token)
      next_token = await next(tokens);
    return [next_token, null];
  }

  return [next_token, null];
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
      acc += next_token.symbol;
      continue;
    }

    while (next_token && "whitespace" in next_token)
      next_token = await next(tokens);
    return [next_token, acc];
  }
};

export const take_body_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, BodyExpression | null]> => {
  let success_token: Token | null = null;
  let expression: Expression | null = null;
  let pattern: PatternExpression | null = null;
  let name: NameIdentifier | null = null;
  let op: string | null = null;

  [next_token, name] = await take_name(next_token, tokens);
  if (name) {
    // name
    next_token = await next(tokens);
    // if nothing else is parsed, it's an expression of type "name"
    if (!next_token) return [null, { expression: name }];

    [next_token, op] = await take_operator(next_token, tokens);
    if (!op) {
      const yard = new Yard<Expression>();
      yard.push_expr(name);

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

      if (expression) return [next_token, { arrow_bind: { name, expression } }];
    }

    if (op.startsWith("=")) {
      // assignment
      const next_op = op.slice(1);
      if (next_op === "") {
        // simple assignment
        [next_token, expression] = await take_expression(next_token, tokens);

        if (expression) return [next_token, { assign: { name, expression } }];
      }

      if (next_op in infix_ops) {
        [next_token, expression] = await take_expression(next_token, tokens);
        if (expression) {
          expression = {
            infix: {
              left: name,
              op: next_op as InfixOp,
              right: expression,
            },
          } satisfies Expression;
          const out = {
            assign: { name, expression },
          } satisfies BodyExpression;
          return [next_token, out];
        }
      }
    }

    const infix_op = get_infix_op(op);
    if (infix_op) {
      const yard = new Yard<Expression>();
      yard.push_expr(name);
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
      yard.push_expr(name);
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

  [next_token, success_token] = await take_keyword(next_token, tokens, "let");
  if (success_token) {
    [next_token, success_token] = await take_keyword(
      next_token,
      tokens,
      "assert",
    );
    const assert = !!success_token;

    [next_token, pattern] = await take_pattern_expression(next_token, tokens);
    if (pattern) {
      [next_token, success_token] = await take_symbol(next_token, tokens, "=");
      if (success_token) {
        [next_token, expression] = await take_expression(null, tokens);
        if (expression)
          return [next_token, { let_bind: { pattern, assert, expression } }];
      }
    }
    return [next_token, null];
  }

  [next_token, expression] = await take_expression(next_token, tokens);
  if (expression) return [next_token, { expression }];

  return [next_token, null];
};

export const take_key_value_pair = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, [NameIdentifier, Expression] | null]> => {
  let key: NameIdentifier | null = null;
  let value: Expression | null = null;
  let token_success: Token | null = null;

  [next_token, key] = await take_name(next_token, tokens);
  if (!key) return [next_token, null];

  [next_token, token_success] = await take_symbol(next_token, tokens, ":");
  if (token_success) {
    [next_token, value] = await take_expression(next_token, tokens);
    if (value) return [next_token, [key, value]];
    return [next_token, null];
  } else {
    return [next_token, [key, key]];
  }
};

const take_match_arm = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, MatchArm | null]> => {
  let token_success: Token | null = null;
  let op: string | null = null;
  let pattern: PatternExpression | null = null;
  let guard: Expression | null = null;
  let body: Expression | null = null;

  [next_token, pattern] = await take_pattern_expression(next_token, tokens);
  if (!pattern) return [next_token, null];

  [next_token, token_success] = await take_keyword(next_token, tokens, "if");
  if (token_success) {
    [next_token, guard] = await take_expression(next_token, tokens);
    if (!guard) return [next_token, null];
  }

  // we must find `=>`
  [next_token, op] = await take_operator(next_token, tokens);
  if (op === "=>") {
    [next_token, body] = await take_expression(next_token, tokens);
    if (!body) return [next_token, null];
    return [next_token, { body, guard, pattern }];
  } else return [next_token, null];
};

export const take_expression = async (
  next_token: Token | null,
  tokens: TokenIter,
  yard = new Yard<Expression>(),
  combine_state = false,
): Promise<[Token | null, Expression | null]> => {
  let name: NameIdentifier | null = null;
  let type: TypeIdentifier | null = null;
  let success_token: Token | null = null;
  let op: string | null = null;
  let int: IntToken | null = null;
  let float: FloatToken | null = null;
  let string: StringToken | null = null;

  // match
  // prefix
  // fn
  while (true) {
    if (combine_state) {
      // function call
      [next_token, success_token] = await take_symbol(next_token, tokens, "(");
      if (success_token) {
        let args: Expression[] | null = null;
        [next_token, args] = await take_list(
          next_token,
          tokens,
          take_expression,
          ",",
          ")",
        );
        if (!args) return [next_token, null];

        yard.push_op({ postfix: ["call", (expr) => ({ call: [expr, args] })] });
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
      if (!op) {
        const expr = yard.finalize();
        return [next_token, expr];
      }

      // select expression
      if (op === ".") {
        let name: NameIdentifier | null;
        [next_token, name] = await take_name(next_token, tokens);
        if (!name) return [next_token, null];

        yard.push_op({
          postfix: ["select", (expr) => ({ select: [expr, name] })],
        });
        combine_state = true;
        continue;
      }

      // for binary expressions, switch back to combine = false
      const infix_op = get_infix_op(op);
      if (infix_op) {
        yard.push_op(infix_op);
        combine_state = false;
        continue;
      }

      // for postfix expressions, keep combining
      const postfix_op = get_postfix_op(op);
      if (postfix_op) {
        yard.push_op(postfix_op);
        combine_state = true;
        continue;
      }

      // invalid operator, something went wrong
      return [next_token, null];
    } else {
      // name
      [next_token, name] = await take_name(next_token, tokens);
      if (name) {
        yard.push_expr(name);
        combine_state = true;
        continue;
      }

      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "self",
      );
      if (success_token) {
        yard.push_expr({ self: null });
        combine_state = true;
        continue;
      }

      // constructor
      [next_token, type] = await take_type(next_token, tokens);
      if (type) {
        yard.push_expr(type);
        combine_state = true;
        continue;
      }

      // fn expression
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "fn",
      );
      if (success_token) {
        let fn: Fn | null;
        // pass the fn token into the parser so that it "reads" the fn token
        [next_token, fn] = await take_fn(success_token, tokens);
        if (!fn) return [next_token, null];

        yard.push_expr(fn_expr(fn));
        combine_state = true;
        continue;
      }

      // group expression
      [next_token, success_token] = await take_symbol(next_token, tokens, "(");
      if (success_token) {
        yard.push_group();
        combine_state = false;
        continue;
      }

      // block expression
      [next_token, success_token] = await take_symbol(next_token, tokens, "{");
      if (success_token) {
        let block: BodyExpression[] | null = null;
        [next_token, block] = await take_list(
          next_token,
          tokens,
          take_body_expression,
          ";",
          "}",
        );
        if (!block) return [next_token, null];
        yard.push_expr({ block });
        combine_state = true;
        continue;
      }

      // match expression
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "match",
      );
      if (success_token) {
        let scrutinee: Expression | null = null;
        let match_arms: MatchArm[] | null = null;
        [next_token, scrutinee] = await take_expression(next_token, tokens);
        if (!scrutinee) return [next_token, null];

        // expect "{"
        [next_token, success_token] = await take_symbol(
          next_token,
          tokens,
          "{",
        );
        if (!success_token) return [next_token, null];

        [next_token, match_arms] = await take_list(
          next_token,
          tokens,
          take_match_arm,
          ",",
          "}",
        );
        if (!match_arms || match_arms.length === 0) return [next_token, null];

        // match success, now combine
        yard.push_expr({ match: [scrutinee, match_arms] });
        combine_state = true;
        continue;
      }

      // Infinity
      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "inf",
      );
      if (success_token) {
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
      if (success_token) {
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
      if (success_token) {
        yard.push_expr({ bool: true });
        combine_state = true;
        continue;
      }

      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "false",
      );
      if (success_token) {
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
      if (success_token) {
        let cond: Expression | null = null;
        [next_token, cond] = await take_expression(next_token, tokens);
        if (!cond) return [next_token, null];

        let if_body: Expression | null = null;
        let else_body: Expression | null = null;
        [next_token, if_body] = await take_expression(next_token, tokens);
        if (!if_body) return [next_token, null];

        [next_token, success_token] = await take_keyword(
          next_token,
          tokens,
          "else",
        );
        if (success_token) {
          [next_token, else_body] = await take_expression(next_token, tokens);
          if (!else_body) return [next_token, null];
        }

        return [next_token, { if_expr: { cond, if_body, else_body } }];
      }

      [next_token, int] = await take_int(next_token, tokens);
      if (int) {
        yard.push_expr({ int: { size: 32, value: int.int } });
        combine_state = true;
        continue;
      }

      [next_token, float] = await take_float(next_token, tokens);
      if (float) {
        yard.push_expr({ float: { size: 64, value: float.float } });
        combine_state = true;
        continue;
      }

      [next_token, string] = await take_string(next_token, tokens);
      if (string) {
        yard.push_expr(string);
        combine_state = true;
        continue;
      }

      // Record or Tuple
      [next_token, success_token] = await take_symbol(next_token, tokens, "#");
      if (success_token) {
        next_token = await next(tokens, false);
        if (!next_token) return [null, null];

        // Record term
        if ("symbol" in next_token && next_token.symbol === "{") {
          let record: [NameIdentifier, Expression][] | null = null;
          [next_token, record] = await take_list(
            null,
            tokens,
            take_key_value_pair,
            ",",
            "}",
          );
          if (!record) return [next_token, null];
          combine_state = true;
          yard.push_expr({ record });
          continue;
        }

        // tuple
        if ("symbol" in next_token && next_token.symbol === "(") {
          let tuple: Expression[] | null = null;
          [next_token, tuple] = await take_list(
            null,
            tokens,
            take_expression,
            ",",
            ")",
          );
          if (!tuple) return [next_token, null];

          yard.push_expr({ tuple });
          combine_state = true;
          continue;
        }

        return [next_token, null];
      }

      [next_token, op] = await take_operator(next_token, tokens);
      if (op) {
        const prefix_op = get_prefix_op(op);
        if (prefix_op) {
          yard.push_op(prefix_op);
          combine_state = false;
          continue;
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
    if ("symbol" in next_token && next_token.symbol === sep) {
      next_token = null;
      continue;
    }

    // parse failed, pass the token on
    return [next_token, null];
  }
};

export const take_fn_param = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, FnParam | null]> => {
  let success_token: Token | null;
  let name: NameIdentifier | null = null;
  let guard: TypeExpression | null = null;

  [next_token, name] = await take_name(next_token, tokens);
  if (!name) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if (success_token) {
    [next_token, guard] = await take_type_expression(next_token, tokens);
    if (!guard) return [next_token, null];
  }

  return [next_token, { name, guard }];
};

export const take_import = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, Import | null]> => {
  let success_token: Token | null = null;
  let name: WasmName | null = null;
  let type: TypeIdentifier | null = null;
  let alias: NameIdentifier | null = null;
  let type_alias: TypeIdentifier | null = null;
  let type_expression: TypeExpression | null = null;
  let min: IntToken | null = null;
  let max: IntToken | null = null;

  // fn import
  [next_token, success_token] = await take_keyword(next_token, tokens, "fn");
  if (success_token) {
    let param_types: FnParam[] | null = null;

    [next_token, name] = await take_name(next_token, tokens);
    if (!name) [next_token, name] = await take_string(next_token, tokens);
    if (!name) return [next_token, null];

    // fn ("name"|name) ("(" ...params ")" -> ReturnType):?
    [next_token, success_token] = await take_symbol(next_token, tokens, "(");
    if (success_token) {
      // full signature is now required
      [next_token, param_types] = await take_list(
        next_token,
        tokens,
        take_fn_param,
        ",",
        ")",
      );
      if (!param_types) return [next_token, null];

      [next_token, success_token] = await take_symbol(next_token, tokens, ":");
      if (!success_token) return [next_token, null];

      [next_token, type_expression] = await take_type_expression(
        next_token,
        tokens,
      );
      if (!type_expression) return [next_token, null];

      [next_token, success_token] = await take_keyword(
        next_token,
        tokens,
        "as",
      );
      if (success_token) {
        // alias required now
        [next_token, alias] = await take_name(next_token, tokens);
        if (!alias) return [next_token, null];
      }

      return [
        next_token,
        {
          fn: {
            name,
            signature: { param_types, return_type: type_expression },
            alias,
          },
        },
      ];
    }

    // optional as keyword
    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      // alias required now
      [next_token, alias] = await take_name(next_token, tokens);
      if (!alias) return [next_token, null];
    }

    return [next_token, { fn: { name, signature: null, alias } }];
  }

  // memory import
  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "memory",
  );
  if (success_token) {
    [next_token, name] = await take_name(next_token, tokens);
    if (!name) [next_token, name] = await take_string(next_token, tokens);
    if (!name) return [next_token, null];

    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if (success_token) {
      // take one or two integers
      [next_token, min] = await take_int(next_token, tokens);
      if (!min) return [next_token, null];

      // this is optional.
      [next_token, max] = await take_int(next_token, tokens);
    }

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      [next_token, alias] = await take_name(next_token, tokens);
      if (!alias) return [next_token, null];
    }

    return [next_token, { memory: { name, min, max, alias } }];
  }

  // global import
  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "global",
  );
  if (success_token) {
    let mut = false;
    [next_token, success_token] = await take_keyword(next_token, tokens, "mut");
    mut = !!success_token;

    [next_token, name] = await take_name(next_token, tokens);
    if (!name) [next_token, name] = await take_string(next_token, tokens);
    if (!name) return [next_token, null];

    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if (!success_token) return [next_token, null];

    [next_token, type_expression] = await take_type_expression(
      next_token,
      tokens,
    );
    if (!type_expression) return [next_token, null];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      [next_token, alias] = await take_name(next_token, tokens);
      if (!alias) return [next_token, null];
    }

    return [
      next_token,
      { global: { alias, global_type: type_expression, mut, name } },
    ];
  }

  // table import
  [next_token, success_token] = await take_keyword(next_token, tokens, "table");
  if (success_token) {
    [next_token, name] = await take_name(next_token, tokens);
    if (!name) [next_token, name] = await take_string(next_token, tokens);
    if (!name) return [next_token, null];

    [next_token, success_token] = await take_symbol(next_token, tokens, ":");
    if (!success_token) return [next_token, null];

    // later check to make sure it's a "Table"
    [next_token, type_expression] = await take_type_expression(
      next_token,
      tokens,
    );
    if (!type_expression) return [next_token, null];

    [next_token, min] = await take_int(next_token, tokens);
    if (!min) return [next_token, null];

    // optional
    [next_token, max] = await take_int(next_token, tokens);

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      // required alias now
      [next_token, alias] = await take_name(next_token, tokens);
      if (!alias) return [next_token, null];
    }

    return [
      next_token,
      { table: { alias, max, min, name, table_type: type_expression } },
    ];
  }

  // type import
  [next_token, success_token] = await take_keyword(next_token, tokens, "type");
  if (success_token) {
    [next_token, type] = await take_type(next_token, tokens);
    if (!type) return [next_token, null];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      [next_token, type_alias] = await take_type(next_token, tokens);
      if (!type_alias) return [next_token, null];
    }

    return [next_token, { type: { name: type, alias: type_alias } }];
  }

  // trait import
  [next_token, success_token] = await take_keyword(next_token, tokens, "trait");
  if (success_token) {
    [next_token, type] = await take_type(next_token, tokens);
    if (!type) return [next_token, null];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      [next_token, type_alias] = await take_type(next_token, tokens);
      if (!type_alias) return [next_token, null];
    }

    return [next_token, { trait: { name: type, alias: type_alias } }];
  }

  // builtin import
  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "builtin",
  );
  if (success_token) {
    [next_token, name] = await take_string(next_token, tokens);
    if (!name) return [next_token, null];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (!success_token) return [next_token, null];

    [next_token, alias] = await take_name(next_token, tokens);
    if (!alias) return [next_token, null];

    return [next_token, { builtin: { alias, name } }];
  }

  [next_token, success_token] = await take_keyword(next_token, tokens, "enum");
  if (success_token) {
    [next_token, type] = await take_type(next_token, tokens);
    if (!type) return [next_token, null];

    [next_token, success_token] = await take_keyword(next_token, tokens, "as");
    if (success_token) {
      [next_token, type_alias] = await take_type(next_token, tokens);
      if (!type_alias) return [next_token, null];
    }

    return [next_token, { enum: { name: type, alias: type_alias } }];
  }

  // named import
  [next_token, name] = await take_name(next_token, tokens);
  if (name) {
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

  // failed import
  return [next_token, null];
};

export const take_import_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, Declaration | null]> => {
  let success_token: Token | null;
  let import_from: StringToken | null;
  let imports: Import[] | null = null;

  [next_token, success_token] = await take_keyword(
    next_token,
    tokens,
    "import",
  );
  if (!success_token) return [next_token, null];

  [next_token, import_from] = await take_string(next_token, tokens);
  if (!import_from) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if (!success_token) return [next_token, null];

  [next_token, imports] = await take_list(null, tokens, take_import, ",", "}");
  if (!imports) return [next_token, null];
  return [
    next_token,
    { import_dec: { import_from: import_from.string, imports } },
  ];
};

export const take_fn = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, Fn | null]> => {
  let success_token: Token | null = null;
  let name: NameIdentifier | null = null;
  let type_params: NameIdentifier[] | null = null;
  let params: FnParam[] | null = null;
  let return_type: TypeExpression | null = null;
  let body: Expression | null = null;
  [next_token, success_token] = await take_keyword(next_token, tokens, "fn");
  if (!success_token) return [next_token, null];

  [next_token, name] = await take_name(next_token, tokens);

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if (success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if (!type_params) return [next_token, null];
  } else type_params = [];

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if (!success_token) return [next_token, null];

  [next_token, params] = await take_list(
    next_token,
    tokens,
    take_fn_param,
    ",",
    ")",
  );
  if (!params) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if (success_token) {
    [next_token, return_type] = await take_type_expression(null, tokens);
    if (!return_type) return [next_token, null];
  }

  [next_token, body] = await take_expression(next_token, tokens);
  if (!body) return [next_token, null];

  return [next_token, { body, name, type_params, params, return_type }];
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
): Promise<[Token | null, IntToken | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "int" in next_token) return [null, next_token];
  return [next_token, null];
};

const take_float = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, FloatToken | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "float" in next_token) return [null, next_token];
  return [next_token, null];
};
const take_symbol = async (
  next_token: Token | null,
  tokens: TokenIter,
  kind: string | null = null,
): Promise<[Token | null, { symbol: string } | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "symbol" in next_token) {
    if (!kind) return [null, next_token];
    if (next_token.symbol === kind) return [null, next_token];
  }

  return [next_token, null];
};

const take_keyword = async (
  next_token: Token | null,
  tokens: TokenIter,
  keyword: string | null = null,
): Promise<[Token | null, Token | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "keyword" in next_token && next_token.keyword === keyword)
    return [null, next_token];
  return [next_token, null];
};

const take_name = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, NameIdentifier | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "name" in next_token) return [null, next_token];
  return [next_token, null];
};

const take_type = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, TypeIdentifier | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "type" in next_token) return [null, next_token];
  return [next_token, null];
};

const take_string = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, { string: string } | null]> => {
  next_token ??= await next(tokens);
  if (next_token && "string" in next_token) return [null, next_token];
  return [next_token, null];
};

const take_type_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, TypeDeclaration | null]> => {
  let success_token: Token | null = null;
  let id: TypeIdentifier | null = null;
  let params: NameIdentifier[] | null = null;
  let symbol: Token | null = null;
  let value: TypeExpression | null = null;

  [next_token, success_token] = await take_keyword(next_token, tokens, "type");
  if (!success_token) return [next_token, null];

  [next_token, id] = await take_type(next_token, tokens);
  if (!id) return [next_token, null];

  [next_token, symbol] = await take_symbol(next_token, tokens, "<");
  if (symbol) {
    [next_token, params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if (!params) return [next_token, null];
  } else params = [];

  [next_token, symbol] = await take_symbol(next_token, tokens, "=");
  if (!symbol) return [next_token, null];

  [next_token, value] = await take_type_expression(next_token, tokens);
  if (!value) return [next_token, null];

  return [next_token, { type_dec: { pub: false, id, params, value } }];
};

const take_enum_variant = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, EnumVariant | null]> => {
  let success_token: Token | null = null;
  let id: TypeIdentifier | null = null;

  [next_token, id] = await take_type(next_token, tokens);
  if (!id) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if (success_token) {
    let values: TypeExpression[] | null = null;
    [next_token, values] = await take_list(
      next_token,
      tokens,
      take_type_expression,
      ",",
      ")",
    );
    if (!values) return [next_token, null];
    return [next_token, { values: { id, values } }];
  }

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if (success_token) {
    let fields: NamedTypeExpression[] | null = null;
    [next_token, fields] = await take_list(
      next_token,
      tokens,
      take_key_value_pair_type_expression,
      ",",
      "}",
    );
    if (!fields) return [next_token, null];

    return [next_token, { fields: { id, fields } }];
  }

  return [next_token, { values: { id, values: [] } }];
};

// enum Option<t> { Some(t), None }
const take_enum_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, EnumDeclaration | null]> => {
  let success_token: Token | null = null;
  let id: TypeIdentifier | null = null;
  let type_params: NameIdentifier[] | null = null;
  let variants: EnumVariant[] | null = null;

  [next_token, success_token] = await take_keyword(next_token, tokens, "enum");
  if (!success_token) return [next_token, null];

  // enum type name
  [next_token, id] = await take_type(next_token, tokens);
  if (!id) return [next_token, null];

  // type parameters
  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if (success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if (!type_params) return [next_token, null];
  } else type_params = [];

  // expect "{"
  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if (!success_token) return [next_token, null];

  [next_token, variants] = await take_list(
    next_token,
    tokens,
    take_enum_variant,
    ",",
    "}",
  );
  if (!variants) return [next_token, null];

  return [next_token, { enum: { id, pub: false, type_params, variants } }];
};

const take_many = async <T>(
  next_token: Token | null,
  tokens: TokenIter,
  take: (
    next_token: Token | null,
    tokens: TokenIter,
  ) => Promise<[Token | null, T | null]>,
  until: string,
): Promise<[Token | null, T[] | null]> => {
  let success_token: Token | null = null;
  const results = [] as T[];
  let value: T | null = null;
  while (true) {
    [next_token, success_token] = await take_symbol(next_token, tokens, until);
    if (success_token) return [next_token, results];

    [next_token, value] = await take(next_token, tokens);
    if (value) {
      results.push(value);
      continue;
    }

    return [next_token, null];
  }
};

const take_trait_fn = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, TraitFn | null]> => {
  let success_token: Token | null = null;
  let name: NameIdentifier | null = null;
  let params: NamedTypeExpression[] | null = null;
  let return_type: TypeExpression | null = null;

  [next_token, success_token] = await take_keyword(next_token, tokens, "fn");
  if (!success_token) return [next_token, null];

  [next_token, name] = await take_name(next_token, tokens);
  if (!name) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, "(");
  if (!success_token) return [next_token, null];

  [next_token, params] = await take_list(
    next_token,
    tokens,
    take_key_value_pair_type_expression,
    ",",
    ")",
  );
  if (!params) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if (!success_token) return [next_token, null];

  // take the next token immediately but don't skip whitespace
  if (success_token) {
    [next_token, return_type] = await take_type_expression(next_token, tokens);
    if (!return_type) return [next_token, null];

    return [next_token, { name, params, return_type }];
  }

  if (next_token && "whitespace" in next_token) return [null, null];
  return [next_token, null];
};

const take_impl_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, ImplDeclaration | null]> => {
  let success_token: Token | null = null;
  let name: TypeIdentifier | null = null;
  let type_params: TypeExpression[] | null = null;
  let impl_for: TypeExpression | null = null;
  let fns: Fn[] | null = null;

  [next_token, success_token] = await take_keyword(next_token, tokens, "impl");
  if (!success_token) return [next_token, null];

  [next_token, name] = await take_type(next_token, tokens);
  if (!name) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if (success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_type_expression,
      ",",
      ">",
    );
    if (!type_params) return [next_token, null];
  } else type_params = [];

  [next_token, success_token] = await take_keyword(next_token, tokens, "for");
  if (!success_token) return [next_token, null];

  [next_token, impl_for] = await take_type_expression(next_token, tokens);
  if (!impl_for) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if (!success_token) return [next_token, null];

  [next_token, fns] = await take_many(next_token, tokens, take_fn, "}");
  if (!fns) return [next_token, null];

  return [next_token, { impl: { for: impl_for, name, type_params, fns } }];
};

const take_trait_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, TraitDeclaration | null]> => {
  let success_token: Token | null = null;
  let id: TypeIdentifier | null = null;
  let type_params: NameIdentifier[] | null = null;
  let fns: TraitFn[] | null = null;

  [next_token, success_token] = await take_keyword(next_token, tokens, "trait");
  if (!success_token) return [next_token, null];

  [next_token, id] = await take_type(next_token, tokens);
  if (!id) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, "<");
  if (success_token) {
    [next_token, type_params] = await take_list(
      next_token,
      tokens,
      take_name,
      ",",
      ">",
    );
    if (!type_params) return [next_token, null];
  } else type_params = [];

  [next_token, success_token] = await take_symbol(next_token, tokens, "{");
  if (!success_token) return [next_token, null];

  [next_token, fns] = await take_many(next_token, tokens, take_trait_fn, "}");
  if (!fns) return [next_token, null];

  return [next_token, { trait: { fns, id, pub: false, type_params } }];
};

const take_let_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, LetDeclaration | null]> => {
  let success_token: Token | null = null;
  let id: NameIdentifier | null = null;
  let guard: TypeExpression | null = null;
  let value: Expression | null = null;

  [next_token, success_token] = await take_keyword(next_token, tokens, "let");
  if (!success_token) return [next_token, null];

  [next_token, id] = await take_name(next_token, tokens);
  if (!id) return [next_token, null];

  [next_token, success_token] = await take_symbol(next_token, tokens, ":");
  if (success_token) {
    [next_token, guard] = await take_type_expression(next_token, tokens);
    if (!guard) return [next_token, null];
  }

  [next_token, success_token] = await take_symbol(next_token, tokens, "=");
  if (!success_token) return [next_token, null];

  [next_token, value] = await take_expression(next_token, tokens);
  if (!value) return [next_token, value];

  return [next_token, { let_dec: { guard, id, pub: false, value } }];
};

const take_fn_declaration = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, FnDeclaration | null]> => {
  let fn: Fn | null = null;

  [next_token, fn] = await take_fn(next_token, tokens);
  if (!fn || !fn.name) return [next_token, null];

  return [next_token, { fn: { pub: false, fn } }];
};

const take_pub = async (
  next_token: Token | null,
  tokens: TokenIter,
): Promise<DeclarationResult> => {
  let success_token: Token | null = null;
  let decl: Declaration | null = null;
  [next_token, success_token] = await take_keyword(next_token, tokens, "pub");
  if (!success_token) return [next_token, null];

  [next_token, decl] = await take_fn_declaration(next_token, tokens);
  if (decl) {
    decl.fn.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_type_declaration(next_token, tokens);
  if (decl) {
    decl.type_dec.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_let_declaration(next_token, tokens);
  if (decl) {
    decl.let_dec.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_enum_declaration(next_token, tokens);
  if (decl) {
    decl.enum.pub = true;
    return [next_token, decl];
  }

  [next_token, decl] = await take_trait_declaration(next_token, tokens);
  if (decl) {
    decl.trait.pub = true;
    return [next_token, decl];
  }

  return [await consume_until_keyword_or_semicolon(tokens), null];
};

export const parse = async (text: string) => ({
  module: await Array.fromAsync(parse_tokens_streaming(lex(chars(text)))),
});

export const parse_file = async (file: string) => ({
  module: await Array.fromAsync(parse_tokens_streaming(lex(chars_from(file)))),
});

const parse_tokens_streaming = async function* (tokens: TokenIter) {
  let next_token: Token | null = null;
  let declaration: Declaration | null = null;

  while (true) {
    if (!next_token) {
      const result = await next(tokens);
      if (!result) return;
      next_token = result;
    }

    [next_token, declaration] = await take_declaration(next_token, tokens);
    if (declaration) yield declaration;
  }
};

export const parse_streaming = (text: string) =>
  parse_tokens_streaming(lex(chars(text)));
export const parse_file_streaming = (file: string) =>
  parse_tokens_streaming(lex(chars_from(file)));

export async function take_declaration(
  next_token: Token | null,
  tokens: TokenIter,
): Promise<[Token | null, Declaration | null]> {
  let decl: Declaration | null;
  [next_token, decl] = await take_pub(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_fn_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_enum_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_type_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_let_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_trait_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_impl_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  [next_token, decl] = await take_import_declaration(next_token, tokens);
  if (decl) return [next_token, decl];

  next_token = await consume_until_keyword_or_semicolon(tokens);
  return [next_token, null];
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
