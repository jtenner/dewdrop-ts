import type { chars } from "./chars.js";

type CharIter = ReturnType<typeof chars>;

const whitespace = (c: string) => /^[\t\r\n ]$/.test(c);
const name_start = (c: string) => /^[\p{Ll}_]$/u.test(c);
const name_cont = (c: string) => /^[\p{Ll}_0-9]$/u.test(c);

const type_start = (c: string) => /^[\p{Lu}]$/u.test(c);
const type_cont = (c: string) => /^[\p{Lu}\p{Ll}_0-9]$/u.test(c);

const zero = (c: string) => c === "0";
const one = (c: string) => c === "1";
const bin = (c: string) => zero(c) || one(c);
const dec = (c: string) => c >= "0" && c <= "9";
const hex = (c: string) =>
  dec(c) || (c >= "a" && c <= "f") || (c >= "A" && c <= "F");
const oct = (c: string) => c >= "0" && c <= "7";
const sign = (c: string) => c === "+" || c === "-";
const e = (c: string) => c === "e" || c === "E";
const dot = (c: string) => c === ".";
const quote = (c: string) => c === `"`;
const symbols = new Set(Array.from("!@#$%^&*()-=+-/<>[]{}:;,.?~|"));
const symbol = (c: string) => symbols.has(c);

const to_num = (c: string) =>
  BigInt(
    dec(c)
      ? c.charCodeAt(0) - "0".charCodeAt(0)
      : c >= "a" && c <= "f"
        ? c.charCodeAt(0) - "a".charCodeAt(0) + 10
        : c >= "A" && c <= "F"
          ? c.charCodeAt(0) - "A".charCodeAt(0) + 10
          : -1,
  );
export type UnknownToken = { unknown: null; position: [number, number] };
export type WhitespaceToken = { whitespace: null; position: [number, number] };
export type IntToken = { int: bigint; position: [number, number] };
export type FloatToken = { float: number; position: [number, number] };
export type NameToken = { name: string; position: [number, number] };
export type TypeToken = { type: string; position: [number, number] };
export type StringToken = { string: string; position: [number, number] };
export type SymbolToken = { symbol: string; position: [number, number] };
export type KeywordToken = { keyword: string; position: [number, number] };
export type Token =
  | UnknownToken
  | WhitespaceToken
  | SymbolToken
  | IntToken
  | FloatToken
  | NameToken
  | TypeToken
  | StringToken
  | KeywordToken;

const take_whitespace = async (
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  while (true) {
    const result = await iter.next();
    if (!result.done && whitespace(result.value[0])) continue;
    return [result.value ?? null, { whitespace: null, position: [line, col] }];
  }
};

const take_name = async (
  first_char: string,
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  let name = first_char;
  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { name, position: [line, col] }];
    const next = result.value;
    if (name_cont(next[0])) {
      name += next[0];
      continue;
    }
    return [next, { name, position: [line, col] }];
  }
};

const take_type = async (
  first_char: string,
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  let type = first_char;
  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { type, position: [line, col] }];
    const next = result.value;
    if (type_cont(next[0])) {
      type += next[0];
      continue;
    }
    return [next, { type, position: [line, col] }];
  }
};

const take_zero = async (
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  let result = await iter.next();
  if (result.done) [null, { int: 0 }];
  const next_char = result.value ?? null;
  let predicate: (c: string) => boolean;
  let stride = 0n;
  if (next_char[0] === ".") {
    return take_after_decimal("0.", iter, line, col);
  } else if (next_char[0] === "b") {
    predicate = bin;
    stride = 2n;
  } else if (next_char[0] === "o") {
    predicate = oct;
    stride = 8n;
  } else if (next_char[0] === "x") {
    predicate = hex;
    stride = 16n;
  } else {
    return [next_char, { int: 0n, position: [line, col] }];
  }

  let value = 0n;

  result = await iter.next();
  if (result.done) return [null, { unknown: null, position: [line, col] }];
  if (predicate(result.value[0]))
    value = value * stride + to_num(result.value[0]);
  else return [result.value, { unknown: null, position: [line, col] }];

  while (true) {
    result = await iter.next();
    if (result.done) return [null, { int: value, position: [line, col] }];

    if (predicate(result.value[0]))
      value = value * stride + to_num(result.value[0]);
    else return [result.value, { int: value, position: [line, col] }];
  }
};

const take_maybe_number = async (
  first_char: string,
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  const result = await iter.next();
  if (result.done) return [null, { symbol: first_char, position: [line, col] }];

  const next_char = result.value;

  if (zero(next_char[0])) {
    const result = await take_zero(iter, line, col);
    if (first_char === "-" && "int" in result[1]) {
      result[1].int *= -1n;
    }
    if (first_char === "-" && "float" in result[1]) {
      result[1].float *= -1;
    }
    return result;
  }
  if (dec(next_char[0]))
    return take_number(first_char + next_char[0], iter, line, col);
  return [next_char, { symbol: first_char, position: [line, col] }];
};

const take_number = async (
  start: string,
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  let acc = start;

  while (true) {
    const result = await iter.next();

    if (result.done) return [null, { int: BigInt(acc), position: [line, col] }];
    const next_char = result.value;
    if (dec(next_char[0])) acc += next_char[0];
    else if (dot(next_char[0])) {
      acc += ".";
      break;
    } else return [next_char, { int: BigInt(acc), position: [line, col] }];
  }

  return take_after_decimal(acc, iter, line, col);
};

const take_after_decimal = async (
  start: string,
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  let acc = start;
  let result = await iter.next();
  if (result.done) return [null, { unknown: null, position: [line, col] }];

  if (dec(result.value[0])) acc += result.value[0];
  else return [result.value, { unknown: null, position: [line, col] }];

  while (true) {
    const result = await iter.next();
    if (result.done)
      return [null, { float: parseFloat(acc), position: [line, col] }];

    const next_char = result.value;
    if (dec(next_char[0])) acc += next_char[0];
    else if (e(next_char[0])) {
      acc += next_char[0];
      break;
    } else
      return [next_char, { float: parseFloat(acc), position: [line, col] }];
  }

  // after "e"
  result = await iter.next();
  if (result.done) return [null, { unknown: null, position: [line, col] }];

  if (sign(result.value[0])) {
    acc += result.value[0];

    result = await iter.next();
    if (result.done) return [null, { unknown: null, position: [line, col] }];
  }

  if (dec(result.value[0])) acc += result.value[0];
  else return [result.value, { unknown: null, position: [line, col] }];

  while (true) {
    const result = await iter.next();
    if (result.done)
      return [null, { float: parseFloat(acc), position: [line, col] }];

    if (dec(result.value[0])) acc += result.value[0];
    else
      return [result.value, { float: parseFloat(acc), position: [line, col] }];
  }
};

const take_string = async (
  iter: CharIter,
  line: number,
  col: number,
): Promise<[[string, number, number] | null, Token]> => {
  let acc = "";

  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { string: acc, position: [line, col] }];

    if (result.value[0] === `"`)
      return [null, { string: acc, position: [line, col] }];

    if (result.value[0] === "\\") {
      const result = await iter.next();
      if (result.done) return [null, { unknown: null, position: [line, col] }];

      switch (result.value[0]) {
        case "0": {
          acc += "\0";
          break;
        }
        case "a": {
          acc += "\u{0007}";
          break;
        }
        case "b": {
          acc += "\u{0008}";
          break;
        }
        case "f": {
          acc += "\f"; // "\u{000C}";
          break;
        }
        case "n": {
          acc += "\n";
          break;
        }
        case "r": {
          acc += "\r";
          break;
        }
        case "t": {
          acc += "\t";
          break;
        }
        case "v": {
          acc += "\v";
          break;
        }
        case "e": {
          acc += "\x1B";
          break;
        }
        case "s": {
          acc += " ";
          break;
        }
        case "x": {
          let v = 0n;
          for (let i = 0; i < 2; i++) {
            const next_char = await iter.next();
            if (next_char.done)
              return [null, { unknown: null, position: [line, col] }];
            if (hex(next_char.value[0]))
              v = v * 16n + to_num(next_char.value[0]);
            else return [null, { unknown: null, position: [line, col] }];
          }

          acc += String.fromCodePoint(Number(v));
          break;
        }

        case "u": {
          let v = 0n;
          for (let i = 0; i < 4; i++) {
            const next_char = await iter.next();
            if (next_char.done)
              return [null, { unknown: null, position: [line, col] }];
            if (hex(next_char.value[0]))
              v = v * 16n + to_num(next_char.value[0]);
            else return [null, { unknown: null, position: [line, col] }];
          }

          acc += String.fromCodePoint(Number(v));
          break;
        }

        case "U": {
          let v = 0n;
          for (let i = 0; i < 8; i++) {
            const next_char = await iter.next();
            if (next_char.done)
              return [null, { unknown: null, position: [line, col] }];
            if (hex(next_char.value[0]))
              v = v * 16n + to_num(next_char.value[0]);
            else return [null, { unknown: null, position: [line, col] }];
          }

          acc += String.fromCodePoint(Number(v));
          break;
        }

        default: {
          acc += result.value[0];
          break;
        }
      }

      continue;
    }
    acc += result.value[0];
  }
};

export async function* lex(
  iter: CharIter,
  keywords: Iterable<string> = [
    "pub",
    "fn",
    "builtin",
    "type",
    "let",
    "impl",
    "trait",
    "inf",
    "nan",
    "import",
    "memory",
    "global",
    "table",
    "mut",
    "as",
    "if",
    "else",
    "match",
    "enum",
    "self",
    "assert",
    "for",
    "rec",
  ],
): AsyncGenerator<Token> {
  const effective_keywords = new Set([...keywords]);
  let next: string | null = null;
  let line: number;
  let col: number;
  let token: Token;
  while (true) {
    if (!next) {
      const result = await iter.next();
      if (result.done) return;
      [next, line, col] = result.value;
    }
    const first_char: string = next;

    if (!first_char) return;
    let ret_next: [string, number, number] | null;
    [ret_next, token] = whitespace(first_char)
      ? await take_whitespace(iter, line!, col!)
      : name_start(first_char)
        ? await take_name(first_char, iter, line!, col!)
        : type_start(first_char)
          ? await take_type(first_char, iter, line!, col!)
          : quote(first_char)
            ? await take_string(iter, line!, col!)
            : sign(first_char)
              ? await take_maybe_number(first_char, iter, line!, col!)
              : symbol(first_char)
                ? [null, { symbol: first_char, position: [line!, col!] }]
                : dec(first_char)
                  ? await take_number(first_char, iter, line!, col!)
                  : [null, { unknown: null, position: [line!, col!] }];
    next = ret_next?.[0] ?? null;
    if ("name" in token && effective_keywords.has(token.name))
      yield { keyword: token.name, position: [line!, col!] };
    else yield token;
  }
}
