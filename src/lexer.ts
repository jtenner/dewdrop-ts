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
export type IntToken = { int: bigint };
export type FloatToken = { float: number };
export type NameToken = { name: string };
export type TypeToken = { type: string };
export type StringToken = { string: string };
export type Token =
  | { unknown: null }
  | { whitespace: null }
  | { symbol: string }
  | IntToken
  | FloatToken
  | NameToken
  | TypeToken
  | StringToken
  | { keyword: string };

const take_whitespace = async (
  iter: CharIter,
): Promise<[string | null, Token]> => {
  while (true) {
    const result = await iter.next();
    if (!result.done && whitespace(result.value)) continue;
    return [result.value ?? null, { whitespace: null }];
  }
};

const take_name = async (
  first_char: string,
  iter: CharIter,
): Promise<[string | null, Token]> => {
  let name = first_char;
  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { name }];
    const next = result.value;
    if (name_cont(next)) {
      name += next;
      continue;
    }
    return [next, { name }];
  }
};

const take_type = async (
  first_char: string,
  iter: CharIter,
): Promise<[string | null, Token]> => {
  let type = first_char;
  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { type }];
    const next = result.value;
    if (type_cont(next)) {
      type += next;
      continue;
    }
    return [next, { type }];
  }
};

const take_zero = async (iter: CharIter): Promise<[string | null, Token]> => {
  let result = await iter.next();
  if (result.done) [null, { int: 0 }];
  const next_char = result.value ?? null;
  let predicate: (c: string) => boolean;
  let stride = 0n;
  if (next_char === ".") {
    return take_after_decimal("0.", iter);
  } else if (next_char === "b") {
    predicate = bin;
    stride = 2n;
  } else if (next_char === "o") {
    predicate = oct;
    stride = 8n;
  } else if (next_char === "x") {
    predicate = hex;
    stride = 16n;
  } else {
    return [next_char, { int: 0n }];
  }

  let value = 0n;

  result = await iter.next();
  if (result.done) return [null, { unknown: null }];
  if (predicate(result.value)) value = value * stride + to_num(result.value);
  else return [result.value, { unknown: null }];

  while (true) {
    result = await iter.next();
    if (result.done) return [null, { int: value }];

    if (predicate(result.value)) value = value * stride + to_num(result.value);
    else return [result.value, { int: value }];
  }
};

const take_maybe_number = async (
  first_char: string,
  iter: CharIter,
): Promise<[string | null, Token]> => {
  const result = await iter.next();
  if (result.done) return [null, { symbol: first_char }];

  const next_char = result.value;

  if (zero(next_char)) {
    const result = await take_zero(iter);
    if (first_char === "-" && "int" in result[1]) {
      result[1].int *= -1n;
    }
    if (first_char === "-" && "float" in result[1]) {
      result[1].float *= -1;
    }
    return result;
  }
  if (dec(next_char)) return take_number(first_char + next_char, iter);
  return [next_char, { symbol: first_char }];
};

const take_number = async (
  start: string,
  iter: CharIter,
): Promise<[string | null, Token]> => {
  let acc = start;

  while (true) {
    const result = await iter.next();

    if (result.done) return [null, { int: BigInt(acc) }];
    const next_char = result.value;
    if (dec(next_char)) acc += next_char;
    else if (dot(next_char)) {
      acc += ".";
      break;
    } else return [next_char, { int: BigInt(acc) }];
  }

  return take_after_decimal(acc, iter);
};

const take_after_decimal = async (
  start: string,
  iter: CharIter,
): Promise<[string | null, Token]> => {
  let acc = start;
  let result = await iter.next();
  if (result.done) return [null, { unknown: null }];

  if (dec(result.value)) acc += result.value;
  else return [result.value, { unknown: null }];

  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { float: parseFloat(acc) }];

    const next_char = result.value;
    if (dec(next_char)) acc += next_char;
    else if (e(next_char)) {
      acc += next_char;
      break;
    } else return [next_char, { float: parseFloat(acc) }];
  }

  // after "e"
  result = await iter.next();
  if (result.done) return [null, { unknown: null }];

  if (sign(result.value)) {
    acc += result.value;

    result = await iter.next();
    if (result.done) return [null, { unknown: null }];
  }

  if (dec(result.value)) acc += result.value;
  else return [result.value, { unknown: null }];

  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { float: parseFloat(acc) }];

    if (dec(result.value)) acc += result.value;
    else return [result.value, { float: parseFloat(acc) }];
  }
};

const take_string = async (iter: CharIter): Promise<[string | null, Token]> => {
  let acc = "";

  while (true) {
    const result = await iter.next();
    if (result.done) return [null, { string: acc }];

    if (result.value === `"`) return [null, { string: acc }];

    if (result.value === "\\") {
      const result = await iter.next();
      if (result.done) return [null, { unknown: null }];

      switch (result.value) {
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
            if (next_char.done) return [null, { unknown: null }];
            if (hex(next_char.value)) v = v * 16n + to_num(next_char.value);
            else return [null, { unknown: null }];
          }

          acc += String.fromCodePoint(Number(v));
          break;
        }

        case "u": {
          let v = 0n;
          for (let i = 0; i < 4; i++) {
            const next_char = await iter.next();
            if (next_char.done) return [null, { unknown: null }];
            if (hex(next_char.value)) v = v * 16n + to_num(next_char.value);
            else return [null, { unknown: null }];
          }

          acc += String.fromCodePoint(Number(v));
          break;
        }

        case "U": {
          let v = 0n;
          for (let i = 0; i < 8; i++) {
            const next_char = await iter.next();
            if (next_char.done) return [null, { unknown: null }];
            if (hex(next_char.value)) v = v * 16n + to_num(next_char.value);
            else return [null, { unknown: null }];
          }

          acc += String.fromCodePoint(Number(v));
          break;
        }

        default: {
          acc += result.value;
          break;
        }
      }

      continue;
    }
    acc += result.value;
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
    "trait",
    "inf",
    "nan",
    "import",
    "memory",
    "global",
    "as",
    "if",
    "else",
    "match",
    "true",
    "false",
    "enum",
  ],
) {
  const effective_keywords = new Set([...keywords]);
  let next: string | null = null;
  let token: Token;
  while (true) {
    if (!next) {
      const result = await iter.next();
      if (result.done) return;
      next = result.value;
    }
    const first_char: string = next!;

    if (!first_char) return;
    [next, token] = whitespace(first_char)
      ? await take_whitespace(iter)
      : name_start(first_char)
        ? await take_name(first_char, iter)
        : type_start(first_char)
          ? await take_type(first_char, iter)
          : quote(first_char)
            ? await take_string(iter)
            : sign(first_char)
              ? await take_maybe_number(first_char, iter)
              : symbol(first_char)
                ? [null, { symbol: first_char }]
                : dec(first_char)
                  ? await take_number(first_char, iter)
                  : [null, { unknown: null }];

    if ("name" in token && effective_keywords.has(token.name))
      yield { keyword: token.name };
    else yield token;
  }
}
