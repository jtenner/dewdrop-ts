import { chars } from "./chars.js";

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
const e = (c: string) => c === "e";
const dot = (c: string) => c === ".";
const quote = (c: string) => c === `"`;
const symbols = new Set(Array.from("!@#$%^&*()-=+-/\\<>[]{}:;,.?~|"));
const symbol = (c: string) => symbols.has(c);

const to_num = (c: string) =>
  dec(c)
    ? c.charCodeAt(0) - "0".charCodeAt(0)
    : c >= "a" && c <= "f"
      ? c.charCodeAt(0) - "a".charCodeAt(0) + 10
      : c >= "A" && c <= "F"
        ? c.charCodeAt(0) - "A".charCodeAt(0) + 10
        : -1;

export type Token =
  | { unknown: null }
  | { whitespace: null }
  | { symbol: string }
  | { int: bigint }
  | { float: number }
  | { name: string }
  | { type: string }
  | { string: string };

const take_whitespace = async (iter: CharIter) => {
  while (true) {
    const result = await iter.next();
    if (!result.done && whitespace(result.value)) continue;
    return { whitespace: null };
  }
};

const take_name = async (first_char: string, iter: CharIter) => {
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

const take_type = async (first_char: string, iter: CharIter) => {
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

const take_int_kind = async (start: string, iter: CharIter) => {
  let result = await iter.next();
  if (result.done) [null, { int: parseInt(start, 10) }];
  const next_char = result.value;
  let predicate: (c: string) => boolean;
  let stride = 0;
  if (next_char === "b") {
    predicate = bin;
    stride = 2;
  } else if (next_char === "o") {
    predicate = oct;
    stride = 8;
  } else if (next_char === "x") {
    predicate = hex;
    stride = 16;
  } else {
    return [next_char, { int: parseInt(start, 10) }];
  }

  let value = 0;

  result = await iter.next();
  if (result.done) return [null, { error: null }];
  if (predicate(result.value)) value = value * stride + to_num(result.value);
  else return [result.value, { error: null }];

  while (true) {
    result = await iter.next();
    if (result.done) return [null, { int: value }];

    if (predicate(result.value)) value = value * stride + to_num(result.value);
    else return [result.value, { int: value }];
  }
};

const take_maybe_number = async (first_char: string, iter: CharIter) => {
  const result = await iter.next();
  if (result.done) return [null, { symbol: first_char }];

  const next_char = result.value;

  if (zero(next_char)) return take_int_kind(first_char + next_char, iter);
  if (dec(next_char)) return take_number(first_char + next_char, iter);
  return [next_char, { symbol: first_char }];
};

export async function* lex(iter: CharIter, keywords = new Set<string>()) {
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
        ? take_name(first_char, iter)
        : type_start(first_char)
          ? take_type(first_char, iter)
          : quote(first_char)
            ? sign(first_char)
            : take_maybe_number(first_char, iter)
              ? take_string(iter)
              : symbol(iter)
                ? { symbol: first_char }
                : dec(first_char)
                  ? take_int(iter)
                  : [null, { unknown: null }];
    if ("name" in token && keywords.has(token.name))
      yield { keyword: token.name };
    else yield token;
  }
}

const next_token = async (
  start: string,
  iter: CharIter,
): Promise<[string | null, Token]> => {};
