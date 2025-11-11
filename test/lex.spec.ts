import { describe, expect, test } from "bun:test";
import { chars } from "../src/chars.js";
import { lex } from "../src/lexer.js";

const strOf = (reg: RegExp) => {
  let acc = "";
  for (let i = 0; i < 0x10ffff; i++) {
    try {
      const char = String.fromCodePoint(i);
      if (reg.test(char)) acc += char;
    } catch (_) {}
  }
  return acc;
};

describe("lexer tokenkinds", () => {
  test.each([
    { token: `name` },
    { token: `_name_with_underscore` },
    { token: `name_with_numbers_393` },
    { token: `a` },
    { token: `a${strOf(/[\p{Ll}_0-9]/u)}` },
  ])("$token is a name token", async ({ token }) => {
    const output = await Array.fromAsync(lex(chars(token)));

    expect(output, `Should be: { name: "${token}" }`).toMatchSnapshot(
      `Name: ${token}`,
    );
  });

  test.each([
    { token: `Name` },
    { token: `NameWithMultiple` },
    { token: `Name_With_Underscores` },
    { token: `Name_With_Numbers_3494892` },
    { token: `A${strOf(/[\p{Ll}\p{Lu}_0-9]/u)}` },
  ])("$token is a type token", async ({ token }) => {
    const output = await Array.fromAsync(lex(chars(token)));
    expect(output, `Should be: { type: "${token}" }`).toMatchSnapshot(
      `Token: ${token}`,
    );
  });

  test("symbols", async () => {
    const output = await Array.fromAsync(
      lex(chars("!@#$%^&*()-=+-/<>[]{}:;,.?~|")),
    );

    expect(output).toMatchInlineSnapshot(`
      [
        {
          "position": [
            1,
            1,
          ],
          "symbol": "!",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "@",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "#",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "$",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "%",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "^",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "&",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "*",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "(",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": ")",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "-",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "=",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "+",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "-",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "/",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "<",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": ">",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "[",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "]",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "{",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "}",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": ":",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": ";",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": ",",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": ".",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "?",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "~",
        },
        {
          "position": [
            1,
            1,
          ],
          "symbol": "|",
        },
      ]
    `);
  });

  test.each([
    // Basic decimal numbers
    { token: "0.0" },
    { token: "1.0" },
    { token: "42.123" },
    { token: "0.0001" },
    { token: "0.123456789" },
    // Positive and negative numbers
    { token: "+42.0" },
    { token: "-42.0" },
    { token: "+0.00001" },
    { token: "-0.00001" },
    // Scientific notation (e/E with optional + or -)
    { token: "123.456e7" },
    { token: "-123.456e-7" },
    { token: "+0.000123e+5" },
    { token: "5.0e0" },
    // Edge cases
    { token: "-0.0" }, // Negative zero },
    { token: "+0.0" }, // Positive zero },
    { token: "1.0e0" },
    { token: "-1.0e0" },
    { token: "1.0e308" }, // Near max safe value },
    { token: "-1.0e308" },
    { token: "1.0e-308" }, // Near min safe value },
    { token: "-1.0e-308" },
    // Large and small exponents
    { token: "2.5e100" },
    { token: "-2.5e100" },
    { token: "2.5e-100" },
    { token: "+2.5e+100" },
    { token: "1.23456789e20" },
    { token: "-1.23456789e-20" },
    // Mixed formats
    { token: "123.456e-5" },
    { token: "-123.456e+5" },
    { token: "+0.000001e10" },
    { token: "0.0000001e-10" },
  ])("$token should be a float", async ({ token }) => {
    const output = await Array.fromAsync(lex(chars(token)));
    expect(output).toMatchSnapshot(`Token: ${token}`);
  });

  test.each([
    // // Basic escape sequences for specified characters
    { token: '"\\a"', expected: "\u{0007}" }, // Escaped 'a' (alert or custom escape)
    { token: '"\\0"', expected: "\0" }, // Escaped null character
    { token: '"\\b"', expected: "\u{0008}" }, // Backspace
    { token: '"\\f"', expected: "\f" }, // Form feed
    { token: '"\\n"', expected: "\n" }, // Newline
    { token: '"\\r"', expected: "\r" }, // Carriage return
    { token: '"\\t"', expected: "\t" }, // Tab
    { token: '"\\v"', expected: "\v" }, // Vertical tab
    { token: '"\\e"', expected: "\x1B" }, // Escape character (non-standard, but included per request)
    { token: '"\\s"', expected: " " }, // Space (custom escape for space)

    // // Hex escape sequences
    { token: '"\\x41"', expected: "\x41" }, // 'A' (ASCII 41 in hex)
    { token: '"\\x20"', expected: "\x20" }, // Space (ASCII 20 in hex)
    { token: '"\\x7F"', expected: "\x7F" }, // DEL character (ASCII 7F in hex)
    { token: '"\\x00"', expected: "\x00" }, // Null character (ASCII 00 in hex)

    // // Unicode escape sequences (4-character)
    { token: '"\\u0041"', expected: "\u0041" }, // 'A' (Unicode U+0041)
    { token: '"\\u0020"', expected: "\u0020" }, // Space (Unicode U+0020)
    { token: '"\\uFFFF"', expected: "\uFFFF" }, // Max 4-digit Unicode (U+FFFF)
    { token: '"\\\0"', expected: "\0" }, // Null character (U+0000)

    // // Extended Unicode escape sequences (8-character)
    { token: '"\\U00000041"', expected: "\u{00000041}" }, // 'A' (Unicode U+00000041)
    { token: '"\\U00000020"', expected: "\u{00000020}" }, // Space (Unicode U+00000020)
    { token: '"\\U0010FFFF"', expected: "\u{0010FFFF}" }, // Max Unicode code point (U+10FFFF)
    { token: '"\\U00000000"', expected: "\u{00000000}" }, // Null character (U+00000000)

    // // Escaped backslash
    { token: '"\\\\"', expected: "\\" }, // Literal backslash
    { token: '"\\\\\\""', expected: '\\"' }, // Backslash followed by quote: \"
    { token: '"\\\\\\\\"', expected: "\\\\" }, // Double backslash: \\

    // // Backslash followed by non-escape characters (literal)
    { token: '"\\q"', expected: "q" }, // Backslash followed by 'q' (literal 'q')
    { token: '"\\1"', expected: "1" }, // Backslash followed by '1' (literal '1')
    { token: '"\\z"', expected: "z" }, // Backslash followed by 'z' (literal 'z')
    { token: '"\\@"', expected: "@" }, // Backslash followed by '@' (literal '@')

    // // Mixed escape sequences
    { token: '"Hello\\nWorld"', expected: "Hello\nWorld" }, // Newline between words
    { token: '"Tab\\tSpace\\s"', expected: "Tab\tSpace " }, // Tab and space
    { token: '"Unicode\\u0041Hex\\x41"', expected: "UnicodeAHexA" }, // Mixing Unicode and hex for 'A'
    { token: '"\\U00000041\\u0041\\x41"', expected: "AAA" }, // Mixing 8-char Unicode, 4-char Unicode, and hex for 'A'
    { token: '"Line1\\r\\nLine2"', expected: "Line1\r\nLine2" }, // Carriage return and newline
    { token: '"\\b\\t\\n\\f\\r"', expected: "\b\t\n\f\r" }, // Multiple control characters
    { token: '"Escaped\\sspace\\s\\x20"', expected: "Escaped space \x20" }, // Multiple ways to represent space

    // // Edge cases
    { token: '""', expected: "" }, // Empty string
    { token: '"\\""', expected: '"' }, // Escaped double quote
    { token: '"Hello\\\\World"', expected: "Hello\\World" }, // Escaped backslash in the middle

    // // Strings with literal characters after backslash
    { token: '"\\a\\b\\c"', expected: "\x07\bc" }, // Mix of valid and invalid escapes
    { token: '"Test\\z\\q\\9"', expected: "Testzq9" }, // Multiple non-escape characters
    { token: '"\\x41\\y42"', expected: "Ay42" }, // Valid hex followed by invalid escape
    { token: '"\\u0041\\z0042"', expected: "Az0042" }, // Valid Unicode followed by invalid escape
  ])("$token should match $expected", async ({ token, expected }) => {
    const output = await Array.fromAsync(lex(chars(token)));

    expect(output, `Should be ${expected}`).toMatchSnapshot(`Token: ${token}`);
  });

  test.each([
    { token: '"\\xGG"' }, // Invalid hex (non-hex digits)
    { token: '"\\u12GH"' }, // Invalid Unicode (non-hex digits)
    { token: '"\\U12GH34JK"' }, // Invalid 8-char Unicode (non-hex digits)
    { token: '"\\x"' }, // Incomplete hex escape
    { token: '"\\u"' }, // Incomplete Unicode escape
    { token: '"\\U"' }, // Incomplete 8-char Unicode escape
    { token: '"\\x1"' }, // Incomplete hex (one digit)
    { token: '"\\u123"' }, // Incomplete Unicode (three digits)
    { token: '"\\U1234567"' }, // Incomplete 8-char Unicode (seven digits)
  ])("$token is invalid string", async ({ token }) => {
    const output = await Array.fromAsync(lex(chars(token)));
    expect(output.length).toBeGreaterThan(0);
    expect("unknown" in output[0]).toBeTrue();
  });

  test.each([
    { token: "0", expected: 0n },
    { token: "123456", expected: 123456n },
    { token: "-2000", expected: -2000n },
    { token: "99999999999", expected: 99999999999n },
  ])("$token should be an integer", async ({ token, expected }) => {
    const output = await Array.fromAsync(lex(chars(token)));

    expect(output, `Should be ${expected}`).toMatchSnapshot(`Token: ${token}`);
  });

  test("keywords", async () => {
    const input = `
    one
    two
    red
    blue
    fish
    dish
    glue
    fn
    type
    trait
    `;
    const output = await Array.fromAsync(
      lex(chars(input), ["fn", "type", "trait"]),
    );
    expect(output).toMatchInlineSnapshot(`
      [
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "one",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "two",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "red",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "blue",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "fish",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "dish",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "name": "glue",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "keyword": "fn",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "keyword": "type",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
        {
          "keyword": "trait",
          "position": [
            2,
            2,
          ],
        },
        {
          "position": [
            2,
            2,
          ],
          "whitespace": null,
        },
      ]
    `);
  });
});
