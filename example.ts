import {
  Context, Kind, Type, Term, typecheck, showType, unitType, unitValue,
  variant_type, con_type, match_term, var_term, variant_pattern, con_term,
  lam_term, wildcard_pattern, var_pattern, record_type, record_pattern,
 } from "./src/types_system_f_omega";
// Example 1: Simple variant matching with wildcard
const optionType = variant_type([
  ["None", unitType],
  ["Some", con_type("Int")],
]);

const unwrap = lam_term(
  "opt",
  optionType,
  match_term(var_term("opt"), [
    [variant_pattern("None", wildcard_pattern()), con_term("0", con_type("Int"))],
    [variant_pattern("Some", var_pattern("x")), var_term("x")],
  ])
);

// Example 2: Record pattern matching
const pointType = record_type([
  ["x", con_type("Int")],
  ["y", con_type("Int")],
]);

const getX = lam_term(
  "p",
  pointType,
  match_term(var_term("p"), [
    [
      record_pattern([
        ["x", var_pattern("xVal")],
        ["y", wildcard_pattern()],
      ]),
      var_term("xVal"),
    ],
  ])
);

// Example 3: Nested pattern matching
const resultType = variant_type([
  ["Ok", con_type("Int")],
  ["Err", con_type("String")],
]);

const nestedOptionResult = variant_type([
  ["None", unitType],
  ["Some", resultType],
]);

const unwrapAll = lam_term(
  "x",
  nestedOptionResult,
  match_term(var_term("x"), [
    [variant_pattern("None", wildcard_pattern()), con_term("-1", con_type("Int"))],
    [
      variant_pattern("Some", variant_pattern("Ok", var_pattern("val"))),
      var_term("val"),
    ],
    [
      variant_pattern("Some", variant_pattern("Err", wildcard_pattern())),
      con_term("-2", con_type("Int")),
    ],
  ])
);

// Example 4: Complex record destructuring
const personType = record_type([
  ["name", con_type("String")],
  ["age", con_type("Int")],
  ["address", record_type([
    ["street", con_type("String")],
    ["city", con_type("String")],
  ])],
]);

const getCity = lam_term(
  "person",
  personType,
  match_term(var_term("person"), [
    [
      record_pattern([
        ["name", wildcard_pattern()],
        ["age", wildcard_pattern()],
        ["address", record_pattern([
          ["street", wildcard_pattern()],
          ["city", var_pattern("c")],
        ])],
      ]),
      var_term("c"),
    ],
  ])
);

// Type check examples
const result1 = typecheck([], unwrap);
console.log("unwrap type:", "ok" in result1 ? showType(result1.ok) : result1.err);

const result2 = typecheck([], getX);
console.log("getX type:", "ok" in result2 ? showType(result2.ok) : result2.err);

const result3 = typecheck([], unwrapAll);
console.log("unwrapAll type:", "ok" in result3 ? showType(result3.ok) : result3.err);

const result4 = typecheck([], getCity);
console.log("getCity type:", "ok" in result4 ? showType(result4.ok) : result4.err);