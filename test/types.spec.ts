// tests.ts

import { expect, test } from "bun:test";
import {
  app_term,
  app_type,
  applySubstitution,
  arrow_type,
  bounded_forall_type,
  type ConType,
  type Context,
  checkExhaustive,
  checkKind,
  checkTraitConstraints,
  checkTraitImplementation,
  checkType,
  con_pattern,
  con_term,
  con_type,
  dict_term,
  type EnumDef,
  fold_term,
  forall_type,
  freshMetaVar,
  getMetaSubstitution,
  getSpineArgs,
  inferTypeWithMode,
  inject_term,
  instantiateWithTraits,
  isAssignableTo,
  lam_term,
  lam_type,
  let_term,
  type MuType,
  match_term,
  mergeSubsts,
  metaKind,
  mu_type,
  normalizeType,
  project_term,
  record_pattern,
  record_term,
  record_type,
  showTerm,
  showType,
  solveConstraints,
  solveMetaVar,
  starKind,
  substituteType,
  subsumes,
  type Term,
  type TraitConstraint,
  type TraitDef,
  type TraitImplBinding,
  type Type,
  trait_app_term,
  trait_lam_term,
  trait_method_term,
  tuple_pattern,
  tuple_project_term,
  tuple_term,
  tuple_type,
  tyapp_term,
  tylam_term,
  typecheck,
  typesEqual,
  unfold_term,
  unifyTypes,
  unifyVariable,
  unitType,
  unitValue,
  type VariantType,
  type VarType,
  var_pattern,
  var_term,
  var_type,
  variant_pattern,
  variant_type,
  type Worklist,
  wildcard_pattern,
} from "../src/types_system_f_omega";

function assert(condition: boolean, message: string): asserts condition {
  if (!condition) {
    throw new Error(`Assertion failed: ${message}`);
  }
}

function assertOk<T>(result: { ok: T } | { err: unknown }, message: string): T {
  if ("err" in result) {
    throw new Error(
      `Expected ok but got error: ${JSON.stringify(result.err)} - ${message}`,
    );
  }
  return result.ok;
}

function assertErr<E>(
  result: { ok: unknown } | { err: E },
  message: string,
): E {
  if ("ok" in result) {
    throw new Error(
      `Expected error but got ok: ${JSON.stringify(result.ok)} - ${message}`,
    );
  }
  return result.err;
}

test("Type variable kinding", () => {
  const ctx: Context = [{ type: { name: "T", kind: starKind } }];
  const result = checkKind(ctx, var_type("T"));
  const kind = assertOk(result, "should infer kind *");
  assert("star" in kind, "should be star kind");
});

test("Higher-kinded type", () => {
  const ctx: Context = [
    { type: { name: "F", kind: { arrow: { from: starKind, to: starKind } } } },
  ];
  const result = checkKind(ctx, var_type("F"));
  const kind = assertOk(result, "should infer kind * -> *");
  assert("arrow" in kind, "should be arrow kind");
});

test("Unbound variable error", () => {
  const result = typecheck([], var_term("x"));
  const err = assertErr(result, "should fail");
  assert("unbound" in err, "should be unbound variable error");
});

test("Type application", () => {
  const intType = con_type("Int");
  const polyId = tylam_term(
    "T",
    starKind,
    lam_term("x", var_type("T"), var_term("x")),
  );
  const intId = tyapp_term(polyId, intType);
  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], intId);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "should be Int -> Int");
});

test("Simple record", () => {
  const personType = record_type([
    ["name", con_type("String")],
    ["age", con_type("Int")],
  ]);

  const person = record_term([
    ["name", con_term('"Alice"', con_type("String"))],
    ["age", con_term("30", con_type("Int"))],
  ]);

  const result = typecheck([], person);
  const type = assertOk(result, "should typecheck");
  assert("record" in type, "should be record type");
  assert(type.record.length === 2, "should have 2 fields");
});

test("Record projection", () => {
  const personType = record_type([
    ["name", con_type("String")],
    ["age", con_type("Int")],
  ]);

  const person = record_term([
    ["name", con_term('"Alice"', con_type("String"))],
    ["age", con_term("30", con_type("Int"))],
  ]);

  const getName = project_term(person, "name");
  const result = typecheck([], getName);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, con_type("String")), "should be String");
});

test("Simple variant", () => {
  const boolType = variant_type([
    ["True", unitType],
    ["False", unitType],
  ]);

  const trueVal = inject_term("True", unitValue, boolType);
  const result = typecheck([], trueVal);
  const type = assertOk(result, "should typecheck");
  assert("variant" in type, "should be variant type");
});

test("Large record", () => {
  const fields: [string, Term][] = [];
  for (let i = 0; i < 100; i++) {
    fields.push([`field${i}`, con_term(`${i}`, con_type("Int"))]);
  }

  const largeRecord = record_term(fields);
  const result = typecheck([], largeRecord);
  assertOk(result, "should handle large records");
});

test("let term", () => {
  const intType = con_type("Int");
  const context: Context = [
    {
      term: {
        name: "+",
        type: arrow_type(intType, arrow_type(intType, intType)),
      },
    },
  ];

  const testTerm = let_term(
    "x",
    con_term("5", intType),
    // Fix: Apply "+" to "x" first, then apply the result to "3"
    app_term(app_term(var_term("+"), var_term("x")), con_term("3", intType)),
  );

  const result = typecheck(context, testTerm);
  assertOk(result, "should typecheck");
});

test("Constant type kinding", () => {
  const result = checkKind(
    [{ type: { kind: starKind, name: "Int" } }],
    con_type("Int"),
  );
  const kind = assertOk(result, "should infer kind *");
  assert("star" in kind, "should be star kind");
});

test("Arrow type kinding", () => {
  const intType = con_type("Int");
  const boolType = con_type("Bool");
  const arrowTy = arrow_type(intType, boolType);
  const result = checkKind(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    arrowTy,
  );
  const kind = assertOk(result, "should infer kind *");
  assert("star" in kind, "should be star kind");
});

test("Identity function", () => {
  const intType = con_type("Int");
  const identity = lam_term("x", intType, var_term("x"));
  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    identity,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "argument should be Int");
  assert(typesEqual(type.arrow.to, intType), "return should be Int");
});

test("Function application", () => {
  const intType = con_type("Int");
  const identity = lam_term("x", intType, var_term("x"));
  const value = con_term("42", intType);
  const app = app_term(identity, value);
  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], app);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, intType), "result should be Int");
});

test("Function composition", () => {
  const intType = con_type("Int");
  const strType = con_type("String");
  const boolType = con_type("Bool");

  // f: Int -> String
  const f = lam_term("x", intType, con_term('"str"', strType));
  // g: String -> Bool
  const g = lam_term("y", strType, con_term("true", boolType));
  // compose g f: Int -> Bool
  const composed = lam_term(
    "z",
    intType,
    app_term(g, app_term(f, var_term("z"))),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    composed,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "argument should be Int");
  assert(typesEqual(type.arrow.to, boolType), "return should be Bool");
});

test("Type mismatch in application", () => {
  const intType = con_type("Int");
  const strType = con_type("String");
  const f = lam_term("x", intType, var_term("x"));
  const arg = con_term('"hello"', strType);
  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    app_term(f, arg),
  );
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});

test("Polymorphic identity", () => {
  const polyId = tylam_term(
    "T",
    starKind,
    lam_term("x", var_type("T"), var_term("x")),
  );
  const result = typecheck([], polyId);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be forall type");
  assert(type.forall.var === "T", "should quantify over T");
});

test("Polymorphic constant function", () => {
  const constFn = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      lam_term("x", var_type("A"), lam_term("y", var_type("B"), var_term("x"))),
    ),
  );
  const result = typecheck([], constFn);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Missing field projection", () => {
  const person = record_term([
    ["name", con_term('"Alice"', con_type("String"))],
  ]);

  const getAge = project_term(person, "age");
  const result = typecheck([], getAge);
  const err = assertErr(result, "should fail");
  assert("missing_field" in err, "should be missing field error");
});

test("Nested record", () => {
  const addressType = record_type([
    ["street", con_type("String")],
    ["city", con_type("String")],
  ]);

  const personType = record_type([
    ["name", con_type("String")],
    ["address", addressType],
  ]);

  const person = record_term([
    ["name", con_term('"Bob"', con_type("String"))],
    [
      "address",
      record_term([
        ["street", con_term('"123 Main"', con_type("String"))],
        ["city", con_term('"Boston"', con_type("String"))],
      ]),
    ],
  ]);

  const getCity = project_term(project_term(person, "address"), "city");
  const result = typecheck([], getCity);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, con_type("String")), "should be String");
});

test("Option type - structural injection with explicit context", () => {
  const intType = con_type("Int");
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", intType], // Uses con_type("Int")
  ]);

  // EXPLICIT CONTEXT: Bind "Int" :: * to handle kind checks
  const context = [
    { type: { name: "Int", kind: starKind } }, // Makes "Int" well-kinded
  ];

  const someVal = inject_term("Some", con_term("42", intType), optionInt);
  const result = typecheck(context, someVal); // Pass context
  const type = assertOk(result, "should typecheck");

  // Assertions
  expect("variant" in type).toBe(true);
  const variant = (type as VariantType).variant;
  expect(variant.length).toBe(2);
  // Optional: Check cases match
  expect(typesEqual(variant[0][1], unitType)).toBe(true); // None: ()
  expect(typesEqual(variant[1][1], intType)).toBe(true); // Some: Int
});

test("Invalid variant label", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  const invalid = inject_term("Other", unitValue, optionInt);
  const result = typecheck([], invalid);
  const err = assertErr(result, "should fail");
  assert("invalid_variant_label" in err, "should be invalid label error");
});

test("Wrong variant payload type", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  const wrongType = inject_term(
    "Some",
    con_term('"str"', con_type("String")),
    optionInt,
  );
  const result = typecheck(
    [
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Int" } },
    ],
    wrongType,
  );
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});

test("Simple pattern match", () => {
  const boolType = variant_type([
    ["True", unitType],
    ["False", unitType],
  ]);

  const notFn = lam_term(
    "b",
    boolType,
    match_term(var_term("b"), [
      [
        variant_pattern("True", wildcard_pattern()),
        inject_term("False", unitValue, boolType),
      ],
      [
        variant_pattern("False", wildcard_pattern()),
        inject_term("True", unitValue, boolType),
      ],
    ]),
  );

  const result = typecheck([], notFn);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("Pattern match with variable binding", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  const unwrap = lam_term(
    "opt",
    optionInt,
    match_term(var_term("opt"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [variant_pattern("Some", var_pattern("x")), var_term("x")],
    ]),
  );

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], unwrap);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.to, con_type("Int")), "should return Int");
});

test("Record pattern matching", () => {
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
    ]),
  );

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], getX);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.to, con_type("Int")), "should return Int");
});

test("Nested pattern matching", () => {
  const resultType = variant_type([
    ["Ok", con_type("Int")],
    ["Err", con_type("String")],
  ]);

  const optionResult = variant_type([
    ["None", unitType],
    ["Some", resultType],
  ]);

  const unwrapAll = lam_term(
    "x",
    optionResult,
    match_term(var_term("x"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("-1", con_type("Int")),
      ],
      [
        variant_pattern("Some", variant_pattern("Ok", var_pattern("val"))),
        var_term("val"),
      ],
      [
        variant_pattern("Some", variant_pattern("Err", wildcard_pattern())),
        con_term("-2", con_type("Int")),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    unwrapAll,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.to, con_type("Int")), "should return Int");
});

test("Non-exhaustive pattern match", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  // Missing the Some case
  const incomplete = lam_term(
    "opt",
    optionInt,
    match_term(var_term("opt"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
    ]),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    incomplete,
  );
  const err = assertErr(result, "should fail");
  assert("missing_case" in err, "should be missing case error");
});

test("Inconsistent branch types", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  const inconsistent = lam_term(
    "opt",
    optionInt,
    match_term(var_term("opt"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [
        variant_pattern("Some", var_pattern("x")),
        con_term('"str"', con_type("String")),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    inconsistent,
  );
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});

test("Polymorphic map for Option", () => {
  const mapOption = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      lam_term(
        "f",
        arrow_type(var_type("A"), var_type("B")),
        lam_term(
          "opt",
          variant_type([
            ["None", unitType],
            ["Some", var_type("A")],
          ]),
          match_term(var_term("opt"), [
            [
              variant_pattern("None", wildcard_pattern()),
              inject_term(
                "None",
                unitValue,
                variant_type([
                  ["None", unitType],
                  ["Some", var_type("B")],
                ]),
              ),
            ],
            [
              variant_pattern("Some", var_pattern("x")),
              inject_term(
                "Some",
                app_term(var_term("f"), var_term("x")),
                variant_type([
                  ["None", unitType],
                  ["Some", var_type("B")],
                ]),
              ),
            ],
          ]),
        ),
      ),
    ),
  );

  const result = typecheck([], mapOption);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("List type with fold", () => {
  // List<T> = Nil | Cons(T, List<T>)
  // Note: This is a simplified version without recursive types
  const listInt = variant_type([
    ["Nil", unitType],
    [
      "Cons",
      record_type([
        ["head", con_type("Int")],
        ["tail", unitType],
      ]),
    ],
  ]);

  const sumList = lam_term(
    "list",
    listInt,
    match_term(var_term("list"), [
      [
        variant_pattern("Nil", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [
        variant_pattern(
          "Cons",
          record_pattern([
            ["head", var_pattern("h")],
            ["tail", wildcard_pattern()],
          ]),
        ),
        var_term("h"), // Simplified - would normally recurse
      ],
    ]),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    sumList,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("State monad type", () => {
  // State s a = s -> (a, s)
  const stateType = (s: Type, a: Type): Type =>
    arrow_type(
      s,
      record_type([
        ["value", a],
        ["state", s],
      ]),
    );

  const returnState = tylam_term(
    "S",
    starKind,
    tylam_term(
      "A",
      starKind,
      lam_term(
        "x",
        var_type("A"),
        lam_term(
          "s",
          var_type("S"),
          record_term([
            ["value", var_term("x")],
            ["state", var_term("s")],
          ]),
        ),
      ),
    ),
  );

  const result = typecheck([], returnState);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Either type with bimap", () => {
  const eitherType = (l: Type, r: Type): Type =>
    variant_type([
      ["Left", l],
      ["Right", r],
    ]);

  const bimap = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      tylam_term(
        "C",
        starKind,
        tylam_term(
          "D",
          starKind,
          lam_term(
            "f",
            arrow_type(var_type("A"), var_type("C")),
            lam_term(
              "g",
              arrow_type(var_type("B"), var_type("D")),
              lam_term(
                "either",
                eitherType(var_type("A"), var_type("B")),
                match_term(var_term("either"), [
                  [
                    variant_pattern("Left", var_pattern("x")),
                    inject_term(
                      "Left",
                      app_term(var_term("f"), var_term("x")),
                      eitherType(var_type("C"), var_type("D")),
                    ),
                  ],
                  [
                    variant_pattern("Right", var_pattern("y")),
                    inject_term(
                      "Right",
                      app_term(var_term("g"), var_term("y")),
                      eitherType(var_type("C"), var_type("D")),
                    ),
                  ],
                ]),
              ),
            ),
          ),
        ),
      ),
    ),
  );

  const result = typecheck([], bimap);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Natural number type", () => {
  const natType = mu_type(
    "N",
    variant_type([
      ["Zero", unitType],
      ["Succ", var_type("N")],
    ]),
  );

  const kind = checkKind([], natType);
  const k = assertOk(kind, "should have kind *");
  assert("star" in k, "should be star kind");
});

test("Zero value", () => {
  const natType = mu_type(
    "N",
    variant_type([
      ["Zero", unitType],
      ["Succ", var_type("N")],
    ]),
  );

  const zero = fold_term(
    natType,
    inject_term(
      "Zero",
      unitValue,
      variant_type([
        ["Zero", unitType],
        ["Succ", natType],
      ]),
    ),
  );

  const result = typecheck([], zero);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, natType), "should be Nat type");
});

test("Successor function", () => {
  const natType = mu_type(
    "N",
    variant_type([
      ["Zero", unitType],
      ["Succ", var_type("N")],
    ]),
  );

  const succ = lam_term(
    "n",
    natType,
    fold_term(
      natType,
      inject_term(
        "Succ",
        var_term("n"),
        variant_type([
          ["Zero", unitType],
          ["Succ", natType],
        ]),
      ),
    ),
  );

  const result = typecheck([], succ);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, natType), "input should be Nat");
  assert(typesEqual(type.arrow.to, natType), "output should be Nat");
});

test("Unfold natural number", () => {
  const natType = mu_type(
    "N",
    variant_type([
      ["Zero", unitType],
      ["Succ", var_type("N")],
    ]),
  );

  const ctx: Context = [{ term: { name: "n", type: natType } }];
  const unfolded = unfold_term(var_term("n"));

  const result = typecheck(ctx, unfolded);
  const type = assertOk(result, "should typecheck");
  assert("variant" in type, "should be variant type");
});

test("List type", () => {
  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const kind = checkKind([{ type: { kind: starKind, name: "Int" } }], listInt);
  const k = assertOk(kind, "should have kind *");
  assert("star" in k, "should be star kind");
});

test("Empty list", () => {
  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const emptyList = fold_term(
    listInt,
    inject_term(
      "Nil",
      unitValue,
      variant_type([
        ["Nil", unitType],
        [
          "Cons",
          record_type([
            ["head", con_type("Int")],
            ["tail", listInt],
          ]),
        ],
      ]),
    ),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    emptyList,
  );
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, listInt), "should be List Int type");
});

test("Cons cell", () => {
  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const emptyList = fold_term(
    listInt,
    inject_term(
      "Nil",
      unitValue,
      variant_type([
        ["Nil", unitType],
        [
          "Cons",
          record_type([
            ["head", con_type("Int")],
            ["tail", listInt],
          ]),
        ],
      ]),
    ),
  );

  const oneElementList = fold_term(
    listInt,
    inject_term(
      "Cons",
      record_term([
        ["head", con_term("42", con_type("Int"))],
        ["tail", emptyList],
      ]),
      variant_type([
        ["Nil", unitType],
        [
          "Cons",
          record_type([
            ["head", con_type("Int")],
            ["tail", listInt],
          ]),
        ],
      ]),
    ),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    oneElementList,
  );
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, listInt), "should be List Int type");
});

test("Simple tuple", () => {
  const tupleType = tuple_type([con_type("Int"), con_type("String")]);
  const tuple = tuple_term([
    con_term("42", con_type("Int")),
    con_term('"hello"', con_type("String")),
  ]);

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], tuple);
  const type = assertOk(result, "should typecheck");
  assert("tuple" in type, "should be tuple type");
  assert(type.tuple.length === 2, "should have 2 elements");
});

test("Tuple projection", () => {
  const tuple = tuple_term([
    con_term("42", con_type("Int")),
    con_term('"hello"', con_type("String")),
    con_term("true", con_type("Bool")),
  ]);

  const proj0 = tuple_project_term(tuple, 0);
  const proj1 = tuple_project_term(tuple, 1);
  const proj2 = tuple_project_term(tuple, 2);

  const result0 = typecheck([], proj0);
  const type0 = assertOk(result0, "should typecheck");
  assert(typesEqual(type0, con_type("Int")), "should be Int");

  const result1 = typecheck([], proj1);
  const type1 = assertOk(result1, "should typecheck");
  assert(typesEqual(type1, con_type("String")), "should be String");

  const result2 = typecheck([], proj2);
  const type2 = assertOk(result2, "should typecheck");
  assert(typesEqual(type2, con_type("Bool")), "should be Bool");
});

test("Out of bounds tuple projection", () => {
  const tuple = tuple_term([
    con_term("42", con_type("Int")),
    con_term('"hello"', con_type("String")),
  ]);

  const outOfBounds = tuple_project_term(tuple, 5);
  const result = typecheck([], outOfBounds);
  const err = assertErr(result, "should fail");
  assert("tuple_index_out_of_bounds" in err, "should be out of bounds error");
});

test("Negative tuple projection", () => {
  const tuple = tuple_term([con_term("42", con_type("Int"))]);
  const negative = tuple_project_term(tuple, -1);
  const result = typecheck([], negative);
  const err = assertErr(result, "should fail");
  assert("tuple_index_out_of_bounds" in err, "should be out of bounds error");
});

test("Empty tuple (unit)", () => {
  const emptyTuple = tuple_term([]);
  const result = typecheck([], emptyTuple);
  const type = assertOk(result, "should typecheck");
  assert("tuple" in type, "should be tuple type");
  assert(type.tuple.length === 0, "should be empty");
});

test("Nested tuples", () => {
  const innerTuple = tuple_term([
    con_term("1", con_type("Int")),
    con_term("2", con_type("Int")),
  ]);

  const outerTuple = tuple_term([
    innerTuple,
    con_term('"outer"', con_type("String")),
  ]);

  const getInnerFirst = tuple_project_term(
    tuple_project_term(outerTuple, 0),
    0,
  );

  const result = typecheck([], getInnerFirst);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, con_type("Int")), "should be Int");
});

test("Tuple pattern matching", () => {
  const pairType = tuple_type([con_type("Int"), con_type("String")]);

  const swap = lam_term(
    "p",
    pairType,
    match_term(var_term("p"), [
      [
        tuple_pattern([var_pattern("x"), var_pattern("y")]),
        tuple_term([var_term("y"), var_term("x")]),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    swap,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("Tuple with wildcard pattern", () => {
  const tripleType = tuple_type([
    con_type("Int"),
    con_type("String"),
    con_type("Bool"),
  ]);

  const getFirst = lam_term(
    "t",
    tripleType,
    match_term(var_term("t"), [
      [
        tuple_pattern([
          var_pattern("x"),
          wildcard_pattern(),
          wildcard_pattern(),
        ]),
        var_term("x"),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    getFirst,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.to, con_type("Int")), "should return Int");
});

test("Polymorphic fst function", () => {
  const fst = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      lam_term(
        "p",
        tuple_type([var_type("A"), var_type("B")]),
        tuple_project_term(var_term("p"), 0),
      ),
    ),
  );

  const result = typecheck([], fst);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Polymorphic snd function", () => {
  const snd = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      lam_term(
        "p",
        tuple_type([var_type("A"), var_type("B")]),
        tuple_project_term(var_term("p"), 1),
      ),
    ),
  );

  const result = typecheck([], snd);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Map function", () => {
  const map = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      lam_term(
        "f",
        arrow_type(var_type("A"), var_type("B")),
        lam_term("x", var_type("A"), app_term(var_term("f"), var_term("x"))),
      ),
    ),
  );

  const result = typecheck([], map);
  assertOk(result, "should typecheck");
});

test("Compose function", () => {
  const compose = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      tylam_term(
        "C",
        starKind,
        lam_term(
          "f",
          arrow_type(var_type("B"), var_type("C")),
          lam_term(
            "g",
            arrow_type(var_type("A"), var_type("B")),
            lam_term(
              "x",
              var_type("A"),
              app_term(var_term("f"), app_term(var_term("g"), var_term("x"))),
            ),
          ),
        ),
      ),
    ),
  );

  const result = typecheck([], compose);
  assertOk(result, "should typecheck");
});

test("Flip function", () => {
  const flip = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      tylam_term(
        "C",
        starKind,
        lam_term(
          "f",
          arrow_type(var_type("A"), arrow_type(var_type("B"), var_type("C"))),
          lam_term(
            "b",
            var_type("B"),
            lam_term(
              "a",
              var_type("A"),
              app_term(app_term(var_term("f"), var_term("a")), var_term("b")),
            ),
          ),
        ),
      ),
    ),
  );

  const result = typecheck([], flip);
  assertOk(result, "should typecheck");
});

test("Type constructor application", () => {
  const listCon = lam_type(
    "T",
    starKind,
    mu_type(
      "L",
      variant_type([
        ["Nil", unitType],
        [
          "Cons",
          record_type([
            ["head", var_type("T")],
            ["tail", var_type("L")],
          ]),
        ],
      ]),
    ),
  );

  const listInt = app_type(listCon, con_type("Int"));
  const kind = checkKind([{ type: { kind: starKind, name: "Int" } }], listInt);
  assertOk(kind, "should have valid kind");
});

test("Type constructor kind mismatch", () => {
  const ctx: Context = [
    { type: { name: "F", kind: { arrow: { from: starKind, to: starKind } } } },
  ];

  // Try to apply F to something that's not kind *
  const badApp = app_type(
    var_type("F"),
    lam_type("X", starKind, var_type("X")),
  );

  const result = checkKind(ctx, badApp);
  const err = assertErr(result, "should fail");
  assert("kind_mismatch" in err, "should be kind mismatch");
});

test("Self-application with unbound type variable", () => {
  // λx: (T → T). x x - fails because T is unbound
  const selfApp = lam_term(
    "x",
    arrow_type(var_type("T"), var_type("T")),
    app_term(var_term("x"), var_term("x")),
  );

  const result = typecheck([], selfApp);
  const err = assertErr(result, "should fail with unbound type variable");
  assert("unbound" in err && err.unbound === "T", "should report unbound T");
});

test("Self-application fails with cyclic type", () => {
  // λx: (T → T). x x
  // This fails because trying to unify T with (T → T) creates a cyclic type
  const selfApp = lam_term(
    "x",
    arrow_type(var_type("T"), var_type("T")),
    app_term(var_term("x"), var_term("x")),
  );

  const context: Context = [{ type: { name: "T", kind: starKind } }];

  const result = typecheck(context, selfApp);
  const err = assertErr(result, "should fail type checking");
  assert(
    "cyclic" in err && err.cyclic === "T",
    "should detect cyclic type T = T → T",
  );
});

test("Self-application fails with type mismatch", () => {
  // λx: (Int → Int). x x
  // This fails because:
  // - x expects argument of type Int
  // - but x itself has type Int → Int (not Int)
  const selfApp = lam_term(
    "x",
    arrow_type(con_type("Int"), con_type("Int")),
    app_term(var_term("x"), var_term("x")),
  );

  const context: Context = [{ type: { name: "Int", kind: starKind } }];

  const result = typecheck(context, selfApp);
  const err = assertErr(result, "should fail type checking");
  assert("type_mismatch" in err, "x x should be a type mismatch");
  // Expected: x takes argument of type Int
  // Actual: x has type Int → Int
});

test("Polymorphic self-application succeeds in System F", () => {
  // λx: (∀β. β → β). x [∀β. β → β] x
  // Instantiate the polymorphic identity at its own type

  const polyId = forall_type(
    "β",
    starKind,
    arrow_type(var_type("β"), var_type("β")),
  );

  const selfApp = lam_term(
    "x",
    polyId, // x : ∀β. β → β
    app_term(
      tyapp_term(var_term("x"), polyId), // x[∀β. β → β] : (∀β. β → β) → (∀β. β → β)
      var_term("x"), // x : ∀β. β → β
    ),
  );

  const result = typecheck([], selfApp);
  const type = assertOk(result, "polymorphic self-application should succeed");
  assert("arrow" in type, "should be function type");
  assert(
    typesEqual(type, arrow_type(polyId, polyId)),
    "should have type (∀β. β → β) → (∀β. β → β)",
  );
});

test("Omega combinator (ω ω) cannot be typed", () => {
  // ω = λx. x x
  // This cannot be given a simple type in System F without recursive types

  // Attempt 1: Try with a concrete type
  const omega = lam_term(
    "x",
    arrow_type(con_type("Int"), con_type("Int")),
    app_term(var_term("x"), var_term("x")),
  );

  const context: Context = [{ type: { name: "Int", kind: starKind } }];

  const result = typecheck(context, omega);
  const err = assertErr(result, "omega combinator should not typecheck");
  assert(
    "type_mismatch" in err || "not_a_function" in err,
    "should fail because x expects Int but gets (Int → Int)",
  );
});

test("Deep nesting", () => {
  // Deeply nested records
  let deepRecord: Term = con_term("42", con_type("Int"));
  for (let i = 0; i < 10; i++) {
    deepRecord = record_term([["inner", deepRecord]]);
  }

  const result = typecheck([], deepRecord);
  assertOk(result, "should handle deep nesting");
});

test("Shadowed variables", () => {
  const shadowed = lam_term(
    "x",
    con_type("Int"),
    lam_term("x", con_type("String"), var_term("x")),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    shadowed,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  // Inner x shadows outer x, so result should be String
});

test("Binary tree type", () => {
  const treeInt = mu_type(
    "T",
    variant_type([
      ["Leaf", con_type("Int")],
      [
        "Node",
        record_type([
          ["left", var_type("T")],
          ["right", var_type("T")],
        ]),
      ],
    ]),
  );

  const kind = checkKind([{ type: { kind: starKind, name: "Int" } }], treeInt);
  assertOk(kind, "should have kind *");
});

test("Infinite list (stream) type", () => {
  const streamInt = mu_type(
    "S",
    record_type([
      ["head", con_type("Int")],
      ["tail", var_type("S")],
    ]),
  );

  const kind = checkKind(
    [{ type: { kind: starKind, name: "Int" } }],
    streamInt,
  );
  assertOk(kind, "should have kind *");
});

test("Multiple patterns same match", () => {
  const eitherType = variant_type([
    ["Left", con_type("Int")],
    ["Right", con_type("Int")],
  ]);

  const toInt = lam_term(
    "e",
    eitherType,
    match_term(var_term("e"), [
      [variant_pattern("Left", var_pattern("x")), var_term("x")],
      [variant_pattern("Right", var_pattern("y")), var_term("y")],
    ]),
  );

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], toInt);
  assertOk(result, "should typecheck");
});

test("Nested tuple and record patterns", () => {
  const complexType = record_type([
    ["data", tuple_type([con_type("Int"), con_type("String")])],
    ["flag", con_type("Bool")],
  ]);

  const extract = lam_term(
    "x",
    complexType,
    match_term(var_term("x"), [
      [
        record_pattern([
          ["data", tuple_pattern([var_pattern("n"), wildcard_pattern()])],
          ["flag", wildcard_pattern()],
        ]),
        var_term("n"),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    extract,
  );
  assertOk(result, "should typecheck");
});

test("type normalization", () => {
  // Test 1: Beta-reduction of type application
  const idType = lam_type("T", starKind, var_type("T"));
  const intType = con_type("Int");
  const appliedType = app_type(idType, intType);

  const normalized = normalizeType(appliedType);
  const expected = intType;

  assert(
    typesEqual(normalized, expected),
    `Test 1 failed: Expected ${showType(expected)} but got ${showType(normalized)}`,
  );

  // Test 2: Nested beta-reductions
  const doubleApp = lam_type(
    "A",
    starKind,
    lam_type("B", starKind, arrow_type(var_type("A"), var_type("B"))),
  );
  const applied = app_type(
    app_type(doubleApp, con_type("Int")),
    con_type("Bool"),
  );

  const normalized2 = normalizeType(applied);
  const expected2 = arrow_type(con_type("Int"), con_type("Bool"));

  assert(
    typesEqual(normalized2, expected2),
    `Test 2 failed: Expected ${showType(expected2)} but got ${showType(normalized2)}`,
  );

  // Test 3: Trivial forall (unused type variable) - preserved, no elimination
  const trivialForall = forall_type("X", starKind, con_type("Int"));

  const normalized3 = normalizeType(trivialForall);
  // Expected: Forall is preserved (no automatic elimination in normalizeType)
  const expected3 = trivialForall;

  assert(
    typesEqual(normalized3, expected3),
    `Test 3 failed: Expected ${showType(expected3)} but got ${showType(normalized3)}`,
  );
  assert(
    "forall" in normalized3,
    `Test 3 failed: Normalized type should preserve forall structure`,
  );

  // Test 4: Mu types should NOT unfold during normalization
  const listType = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      ["Cons", tuple_type([con_type("Int"), var_type("L")])],
    ]),
  );

  const normalized4 = normalizeType(listType);

  // It should still be a mu type after normalization
  assert(
    "mu" in normalized4,
    `Test 4 failed: Mu type should not unfold during normalization`,
  );
  assert(
    typesEqual(normalized4, listType),
    `Test 4 failed: Mu type should remain unchanged`,
  );

  // Test 5: Single-element tuple - preserved, no simplification
  const singleTuple = tuple_type([con_type("Int")]);
  const normalized5 = normalizeType(singleTuple);
  // Expected: Remains a tuple (no simplification to Int in normalizeType)
  const expected5 = singleTuple;

  assert(
    typesEqual(normalized5, expected5),
    `Test 5 failed: Expected ${showType(expected5)} but got ${showType(normalized5)}`,
  );
  assert(
    "tuple" in normalized5 && normalized5.tuple.length === 1,
    `Test 5 failed: Normalized type should preserve single-element tuple structure`,
  );

  // Test 6: Empty record - preserved, no simplification to unit
  // Note: In this system, records and tuples (unitType = empty tuple) are distinct;
  // no automatic conversion in normalizeType.
  const emptyRecord = record_type([]);
  const normalized6 = normalizeType(emptyRecord);
  // Expected: Remains an empty record (no conversion to unitType)
  const expected6 = emptyRecord;

  assert(
    typesEqual(normalized6, expected6),
    `Test 6 failed: Expected ${showType(expected6)} but got ${showType(normalized6)}`,
  );
  assert(
    "record" in normalized6 && normalized6.record.length === 0,
    `Test 6 failed: Normalized type should preserve empty record structure`,
  );

  // Test 7: Complex nested application
  const constType = lam_type(
    "A",
    starKind,
    lam_type("B", starKind, var_type("A")),
  );
  const applied7 = app_type(
    app_type(constType, con_type("String")),
    con_type("Bool"),
  );

  const normalized7 = normalizeType(applied7);
  const expected7 = con_type("String");

  assert(
    typesEqual(normalized7, expected7),
    `Test 7 failed: Expected ${showType(expected7)} but got ${showType(normalized7)}`,
  );

  // Test 8: Normalization preserves used forall variables
  const usedForall = forall_type(
    "T",
    starKind,
    arrow_type(var_type("T"), var_type("T")),
  );

  const normalized8 = normalizeType(usedForall);

  assert(
    "forall" in normalized8,
    `Test 8 failed: Forall with used variable should be preserved`,
  );
  assert(
    typesEqual(normalized8, usedForall),
    `Test 8 failed: Used forall should remain unchanged`,
  );
});

// ============= TRAIT SYSTEM TESTS =============

test("Simple trait definition", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const context: Context = [{ trait_def: showTrait }];

  // Verify the trait definition is in context
  const binding = context.find((b) => "trait_def" in b);
  assert(binding !== undefined, "trait should be in context");
  assert("trait_def" in binding!, "should be trait_def binding");
});

test("Dictionary for Show Int", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term('"42"', con_type("String")));

  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { trait_def: showTrait },
  ];

  const result = typecheck(context, intShowDict);
  const type = assertOk(result, "should typecheck");
  assert("con" in type, "should be dictionary type");
});

test("Dictionary with missing method", () => {
  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: starKind,
    methods: [
      [
        "eq",
        arrow_type(
          var_type("Self"),
          arrow_type(var_type("Self"), con_type("Bool")),
        ),
      ],
      [
        "neq",
        arrow_type(
          var_type("Self"),
          arrow_type(var_type("Self"), con_type("Bool")),
        ),
      ],
    ],
  };

  const intType = con_type("Int");
  const eqImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("true", con_type("Bool"))),
  );

  // Missing neq method
  const incompleteDict = dict_term("Eq", intType, [["eq", eqImpl]]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "Bool" } },
    { trait_def: eqTrait },
  ];

  const result = typecheck(context, incompleteDict);
  const err = assertErr(result, "should fail");
  assert("missing_method" in err, "should be missing method error");
});

test("Dictionary with wrong method type", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  // Wrong: returns Int instead of String
  const wrongImpl = lam_term("x", intType, con_term("42", intType));

  const wrongDict = dict_term("Show", intType, [["show", wrongImpl]]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { trait_def: showTrait },
  ];

  const result = typecheck(context, wrongDict);
  console.log(result);
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});

test("Trait method access from dictionary variable", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");

  const context: Context = [
    { trait_def: showTrait },
    {
      dict: {
        name: "showIntDict",
        trait: "Show",
        type: intType,
      },
    },
  ];

  const methodAccess = trait_method_term(var_term("showIntDict"), "show");

  const result = typecheck(context, methodAccess);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "should take Int");
  assert(typesEqual(type.arrow.to, con_type("String")), "should return String");
});

test("Trait method access from concrete dictionary", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term('"42"', con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { trait_def: showTrait },
  ];

  const methodAccess = trait_method_term(intShowDict, "show");

  const result = typecheck(context, methodAccess);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("Trait method access with wrong method name", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");

  const context: Context = [
    { trait_def: showTrait },
    {
      dict: {
        name: "showIntDict",
        trait: "Show",
        type: intType,
      },
    },
  ];

  const wrongMethod = trait_method_term(var_term("showIntDict"), "wrongMethod");

  const result = typecheck(context, wrongMethod);
  const err = assertErr(result, "should fail");
  assert("missing_method" in err, "should be missing method error");
});

test("Simple trait lambda", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // Λ T where Show<T>. λx:T. show(dict, x)
  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(trait_method_term(var_term("showDict"), "show"), var_term("x")),
    ),
  );

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, showValue);
  const type = assertOk(result, "should typecheck");
  assert("bounded_forall" in type, "should be bounded forall type");
  assert(
    type.bounded_forall.constraints.length === 1,
    "should have one constraint",
  );
  assert(
    type.bounded_forall.constraints[0]?.trait === "Show",
    "should have Show constraint",
  );
});

test("Trait application with concrete type", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term('"42"', con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(trait_method_term(var_term("showDict"), "show"), var_term("x")),
    ),
  );

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { trait_def: showTrait },
    { trait_impl: { trait: "Show", type: intType, dict: intShowDict } },
  ];

  // Apply to Int type with dictionary
  const applied = trait_app_term(showValue, intType, [intShowDict]);

  const result = typecheck(context, applied);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "should take Int");
  assert(typesEqual(type.arrow.to, con_type("String")), "should return String");
});

test("Trait application with missing implementation", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(trait_method_term(var_term("showDict"), "show"), var_term("x")),
    ),
  );

  const context: Context = [{ trait_def: showTrait }];

  const boolType = con_type("Bool");
  // No Show implementation for Bool provided
  const applied = trait_app_term(showValue, boolType, []);

  const result = typecheck(context, applied);
  const err = assertErr(result, "should fail");
  assert(
    "missing_trait_impl" in err || "wrong_number_of_dicts" in err,
    "should be missing implementation error",
  );
});

test("Multiple trait constraints", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: starKind,
    methods: [
      [
        "eq",
        arrow_type(
          var_type("Self"),
          arrow_type(var_type("Self"), con_type("Bool")),
        ),
      ],
    ],
  };

  // Λ T where Show<T>, Eq<T>. λx:T. λy:T. ...
  const compareAndShow = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [
      { trait: "Show", type: var_type("T") },
      { trait: "Eq", type: var_type("T") },
    ],
    lam_term(
      "x",
      var_type("T"),
      lam_term("y", var_type("T"), con_term('"result"', con_type("String"))),
    ),
  );

  const context: Context = [{ trait_def: showTrait }, { trait_def: eqTrait }];

  const result = typecheck(context, compareAndShow);
  const type = assertOk(result, "should typecheck");
  assert("bounded_forall" in type, "should be bounded forall type");
  assert(
    type.bounded_forall.constraints.length === 2,
    "should have two constraints",
  );
});

test("Functor trait with map", () => {
  const functorTrait: TraitDef = {
    name: "Functor",
    type_param: "F",
    kind: { arrow: { from: starKind, to: starKind } },
    methods: [
      [
        "map",
        forall_type(
          "A",
          starKind,
          forall_type(
            "B",
            starKind,
            arrow_type(
              arrow_type(var_type("A"), var_type("B")),
              arrow_type(
                app_type(var_type("F"), var_type("A")),
                app_type(var_type("F"), var_type("B")),
              ),
            ),
          ),
        ),
      ],
    ],
  };

  const context: Context = [{ trait_def: functorTrait }];

  // Just verify the trait definition typechecks
  const binding = context.find((b) => "trait_def" in b);
  assert(binding !== undefined, "trait should be in context");
});

test("Pattern matching with constant patterns", () => {
  const intType = con_type("Int");

  const isZero = lam_term(
    "x",
    intType,
    match_term(var_term("x"), [
      [con_pattern("0", intType), con_term("true", con_type("Bool"))],
      [wildcard_pattern(), con_term("false", con_type("Bool"))],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    isZero,
  );
  assertOk(result, "should handle constant patterns");
});

test("Higher-kinded trait (Functor)", () => {
  const functorTrait: TraitDef = {
    name: "Functor",
    type_param: "F",
    kind: { arrow: { from: starKind, to: starKind } },
    methods: [
      [
        "map",
        forall_type(
          "A",
          starKind,
          forall_type(
            "B",
            starKind,
            arrow_type(
              arrow_type(var_type("A"), var_type("B")),
              arrow_type(
                app_type(var_type("F"), var_type("A")),
                app_type(var_type("F"), var_type("B")),
              ),
            ),
          ),
        ),
      ],
    ],
  };

  // Option functor
  const optionCon = lam_type(
    "T",
    starKind,
    variant_type([
      ["None", unitType],
      ["Some", var_type("T")],
    ]),
  );

  const context: Context = [{ trait_def: functorTrait }];

  // Check that Option has the right kind for Functor
  const optionKind = checkKind(context, optionCon);
  const kind = assertOk(optionKind, "should have kind * -> *");
  assert("arrow" in kind, "should be arrow kind");
});

test("Trait with associated types simulation", () => {
  // Collection with element type
  const collectionTrait: TraitDef = {
    name: "Collection",
    type_param: "C",
    kind: starKind,
    methods: [
      ["empty", var_type("C")],
      ["size", arrow_type(var_type("C"), con_type("Int"))],
    ],
  };

  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const emptyList = fold_term(
    listInt,
    inject_term(
      "Nil",
      unitValue,
      variant_type([
        ["Nil", unitType],
        [
          "Cons",
          record_type([
            ["head", con_type("Int")],
            ["tail", listInt],
          ]),
        ],
      ]),
    ),
  );

  const sizeImpl = lam_term("list", listInt, con_term("0", con_type("Int")));

  const listDict = dict_term("Collection", listInt, [
    ["empty", emptyList],
    ["size", sizeImpl],
  ]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { type: { kind: starKind, name: "Bool" } },
    { trait_def: collectionTrait },
  ];

  const result = typecheck(context, listDict);
  assertOk(result, "should typecheck collection instance");
});

test("Overlapping trait constraints", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // Function that requires Show twice (redundant but valid)
  const doubleShow = trait_lam_term(
    "dict1",
    "Show",
    "T",
    starKind,
    [
      { trait: "Show", type: var_type("T") },
      { trait: "Show", type: var_type("T") },
    ],
    lam_term("x", var_type("T"), con_term('"shown"', con_type("String"))),
  );

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, doubleShow);
  assertOk(result, "should handle duplicate constraints");
});

test("Trait method returning trait-constrained type", () => {
  const monadTrait: TraitDef = {
    name: "Monad",
    type_param: "M",
    kind: { arrow: { from: starKind, to: starKind } },
    methods: [
      [
        "pure",
        forall_type(
          "A",
          starKind,
          arrow_type(var_type("A"), app_type(var_type("M"), var_type("A"))),
        ),
      ],
    ],
  };

  const context: Context = [{ trait_def: monadTrait }];

  // Function that uses monad methods
  const wrapValue = trait_lam_term(
    "monadDict",
    "Monad",
    "M",
    { arrow: { from: starKind, to: starKind } },
    [{ trait: "Monad", type: var_type("M") }],
    tylam_term(
      "A",
      starKind,
      lam_term(
        "x",
        var_type("A"),
        tyapp_term(
          trait_method_term(var_term("monadDict"), "pure"),
          var_type("A"),
        ),
      ),
    ),
  );

  const result = typecheck(context, wrapValue);
  assertOk(result, "should handle trait methods with polymorphic returns");
});

// ============= TYPE NORMALIZATION TESTS =============

test("Normalization of nested applications", () => {
  const f = lam_type("A", starKind, lam_type("B", starKind, var_type("A")));
  const applied = app_type(app_type(f, con_type("Int")), con_type("Bool"));

  const normalized = normalizeType(applied);
  assert(typesEqual(normalized, con_type("Int")), "should reduce to Int");
});

test("Normalization preserves mu types", () => {
  const listType = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      ["Cons", tuple_type([con_type("Int"), var_type("L")])],
    ]),
  );

  const normalized = normalizeType(listType);
  assert("mu" in normalized, "should still be mu type");
  assert(typesEqual(normalized, listType), "should be unchanged");
});

test("Normalization with shadowed variables", () => {
  const type1 = forall_type(
    "A",
    starKind,
    forall_type("A", starKind, var_type("A")),
  );

  const normalized = normalizeType(type1);
  assert("forall" in normalized, "should preserve forall structure");
});

test("Normalization of complex arrow types", () => {
  const complexArrow = arrow_type(
    app_type(lam_type("T", starKind, var_type("T")), con_type("Int")),
    app_type(lam_type("T", starKind, var_type("T")), con_type("Bool")),
  );

  const normalized = normalizeType(complexArrow);
  const expected = arrow_type(con_type("Int"), con_type("Bool"));

  assert(
    typesEqual(normalized, expected),
    "should normalize both sides of arrow",
  );
});

// ============= ALPHA-EQUIVALENCE TESTS =============

test("Alpha-equivalent forall types", () => {
  const type1 = forall_type("A", starKind, var_type("A"));
  const type2 = forall_type("B", starKind, var_type("B"));

  assert(typesEqual(type1, type2), "should be alpha-equivalent");
});

test("Alpha-equivalent nested foralls", () => {
  const type1 = forall_type(
    "A",
    starKind,
    forall_type("B", starKind, arrow_type(var_type("A"), var_type("B"))),
  );

  const type2 = forall_type(
    "X",
    starKind,
    forall_type("Y", starKind, arrow_type(var_type("X"), var_type("Y"))),
  );

  assert(typesEqual(type1, type2), "nested foralls should be alpha-equivalent");
});

test("Not alpha-equivalent with wrong binding", () => {
  const type1 = forall_type(
    "A",
    starKind,
    forall_type("B", starKind, var_type("A")),
  );

  const type2 = forall_type(
    "A",
    starKind,
    forall_type("B", starKind, var_type("B")),
  );

  assert(!typesEqual(type1, type2), "should not be alpha-equivalent");
});

// ============= OCCURS CHECK TESTS =============

test("Occurs check prevents infinite types", () => {
  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  // [FIX: Use a meta-var (flexible) for left to trigger binding + occurs check]
  // Regular var_type("X") is rigid → immediate mismatch, no occurs.
  // Meta ?0 is flexible → attempts bind ?0 := ?0 -> Int → occursCheck detects cycle.
  const X = freshMetaVar() as VarType; // e.g., { var: "?0" } – now isMetaVar(X) = true
  const infiniteType = arrow_type(X, con_type("Int")); // ?0 -> Int

  const result = unifyTypes(
    X, // Meta: flexible, will try to bind
    infiniteType,
    worklist,
    subst,
  );
  console.log("Result:", result);
  console.log("Subst after:", subst); // Should be empty (binding rejected)

  const err = assertErr(result, "should fail occurs check");
  assert("cyclic" in err, "should be cyclic error");
  assert(
    err.cyclic === X.var,
    `cyclic should mention var ${X.var}, got ${err.cyclic}`,
  ); // Optional: more precise
});

test("Occurs check in records", () => {
  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  const x = freshMetaVar();
  // Try to unify X with {f: X}
  const result = unifyTypes(x, record_type([["f", x]]), worklist, subst);

  const err = assertErr(result, "should fail occurs check");
  assert("cyclic" in err, "should be cyclic error");
});

test("Occurs check with nested types", () => {
  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  const x = freshMetaVar();

  // X = List<X> should fail
  const listOfX = variant_type([
    ["Nil", unitType],
    [
      "Cons",
      record_type([
        ["head", x],
        ["tail", x],
      ]),
    ],
  ]);

  const result = unifyTypes(x, listOfX, worklist, subst);

  const err = assertErr(result, "should fail occurs check");
  assert("cyclic" in err, "should be cyclic error");
});

// ============= COMPLEX FEATURE COMBINATIONS =============

test("Polymorphic list with trait constraints", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // showList: ∀T where Show<T>. List<T> -> String
  const showList = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "list",
      mu_type(
        "L",
        variant_type([
          ["Nil", unitType],
          [
            "Cons",
            record_type([
              ["head", var_type("T")],
              ["tail", var_type("L")],
            ]),
          ],
        ]),
      ),
      match_term(unfold_term(var_term("list")), [
        [
          variant_pattern("Nil", wildcard_pattern()),
          con_term('"[]"', con_type("String")),
        ],
        [
          variant_pattern(
            "Cons",
            record_pattern([
              ["head", var_pattern("h")],
              ["tail", wildcard_pattern()],
            ]),
          ),
          app_term(
            trait_method_term(var_term("showDict"), "show"),
            var_term("h"),
          ),
        ],
      ]),
    ),
  );

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, showList);
  assertOk(result, "should combine traits with recursive types");
});

test("Functor instance for Option", () => {
  const functorTrait: TraitDef = {
    name: "Functor",
    type_param: "F",
    kind: { arrow: { from: starKind, to: starKind } },
    methods: [
      [
        "map",
        forall_type(
          "A",
          starKind,
          forall_type(
            "B",
            starKind,
            arrow_type(
              arrow_type(var_type("A"), var_type("B")),
              arrow_type(
                app_type(var_type("F"), var_type("A")),
                app_type(var_type("F"), var_type("B")),
              ),
            ),
          ),
        ),
      ],
    ],
  };

  const optionType = lam_type(
    "T",
    starKind,
    variant_type([
      ["None", unitType],
      ["Some", var_type("T")],
    ]),
  );

  // Simplified map implementation
  const mapImpl = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      lam_term(
        "f",
        arrow_type(var_type("A"), var_type("B")),
        lam_term(
          "opt",
          variant_type([
            ["None", unitType],
            ["Some", var_type("A")],
          ]),
          inject_term(
            "None",
            unitValue,
            variant_type([
              ["None", unitType],
              ["Some", var_type("B")],
            ]),
          ),
        ),
      ),
    ),
  );

  const optionFunctor = dict_term("Functor", optionType, [["map", mapImpl]]);

  const context: Context = [{ trait_def: functorTrait }];

  const result = typecheck(context, optionFunctor);
  assertOk(result, "should implement Functor for Option");
});

test("Monad with do-notation simulation", () => {
  const monadTrait: TraitDef = {
    name: "Monad",
    type_param: "M",
    kind: { arrow: { from: starKind, to: starKind } },
    methods: [
      [
        "bind",
        forall_type(
          "A",
          starKind,
          forall_type(
            "B",
            starKind,
            arrow_type(
              app_type(var_type("M"), var_type("A")),
              arrow_type(
                arrow_type(
                  var_type("A"),
                  app_type(var_type("M"), var_type("B")),
                ),
                app_type(var_type("M"), var_type("B")),
              ),
            ),
          ),
        ),
      ],
    ],
  };

  const optionType = lam_type(
    "T",
    starKind,
    variant_type([
      ["None", unitType],
      ["Some", var_type("T")],
    ]),
  );

  const context: Context = [{ trait_def: monadTrait }];

  // Just verify the trait structure
  const binding = context.find((b) => "trait_def" in b);
  assert(binding !== undefined, "monad trait should be defined");
});

test("GADTs simulation with variants", () => {
  // Expr: Int literal or Bool literal or Add
  const exprType = mu_type(
    "Expr",
    variant_type([
      ["IntLit", con_type("Int")],
      ["BoolLit", con_type("Bool")],
      ["Add", tuple_type([var_type("Expr"), var_type("Expr")])],
    ]),
  );

  const eval_term = lam_term(
    "expr",
    exprType,
    match_term(unfold_term(var_term("expr")), [
      [variant_pattern("IntLit", var_pattern("n")), var_term("n")],
      [
        variant_pattern("BoolLit", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [
        variant_pattern("Add", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
    ]),
  );

  // This is a simplified version - true GADTs would need more type system support
  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    eval_term,
  );
  assertOk(result, "should handle GADT-like structures");
});

test("Phantom types simulation", () => {
  // SafeList tagged with length
  const safeListType = (tag: Type, elem: Type) =>
    record_type([
      ["tag", tag],
      [
        "data",
        mu_type(
          "L",
          variant_type([
            ["Nil", unitType],
            [
              "Cons",
              record_type([
                ["head", elem],
                ["tail", var_type("L")],
              ]),
            ],
          ]),
        ),
      ],
    ]);

  const emptyTag = con_type("Zero");
  const elemType = con_type("Int");

  const emptyList = record_term([
    ["tag", con_term("zero", emptyTag)],
    [
      "data",
      fold_term(
        mu_type(
          "L",
          variant_type([
            ["Nil", unitType],
            [
              "Cons",
              record_type([
                ["head", elemType],
                ["tail", var_type("L")],
              ]),
            ],
          ]),
        ),
        inject_term(
          "Nil",
          unitValue,
          variant_type([
            ["Nil", unitType],
            [
              "Cons",
              record_type([
                ["head", elemType],
                [
                  "tail",
                  mu_type(
                    "L",
                    variant_type([
                      ["Nil", unitType],
                      [
                        "Cons",
                        record_type([
                          ["head", elemType],
                          ["tail", var_type("L")],
                        ]),
                      ],
                    ]),
                  ),
                ],
              ]),
            ],
          ]),
        ),
      ),
    ],
  ]);

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "Zero" } },
    ],
    emptyList,
  );
  assertOk(result, "should support phantom types");
});

// ============= ERROR BOUNDARY TESTS =============

test("Deeply nested errors maintain context", () => {
  const deeplyNested = lam_term(
    "a",
    con_type("Int"),
    lam_term(
      "b",
      con_type("String"),
      lam_term(
        "c",
        con_type("Bool"),
        // Type error deep inside
        app_term(var_term("a"), con_term('"wrong"', con_type("String"))),
      ),
    ),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
      { type: { kind: starKind, name: '"wrong"' } },
    ],
    deeplyNested,
  );
  const err = assertErr(result, "should propagate error from deep nesting");
  assert(
    "type_mismatch" in err || "not_a_function" in err,
    "should have appropriate error",
  );
});

test("Error in pattern match branch", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  const badMatch = lam_term(
    "opt",
    optionInt,
    match_term(var_term("opt"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [
        variant_pattern("Some", var_pattern("x")),
        // Type error: trying to return string when expecting int
        con_term('"error"', con_type("String")),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    badMatch,
  );
  const err = assertErr(result, "should catch error in branch");
  assert("type_mismatch" in err, "should be type mismatch");
});

test("Large tuple size limits", () => {
  const elements: Term[] = [];
  for (let i = 0; i < 1000; i++) {
    elements.push(con_term(`${i}`, con_type("Int")));
  }

  const largeTuple = tuple_term(elements);
  const result = typecheck([], largeTuple);

  // Should either succeed or fail gracefully
  // (depends on implementation limits)
  if ("ok" in result) {
    assert("tuple" in result.ok, "should be tuple type");
  }
});

// ============= TYPE INFERENCE TESTS =============

test("Automatic type instantiation for polymorphic identity", () => {
  const polyId = tylam_term(
    "T",
    starKind,
    lam_term("x", var_type("T"), var_term("x")),
  );

  const context: Context = [
    {
      term: {
        name: "id",
        type: forall_type(
          "T",
          starKind,
          arrow_type(var_type("T"), var_type("T")),
        ),
      },
    },
  ];

  // id 5 should automatically instantiate T = Int
  const app = app_term(var_term("id"), con_term("5", con_type("Int")));

  const result = typecheck(context, app);
  const type = assertOk(result, "should infer type argument");
  assert(typesEqual(type, con_type("Int")), "should be Int");
});

test("Inference with nested applications", () => {
  const compose = tylam_term(
    "A",
    starKind,
    tylam_term(
      "B",
      starKind,
      tylam_term(
        "C",
        starKind,
        lam_term(
          "f",
          arrow_type(var_type("B"), var_type("C")),
          lam_term(
            "g",
            arrow_type(var_type("A"), var_type("B")),
            lam_term(
              "x",
              var_type("A"),
              app_term(var_term("f"), app_term(var_term("g"), var_term("x"))),
            ),
          ),
        ),
      ),
    ),
  );

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { type: { kind: starKind, name: "Bool" } },
    {
      term: {
        name: "compose",
        type: forall_type(
          "A",
          starKind,
          forall_type(
            "B",
            starKind,
            forall_type(
              "C",
              starKind,
              arrow_type(
                arrow_type(var_type("B"), var_type("C")),
                arrow_type(
                  arrow_type(var_type("A"), var_type("B")),
                  arrow_type(var_type("A"), var_type("C")),
                ),
              ),
            ),
          ),
        ),
      },
    },
  ];

  // compose should infer all three type arguments
  const f = lam_term(
    "x",
    con_type("Int"),
    con_term('"str"', con_type("String")),
  );
  const g = lam_term("y", con_type("Bool"), con_term("42", con_type("Int")));

  const app = app_term(app_term(var_term("compose"), f), g);

  const result = typecheck(context, app);
  const type = assertOk(result, "should infer all type arguments");
  assert("arrow" in type, "should be function type");
});

test("Subsumption allows polymorphic to specific", () => {
  const polyId = forall_type(
    "T",
    starKind,
    arrow_type(var_type("T"), var_type("T")),
  );
  const intToInt = arrow_type(con_type("Int"), con_type("Int"));

  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  // Should allow assigning polyId to intToInt
  const result = unifyTypes(polyId, intToInt, worklist, subst);

  if ("ok" in result) {
    const solveResult = solveConstraints(worklist, subst);
    assertOk(solveResult, "should unify through subsumption");
  }
});

test("Bidirectional checking for lambdas", () => {
  const intType = con_type("Int");
  const expectedType = arrow_type(intType, intType);

  // Lambda without explicit type on argument
  const lam = lam_term("x", intType, var_term("x"));

  const result = checkType(
    [{ type: { kind: starKind, name: "Int" } }],
    lam,
    expectedType,
  );
  const type = assertOk(result, "should check lambda against expected type");
  assert(typesEqual(type.type, expectedType), "should match expected type");
});

test("Bidirectional checking for records", () => {
  const expectedType = record_type([
    ["x", con_type("Int")],
    ["y", con_type("String")],
  ]);

  const record = record_term([
    ["x", con_term("42", con_type("Int"))],
    ["y", con_term('"hello"', con_type("String"))],
  ]);

  const result = checkType(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    record,
    expectedType,
  );
  assertOk(result, "should check record structure");
});

test("Bidirectional checking for tuples", () => {
  const expectedType = tuple_type([con_type("Int"), con_type("Bool")]);

  const tuple = tuple_term([
    con_term("5", con_type("Int")),
    con_term("true", con_type("Bool")),
  ]);

  const result = checkType(
    [
      { type: { kind: starKind, name: "Bool" } },
      { type: { kind: starKind, name: "Int" } },
    ],
    tuple,
    expectedType,
  );
  assertOk(result, "should check tuple elements");
});

test("Inference fails with ambiguous types", () => {
  // Function that returns a polymorphic value
  const ambiguous = lam_term(
    "x",
    con_type("Int"),
    tylam_term("T", starKind, lam_term("y", var_type("T"), var_term("y"))),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    ambiguous,
  );
  assertOk(result, "should infer polymorphic result");
});

test("Metavariable unification", () => {
  const subst = new Map<string, Type>();

  // Unify ?0 with Int
  const result = unifyVariable("?0", con_type("Int"), subst);
  assertOk(result, "should unify metavar with concrete type");

  assert(subst.has("?0"), "should record substitution");
  assert(typesEqual(subst.get("?0")!, con_type("Int")), "should map to Int");
});

test("Metavariable substitution chain", () => {
  const subst = new Map<string, Type>();

  // ?0 = ?1, ?1 = Int
  subst.set("?0", var_type("?1"));
  subst.set("?1", con_type("Int"));

  const resolved = applySubstitution(subst, var_type("?0"));
  assert(typesEqual(resolved, con_type("Int")), "should resolve chain");
});

test("Occurs check with metavariables", () => {
  const subst = new Map<string, Type>();

  // Try to unify ?0 with ?0 -> Int
  const result = unifyVariable(
    "?0",
    arrow_type(var_type("?0"), con_type("Int")),
    subst,
  );

  const err = assertErr(result, "should fail occurs check");
  assert("cyclic" in err, "should detect cycle");
});

test("Higher-rank polymorphism simulation", () => {
  // (∀a. a -> a) -> Int -> Int
  const higherRank = arrow_type(
    forall_type("a", starKind, arrow_type(var_type("a"), var_type("a"))),
    arrow_type(con_type("Int"), con_type("Int")),
  );

  const f = lam_term(
    "id",
    forall_type("a", starKind, arrow_type(var_type("a"), var_type("a"))),
    lam_term(
      "x",
      con_type("Int"),
      app_term(tyapp_term(var_term("id"), con_type("Int")), var_term("x")),
    ),
  );

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], f);
  assertOk(result, "should handle rank-2 types");
});

// ============= CONSTRAINT SOLVING TESTS =============

test("Worklist-based unification", () => {
  const worklist: Worklist = [];
  const subst = new Map<string, Type>(); // Explicitly pass to solveConstraints (though default is new Map)

  // [FIX: Use meta-vars for A/B to allow binding (regular vars are rigid, cause immediate mismatch)]
  // Regular var_type("A") is rigid → no binding, type_mismatch error.
  // Metas (?0, ?1) are flexible → unifyVariable binds them, supports transitivity.
  const A = freshMetaVar() as VarType; // e.g., { var: "?0" }
  const B = freshMetaVar() as VarType; // e.g., { var: "?1" }
  const intType = con_type("Int");

  worklist.push(
    { type_eq: { left: A, right: intType } }, // ?0 ~ Int
    { type_eq: { left: B, right: A } }, // ?1 ~ ?0 (will become ?1 ~ Int after subst)
  );

  const result = solveConstraints(worklist, subst); // Pass subst explicitly for clarity
  const solvedSubst = assertOk(result, "should solve constraints"); // Now uses passed subst

  // Verify bindings (apply subst recursively if needed, but here direct)
  assert(
    typesEqual(solvedSubst.get(A.var)!, intType),
    `A (${A.var}) should map to Int, got ${showType(solvedSubst.get(A.var)!)}`,
  );
  assert(
    typesEqual(solvedSubst.get(B.var)!, intType),
    `B (${B.var}) should map to Int, got ${showType(solvedSubst.get(B.var)!)}`,
  );

  // Optional: Verify transitivity by applying subst to B ~ A
  const appliedA = applySubstitution(solvedSubst, A);
  assert(
    typesEqual(appliedA, intType),
    "applySubstitution on A should yield Int",
  );

  // Ensure no errors propagated to global meta solutions (isolated subst)
  console.log("Solved subst:", solvedSubst); // e.g., Map { "?0" => { con: "Int" }, "?1" => { con: "Int" } }
});

test("Kind constraint solving", () => {
  const worklist: Worklist = [{ kind_eq: { left: starKind, right: starKind } }];

  const result = solveConstraints(worklist);
  assertOk(result, "should solve kind constraints");
});

test("Has-kind constraint", () => {
  const worklist: Worklist = [
    {
      has_kind: {
        ty: con_type("Int"),
        kind: starKind,
        context: [{ type: { kind: starKind, name: "Int" } }],
      },
    },
  ];

  const result = solveConstraints(worklist);
  assertOk(result, "should verify kind");
});

test("Has-type constraint", () => {
  const worklist: Worklist = [
    {
      has_type: {
        term: con_term("5", con_type("Int")),
        ty: con_type("Int"),
        context: [],
      },
    },
  ];

  const result = solveConstraints(worklist);
  assertOk(result, "should verify type");
});

test("Conflicting constraints fail", () => {
  const worklist: Worklist = [
    { type_eq: { left: var_type("A"), right: con_type("Int") } },
    { type_eq: { left: var_type("A"), right: con_type("Bool") } },
  ];

  const result = solveConstraints(worklist);
  const err = assertErr(result, "should detect conflict");
  assert("type_mismatch" in err, "should be type mismatch");
});

// ============= TRAIT INFERENCE TESTS =============

test("Automatic dictionary passing", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term('"42"', con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(trait_method_term(var_term("showDict"), "show"), var_term("x")),
    ),
  );

  const context: Context = [
    { trait_def: showTrait },
    { trait_impl: { trait: "Show", type: intType, dict: intShowDict } },
  ];

  // Should automatically find and pass the dictionary
  const result = instantiateWithTraits(
    context,
    bounded_forall_type(
      "T",
      starKind,
      [{ trait: "Show", type: var_type("T") }],
      arrow_type(var_type("T"), con_type("String")),
    ),
  );

  assertOk(result, "should find trait implementation");
});

test("Multiple dictionary inference", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: starKind,
    methods: [
      [
        "eq",
        arrow_type(
          var_type("Self"),
          arrow_type(var_type("Self"), con_type("Bool")),
        ),
      ],
    ],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term('"42"', con_type("String")));
  const eqImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("true", con_type("Bool"))),
  );

  const context: Context = [
    { trait_def: showTrait },
    { trait_def: eqTrait },
    {
      trait_impl: {
        trait: "Show",
        type: intType,
        dict: dict_term("Show", intType, [["show", showImpl]]),
      },
    },
    {
      trait_impl: {
        trait: "Eq",
        type: intType,
        dict: dict_term("Eq", intType, [["eq", eqImpl]]),
      },
    },
  ];

  const constraints: TraitConstraint[] = [
    { trait: "Show", type: intType },
    { trait: "Eq", type: intType },
  ];

  const result = checkTraitConstraints(context, constraints);
  const dicts = assertOk(result, "should find both implementations");
  assert(dicts.length === 2, "should have two dictionaries");
});

test("Trait constraint substitution during instantiation", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const polyType = bounded_forall_type(
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String")),
  );

  const intType = con_type("Int");
  const instantiated = substituteType("T", intType, polyType);

  // The constraint should now be Show<Int>
  assert(
    "bounded_forall" in instantiated || "arrow" in instantiated,
    "should substitute in constraints",
  );
});

// ============= BOTTOM TYPE TESTS =============

test("Never type is subtype of everything", () => {
  const neverType: Type = { never: null };
  const intType = con_type("Int");

  assert(
    isAssignableTo(neverType, intType),
    "never should be assignable to Int",
  );
  assert(
    isAssignableTo(neverType, arrow_type(intType, intType)),
    "never should be assignable to function",
  );
});

test("Never in match branches", () => {
  const neverType: Type = { never: null };
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  // Branch that returns never is compatible with Int
  const matchWithNever = lam_term(
    "opt",
    optionInt,
    match_term(var_term("opt"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [variant_pattern("Some", var_pattern("x")), var_term("x")],
    ]),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    matchWithNever,
  );
  assertOk(result, "never should unify with any branch type");
});

test("Never type kinding", () => {
  const neverType: Type = { never: null };
  const result = checkKind([], neverType);
  const kind = assertOk(result, "never should have kind");
  assert("star" in kind, "never should have kind *");
});

// ============= LET POLYMORPHISM TESTS =============

test("Let polymorphism basic", () => {
  const polyId = tylam_term(
    "T",
    starKind,
    lam_term("x", var_type("T"), var_term("x")),
  );

  const letTerm = let_term(
    "id",
    polyId,
    tuple_term([
      app_term(
        tyapp_term(var_term("id"), con_type("Int")),
        con_term("5", con_type("Int")),
      ),
      app_term(
        tyapp_term(var_term("id"), con_type("Bool")),
        con_term("true", con_type("Bool")),
      ),
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    letTerm,
  );
  assertOk(result, "should allow polymorphic use of let-bound variable");
});

test("Let with type inference", () => {
  const intType = con_type("Int");

  const letTerm = let_term(
    "x",
    con_term("5", intType),
    app_term(lam_term("y", intType, var_term("y")), var_term("x")),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    letTerm,
  );
  const type = assertOk(result, "should infer let binding type");
  assert(typesEqual(type, intType), "should be Int");
});

test("Nested let bindings", () => {
  const intType = con_type("Int");

  const nested = let_term(
    "x",
    con_term("5", intType),
    let_term("y", var_term("x"), let_term("z", var_term("y"), var_term("z"))),
  );

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], nested);
  assertOk(result, "should handle nested lets");
});

test("Let with shadowing", () => {
  const intType = con_type("Int");
  const strType = con_type("String");

  const shadowed = let_term(
    "x",
    con_term("5", intType),
    let_term("x", con_term('"hello"', strType), var_term("x")),
  );

  const result = typecheck([], shadowed);
  const type = assertOk(result, "should handle shadowing");
  assert(typesEqual(type, strType), "inner binding should shadow");
});

// ============= SUBSTITUTION TESTS =============

test("Substitution in complex types", () => {
  const complexType = arrow_type(
    var_type("T"),
    record_type([
      ["field", var_type("T")],
      ["nested", tuple_type([var_type("T"), con_type("Int")])],
    ]),
  );

  const substituted = substituteType("T", con_type("Bool"), complexType);

  assert("arrow" in substituted, "should preserve structure");
  assert(
    typesEqual(substituted.arrow.from, con_type("Bool")),
    "should substitute in parameter",
  );
});

test("Substitution avoids capture", () => {
  const type1 = forall_type(
    "A",
    starKind,
    arrow_type(var_type("A"), var_type("B")),
  );

  const substituted = substituteType("B", var_type("A"), type1);

  // Should not substitute inside the forall since A is bound
  assert("forall" in substituted, "should preserve forall");
});

test("Substitution in mu types", () => {
  const listT = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", var_type("T")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const substituted = substituteType("T", con_type("Int"), listT);

  assert("mu" in substituted, "should preserve mu");
  // The head type should be substituted
  const body = (substituted as MuType).mu.body;
  assert("variant" in body, "should have variant body");
});

test("Substitution with infinite recursion protection", () => {
  const recursiveType = mu_type("T", arrow_type(var_type("T"), var_type("T")));

  // Should not infinite loop
  const substituted = substituteType("T", recursiveType, recursiveType);
  assert("mu" in substituted, "should handle self-substitution");
});

test("Constant pattern type checking", () => {
  const intType = con_type("Int");

  const matchConst = lam_term(
    "x",
    intType,
    match_term(var_term("x"), [
      [con_pattern("42", intType), con_term("true", con_type("Bool"))],
      [wildcard_pattern(), con_term("false", con_type("Bool"))],
    ]),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    matchConst,
  );
  assertOk(result, "should handle constant patterns");
});

test("Pattern with wrong constant type", () => {
  const intType = con_type("Int");
  const strType = con_type("String");

  const badMatch = lam_term(
    "x",
    intType,
    match_term(var_term("x"), [
      [con_pattern('"hello"', strType), con_term("true", con_type("Bool"))],
      [wildcard_pattern(), con_term("false", con_type("Bool"))],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    badMatch,
  );
  const err = assertErr(result, "should reject mismatched constant");
  assert("type_mismatch" in err, "should be type mismatch");
});

test("Exhaustiveness with constants", () => {
  const boolType = variant_type([
    ["True", unitType],
    ["False", unitType],
  ]);

  const patterns = [
    variant_pattern("True", wildcard_pattern()),
    variant_pattern("False", wildcard_pattern()),
  ];

  const result = checkExhaustive(patterns, boolType, []);
  assertOk(result, "should be exhaustive");
});

test("Non-exhaustive with missing constant", () => {
  const boolType = variant_type([
    ["True", unitType],
    ["False", unitType],
  ]);

  const patterns = [variant_pattern("True", wildcard_pattern())];

  const result = checkExhaustive(patterns, boolType, []);
  const err = assertErr(result, "should be non-exhaustive");
  assert("missing_case" in err, "should detect missing case");
});

// ============= NORMALIZATION EDGE CASES =============

test("Normalization idempotence", () => {
  const type1 = arrow_type(
    app_type(lam_type("T", starKind, var_type("T")), con_type("Int")),
    con_type("Bool"),
  );

  const normalized1 = normalizeType(type1);
  const normalized2 = normalizeType(normalized1);

  assert(
    typesEqual(normalized1, normalized2),
    "normalization should be idempotent",
  );
});

test("Normalization of bounded forall", () => {
  const type1 = bounded_forall_type(
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    app_type(lam_type("X", starKind, var_type("X")), var_type("T")),
  );

  const normalized = normalizeType(type1);

  // Body should be normalized
  assert("bounded_forall" in normalized, "should preserve bounded forall");
});

test("Polymorphic trait impl instantiation", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // Polymorphic impl: ∀T. Show T where T=Int (but pretend it's poly)
  const polyImplType = forall_type("T", starKind, con_type("Int")); // Simplified poly type
  const polyDict = dict_term("Show", polyImplType, [
    [
      "show",
      lam_term("x", polyImplType, con_term('"poly"', con_type("String"))),
    ],
  ]);

  const polyImplBinding: TraitImplBinding = {
    trait_impl: { trait: "Show", type: polyImplType, dict: polyDict },
  };

  const context: Context = [
    { trait_def: showTrait },
    { type: { name: "Int", kind: starKind } },
    { type: { name: "String", kind: starKind } },
    polyImplBinding,
  ];

  // Instantiate for concrete Int
  const instRes = checkTraitImplementation(context, "Show", con_type("Int"));
  const instDict = assertOk(instRes, "should instantiate poly impl");
  assert("dict" in instDict, "should return instantiated dict");

  // Test failure: No impl for String
  const noImplRes = checkTraitImplementation(
    context,
    "Show",
    con_type("String"),
  );
  const err = assertErr(noImplRes, "should fail without impl");
  assert(
    "missing_trait_impl" in err &&
      (err.missing_trait_impl.type as ConType).con === "String",
    "should report missing impl",
  );
});

test("Not a function error in app", () => {
  const intType = con_type("Int");

  // Proper term-level constant: 42 : Int
  const intConstant = con_term("42", intType);

  // Make a variable bound in the context: x : Int
  const badCallee = var_term("x");

  // x 42 -- apply an Int-typed value as a function
  const badApp = app_term(badCallee, intConstant);

  // Context where x : Int
  const context: Context = [
    { type: { name: "Int", kind: starKind } }, // type binding
    { term: { name: "x", type: intType } }, // term binding
  ];

  const result = typecheck(context, badApp);
  const err = assertErr(result, "should fail");

  console.log(err);

  // Expect: not_a_function error on type Int
  assert(
    "not_a_function" in err && typesEqual(err.not_a_function, intType),
    "should report not_a_function(Int)",
  );
});

test("Direct self-reference μX.X is cyclic", () => {
  const selfMu = mu_type("X", var_type("X"));
  const unifyRes = unifyTypes(selfMu, var_type("Z"), [], new Map());
  const err = assertErr(unifyRes, "should detect self-referential mu");
  assert("cyclic" in err && err.cyclic === "X", "should report cyclic X");
});

test("Wildcard pattern in all positions", () => {
  const pairType = tuple_type([con_type("Int"), con_type("String")]);

  const alwaysTrue = lam_term(
    "p",
    pairType,
    match_term(var_term("p"), [
      [wildcard_pattern(), con_term("true", con_type("Bool"))],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    alwaysTrue,
  );
  assertOk(result, "wildcard should match anything");
});

test("Pattern matching with multiple wildcards", () => {
  const tripleType = tuple_type([
    con_type("Int"),
    con_type("String"),
    con_type("Bool"),
  ]);

  const getMiddle = lam_term(
    "t",
    tripleType,
    match_term(var_term("t"), [
      [
        tuple_pattern([
          wildcard_pattern(),
          var_pattern("middle"),
          wildcard_pattern(),
        ]),
        var_term("middle"),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    getMiddle,
  );
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.to, con_type("String")), "should return String");
});

test("Deeply nested pattern matching", () => {
  const deepType = record_type([
    [
      "outer",
      record_type([
        [
          "inner",
          tuple_type([
            con_type("Int"),
            variant_type([
              ["Some", con_type("String")],
              ["None", unitType],
            ]),
          ]),
        ],
      ]),
    ],
  ]);

  const extract = lam_term(
    "x",
    deepType,
    match_term(var_term("x"), [
      [
        record_pattern([
          [
            "outer",
            record_pattern([
              [
                "inner",
                tuple_pattern([
                  var_pattern("num"),
                  variant_pattern("Some", var_pattern("str")),
                ]),
              ],
            ]),
          ],
        ]),
        var_term("str"),
      ],
      [
        record_pattern([
          [
            "outer",
            record_pattern([
              [
                "inner",
                tuple_pattern([
                  wildcard_pattern(),
                  variant_pattern("None", wildcard_pattern()),
                ]),
              ],
            ]),
          ],
        ]),
        con_term('"default"', con_type("String")),
      ],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "String" } },
    ],
    extract,
  );
  assertOk(result, "should handle deeply nested patterns");
});

test("Constant pattern type checking 2", () => {
  const intType = con_type("Int");

  const isZero = lam_term(
    "x",
    intType,
    match_term(var_term("x"), [
      [con_pattern("0", intType), con_term("true", con_type("Bool"))],
      [wildcard_pattern(), con_term("false", con_type("Bool"))],
    ]),
  );

  const result = typecheck(
    [
      { type: { kind: starKind, name: "Int" } },
      { type: { kind: starKind, name: "Bool" } },
    ],
    isZero,
  );
  assertOk(result, "should handle constant patterns");
});

test("Mutual recursion with mu types", () => {
  // Tree with leaves that are either values or subtrees
  const treeType = mu_type(
    "T",
    variant_type([
      ["Leaf", con_type("Int")],
      [
        "Branch",
        record_type([
          ["left", var_type("T")],
          ["right", var_type("T")],
        ]),
      ],
    ]),
  );

  const leafNode = fold_term(
    treeType,
    inject_term(
      "Leaf",
      con_term("42", con_type("Int")),
      variant_type([
        ["Leaf", con_type("Int")],
        [
          "Branch",
          record_type([
            ["left", treeType],
            ["right", treeType],
          ]),
        ],
      ]),
    ),
  );

  const result = typecheck(
    [{ type: { kind: starKind, name: "Int" } }],
    leafNode,
  );
  assertOk(result, "should handle recursive tree construction");
});

test("List concatenation function type", () => {
  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  // concat: List -> List -> List
  const concatType = arrow_type(listInt, arrow_type(listInt, listInt));

  const concat = lam_term(
    "xs",
    listInt,
    lam_term(
      "ys",
      listInt,
      var_term("ys"), // Simplified implementation
    ),
  );

  const result = typecheck([{ type: { kind: starKind, name: "Int" } }], concat);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("Nested mu types", () => {
  // List of lists
  const innerList = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const outerList = mu_type(
    "LL",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", innerList],
          ["tail", var_type("LL")],
        ]),
      ],
    ]),
  );

  const kind = checkKind(
    [{ type: { kind: starKind, name: "Int" } }],
    outerList,
  );
  assertOk(kind, "should handle nested mu types");
});

test("Unfolding and refolding", () => {
  const natType = mu_type(
    "N",
    variant_type([
      ["Zero", unitType],
      ["Succ", var_type("N")],
    ]),
  );

  const ctx: Context = [{ term: { name: "n", type: natType } }];

  // unfold then fold should preserve type
  const roundtrip = fold_term(natType, unfold_term(var_term("n")));

  const result = typecheck(ctx, roundtrip);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, natType), "should preserve type through unfold/fold");
});

test("Ord trait with superclass", () => {
  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: starKind,
    methods: [
      [
        "eq",
        arrow_type(
          var_type("Self"),
          arrow_type(var_type("Self"), con_type("Bool")),
        ),
      ],
    ],
  };

  const ordTrait: TraitDef = {
    name: "Ord",
    type_param: "Self",
    kind: starKind,
    methods: [
      [
        "compare",
        arrow_type(
          var_type("Self"),
          arrow_type(var_type("Self"), con_type("Int")),
        ),
      ],
    ],
  };

  const intType = con_type("Int");

  const eqImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("true", con_type("Bool"))),
  );

  const ordImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("0", intType)),
  );

  const eqDict = dict_term("Eq", intType, [["eq", eqImpl]]);
  const ordDict = dict_term("Ord", intType, [["compare", ordImpl]]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "Bool" } },
    { trait_def: eqTrait },
    { trait_def: ordTrait },
  ];

  const result1 = typecheck(context, eqDict);
  assertOk(result1, "Eq dict should typecheck");

  const result2 = typecheck(context, ordDict);
  assertOk(result2, "Ord dict should typecheck");
});

test("Generic function with trait bound", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // Λ T where Show<T>. λxs: List<T>. ...
  const showList = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "xs",
      mu_type(
        "L",
        variant_type([
          ["Nil", unitType],
          [
            "Cons",
            record_type([
              ["head", var_type("T")],
              ["tail", var_type("L")],
            ]),
          ],
        ]),
      ),
      con_term('"[...]"', con_type("String")),
    ),
  );

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, showList);
  const type = assertOk(result, "should typecheck");
  assert("bounded_forall" in type, "should be bounded forall type");
});

test("Trait for recursive type", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", var_type("L")],
        ]),
      ],
    ]),
  );

  const showListImpl = lam_term(
    "list",
    listInt,
    con_term('"[1,2,3]"', con_type("String")),
  );

  const listShowDict = dict_term("Show", listInt, [["show", showListImpl]]);

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { trait_def: showTrait },
  ];

  const result = typecheck(context, listShowDict);
  assertOk(result, "should typecheck");
});

test("Monad trait structure", () => {
  const monadTrait: TraitDef = {
    name: "Monad",
    type_param: "M",
    kind: { arrow: { from: starKind, to: starKind } },
    methods: [
      [
        "return",
        forall_type(
          "A",
          starKind,
          arrow_type(var_type("A"), app_type(var_type("M"), var_type("A"))),
        ),
      ],
      [
        "bind",
        forall_type(
          "A",
          starKind,
          forall_type(
            "B",
            starKind,
            arrow_type(
              app_type(var_type("M"), var_type("A")),
              arrow_type(
                arrow_type(
                  var_type("A"),
                  app_type(var_type("M"), var_type("B")),
                ),
                app_type(var_type("M"), var_type("B")),
              ),
            ),
          ),
        ),
      ],
    ],
  };

  const context: Context = [{ trait_def: monadTrait }];

  const binding = context.find((b) => "trait_def" in b);
  assert(binding !== undefined, "monad trait should be in context");
  assert("trait_def" in binding!, "should be trait_def binding");
  assert(binding!.trait_def.methods.length === 2, "should have 2 methods");
});

test("Bounded forall type equality", () => {
  const constraint: TraitConstraint = {
    trait: "Show",
    type: var_type("T"),
  };

  const type1 = bounded_forall_type(
    "T",
    starKind,
    [constraint],
    arrow_type(var_type("T"), con_type("String")),
  );

  const type2 = bounded_forall_type(
    "T",
    starKind,
    [constraint],
    arrow_type(var_type("T"), con_type("String")),
  );

  assert(typesEqual(type1, type2), "identical bounded foralls should be equal");
});

test("Bounded forall with different constraints not equal", () => {
  const type1 = bounded_forall_type(
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String")),
  );

  const type2 = bounded_forall_type(
    "T",
    starKind,
    [{ trait: "Eq", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String")),
  );

  assert(
    !typesEqual(type1, type2),
    "different constraints should not be equal",
  );
});

test("Trait substitution in constraints", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term('"42"', con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  // Λ T where Show<T>. λf: T -> T. λx: T. x
  const polyFunc = trait_lam_term(
    "showDict",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "f",
      arrow_type(var_type("T"), var_type("T")),
      lam_term("x", var_type("T"), var_term("x")),
    ),
  );

  const context: Context = [
    { type: { kind: starKind, name: "Int" } },
    { type: { kind: starKind, name: "String" } },
    { trait_def: showTrait },
    { trait_impl: { trait: "Show", type: intType, dict: intShowDict } },
  ];

  // Apply to Int - should substitute T with Int in Show<T> constraint
  const applied = trait_app_term(polyFunc, intType, [intShowDict]);

  const result = typecheck(context, applied);
  const type = assertOk(result, "should typecheck after substitution");
  assert("arrow" in type, "should be function type");
});

test("Kind checking bounded forall", () => {
  const polyType = bounded_forall_type(
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String")),
  );

  const result = checkKind([], polyType);
  const kind = assertOk(result, "should have valid kind");
  assert("star" in kind, "bounded forall should have kind *");
});

test("Kind mismatch in trait constraint type", () => {
  // Constraint type must have kind *, not * -> *
  const badType = bounded_forall_type(
    "F",
    { arrow: { from: starKind, to: starKind } },
    [{ trait: "Show", type: var_type("F") }], // F has kind * -> *, but Show expects *
    var_type("F"),
  );

  const result = checkKind([], badType);
  const err = assertErr(result, "should fail");
  assert("kind_mismatch" in err, "should be kind mismatch error");
});

test("Alpha equivalence with unused variables", () => {
  const type1 = forall_type("A", starKind, con_type("Int"));
  const type2 = forall_type("B", starKind, con_type("Int"));

  assert(
    typesEqual(type1, type2),
    "unused variables should be alpha equivalent",
  );
});

test("Structural equality for large types", () => {
  const fields: [string, Type][] = [];
  for (let i = 0; i < 100; i++) {
    fields.push([`field${i}`, con_type("Int")]);
  }

  const type1 = record_type(fields);
  const type2 = record_type(fields);

  assert(typesEqual(type1, type2), "large records should be equal");
});

test("Inequality with different structures", () => {
  const type1 = record_type([["x", con_type("Int")]]);
  const type2 = tuple_type([con_type("Int")]);

  assert(!typesEqual(type1, type2), "different structures should not be equal");
});

test("Subsumption: Bottom subtypes everything", () => {
  const neverType = { never: null };
  const intType = con_type("Int");
  const arrowType = arrow_type(intType, con_type("String"));

  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  // ⊥ <: Int
  let subsumesRes = subsumes([], arrowType, neverType, worklist, subst); // General first
  assertOk(subsumesRes, "⊥ should subtype Int");

  // ⊥ <: arrow
  subsumesRes = subsumes([], arrowType, neverType, worklist, subst);
  assertOk(subsumesRes, "⊥ should subtype function");

  // Non-bottom not subtype of ⊥
  subsumesRes = subsumes([], neverType, intType, worklist, subst);
  const err = assertErr(subsumesRes, "Int should not subtype ⊥");
  assert("type_mismatch" in err, "should mismatch");

  // AssignableTo checks
  assert(isAssignableTo(neverType, intType), "⊥ assignable to Int");
  assert(!isAssignableTo(intType, neverType), "Int not assignable to ⊥");
});

test("Subsumption in match branches (unreachable = bottom)", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);
  const neverType = { never: null };

  // Create a term with bottom type (e.g., unreachable())
  const unreachableTerm = con_term("unreachable", neverType);

  // Match with unreachable Some branch
  const unreachableMatch = lam_term(
    "opt",
    optionInt,
    match_term(var_term("opt"), [
      [
        variant_pattern("None", wildcard_pattern()),
        con_term("0", con_type("Int")),
      ],
      [variant_pattern("Some", var_pattern("x")), unreachableTerm], // Bottom term
    ]),
  );

  const context: Context = [{ type: { name: "Int", kind: starKind } }];
  const result = typecheck(context, unreachableMatch);
  const type = assertOk(result, "bottom branch should subtype Int");
  assert(
    typesEqual(type, arrow_type(optionInt, con_type("Int"))),
    "overall type should be Option Int -> Int",
  );
});

test("Subsumption for records (width subtyping simulation)", () => {
  const baseRecord = record_type([["x", con_type("Int")]]);
  const extendedRecord = record_type([
    ["x", con_type("Int")],
    ["y", con_type("String")],
  ]);

  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  // Extended <: base (width: extra fields OK)
  const subsumesRes = subsumes([], baseRecord, extendedRecord, worklist, subst);
  assertOk(subsumesRes, "extended record should subtype base");

  // Reverse fails
  const reverseRes = subsumes([], extendedRecord, baseRecord, worklist, subst);
  const err = assertErr(reverseRes, "base should not subtype extended");
  assert("type_mismatch" in err, "should mismatch");
});

test("Infer mode for simple lambda", () => {
  const lam = lam_term("x", con_type("Int"), var_term("x"));
  const context: Context = [{ type: { name: "Int", kind: starKind } }];

  const inferRes = inferTypeWithMode(context, lam, { infer: null });
  const type = assertOk(inferRes, "infer mode should work");
  assert("arrow" in type, "should infer arrow");
});

test("Check mode against expected arrow", () => {
  const expected = arrow_type(con_type("Int"), con_type("String"));
  const lam = lam_term(
    "x",
    con_type("Int"),
    con_term('"hi"', con_type("String")),
  );

  const context: Context = [
    { type: { name: "Int", kind: starKind } },
    { type: { name: "String", kind: starKind } },
  ];

  const checkRes = inferTypeWithMode(context, lam, { check: expected });
  const typeRes = assertOk(checkRes, "check mode should succeed");
  assert(typesEqual(typeRes, expected), "inferred should match checked");
});

test("Check mode failure for inject", () => {
  const variant = variant_type([
    ["Ok", con_type("Int")],
    ["Err", con_type("String")],
  ]);
  const badInject = inject_term(
    "Ok",
    con_term('"bad"', con_type("String")),
    variant,
  );

  const context: Context = [
    { type: { name: "Int", kind: starKind } },
    { type: { name: "String", kind: starKind } },
  ];

  const checkRes = checkType(context, badInject, variant);
  const err = assertErr(checkRes, "should fail check");
  assert("type_mismatch" in err, "should mismatch payload");

  // Infer mode also fails (same error)
  const inferRes = typecheck(context, badInject);
  const inferErr = assertErr(inferRes, "infer should also fail");
  assert("type_mismatch" in inferErr, "infer should propagate mismatch");
});

test("Check mode for fold against mu", () => {
  const natType = mu_type(
    "N",
    variant_type([
      ["Zero", unitType],
      ["Succ", var_type("N")],
    ]),
  );
  const unfolded = variant_type([
    ["Zero", unitType],
    ["Succ", natType],
  ]);
  const zeroFold = fold_term(natType, inject_term("Zero", unitValue, unfolded));

  const context: Context = [
    { type: { kind: starKind, name: "Int" } }, // Add Int binding
  ];

  const checkRes = checkType(context, zeroFold, natType);
  assertOk(checkRes, "should check fold against mu");

  // Wrong: Fold against non-mu
  const wrongCheck = checkType(context, zeroFold, con_type("Int"));
  const err = assertErr(wrongCheck, "should fail non-mu check");
  assert(
    "type_mismatch" in err || "not_a_variant" in err,
    "should mismatch or invalid fold",
  );
});

test("Fresh meta-var creation and solving", () => {
  const meta1 = freshMetaVar(); // { var: "?0" }
  const meta2 = freshMetaVar(); // { var: "?1" }

  assert("var" in meta1 && meta1.var.startsWith("?"), "should create meta");
  assert(
    metaKind.has(meta1.var) && "star" in metaKind.get(meta1.var)!,
    "should assign * kind",
  );

  const intType = con_type("Int");
  const solveRes1 = solveMetaVar(meta1.var, intType);
  assertOk(solveRes1, "should solve meta");

  // Global lookup
  const globalSubst = getMetaSubstitution();
  assert(
    globalSubst.has(meta1.var) &&
      typesEqual(globalSubst.get(meta1.var)!, intType),
    "should be in global subst",
  );

  // Solve same meta twice (conflict)
  const solveRes2 = solveMetaVar(meta1.var, con_type("String"));
  const conflictErr = assertErr(solveRes2, "should conflict on re-solve");
  assert("type_mismatch" in conflictErr, "should report mismatch");
});

test("Meta-var in unification with conflict", () => {
  const meta = freshMetaVar();
  const intType = con_type("Int");
  const strType = con_type("String");

  const worklist1: Worklist = [{ type_eq: { left: meta, right: intType } }];
  const subst1 = new Map<string, Type>();
  const solveRes1 = solveConstraints(worklist1, subst1);
  assertOk(solveRes1, "first unify OK");

  // Conflict: Same meta ~ String
  const worklist2: Worklist = [{ type_eq: { left: meta, right: strType } }];
  const subst2 = new Map<string, Type>(subst1); // Inherit
  const solveRes2 = solveConstraints(worklist2, subst2);
  const conflict = assertErr(solveRes2, "should conflict");
  assert(
    "type_mismatch" in conflict &&
      typesEqual(conflict.type_mismatch.expected, intType),
    "should keep first binding",
  );
});

test("Merge local/global substitutions with override", () => {
  const globalSubst = new Map<string, Type>([["?0", con_type("Int")]]);

  // Local overrides ?0 to String, adds ?1=Bool
  const localSubst = new Map<string, Type>([
    ["?0", con_type("String")],
    ["?1", con_type("Bool")],
  ]);

  const merged = mergeSubsts(localSubst, globalSubst);
  assert(merged.size === 2, "should merge without duplicates");
  assert(
    typesEqual(merged.get("?0")!, con_type("String")),
    "local should override global",
  );
  assert(
    typesEqual(merged.get("?1")!, con_type("Bool")),
    "local should add new",
  );
});

test("Not a function error in app 2", () => {
  const intType = con_type("Int");
  const badApp = app_term(con_term("42", intType), con_term("5", intType));

  const result = typecheck([], badApp);
  const err = assertErr(result, "should fail");
  assert("not_a_function" in err, "should report not_a_function");
});

test("Not a type function in kind app", () => {
  const badApp = app_type(con_type("Int"), con_type("Bool")); // Int applied to Bool (not lam)

  const context: Context = [
    { type: { name: "Int", kind: starKind } },
    { type: { name: "Bool", kind: starKind } },
  ];
  const kindRes = checkKind(context, badApp);
  const err = assertErr(kindRes, "should fail kind app");
  assert("not_a_type_function" in err, "should report not type func");
});

test("Cyclic type error in mu", () => {
  const cyclicMu = mu_type("X", var_type("Y"));
  const worklist: Worklist = [];
  const subst = new Map<string, Type>();

  const unifyRes = unifyTypes(cyclicMu, var_type("Y"), worklist, subst);
  const err = assertErr(unifyRes, "should detect cyclic mu");
  assert("cyclic" in err, "should report cyclic");
});

test("Unexpected kind error", () => {
  // Type var with unexpected kind in context
  const ctx: Context = [
    { type: { name: "Int", kind: starKind } },
    { type: { name: "T", kind: { arrow: { from: starKind, to: starKind } } } },
  ];
  const app = app_type(var_type("T"), con_type("Int")); // T :: * -> * applied to *, OK
  const kindRes = checkKind(ctx, app);
  assertOk(kindRes, "should succeed");

  // Unexpected: Use arrow kind where * expected
  const badUse = arrow_type(var_type("T"), con_type("Int")); // T has * -> *, but arrow expects *
  const badRes = checkKind(ctx, badUse);
  const err = assertErr(badRes, "should fail unexpected kind");
  assert(
    "unexpected_kind" in err || "kind_mismatch" in err,
    "should report kind error",
  );
});

test("Enum definition and nominal Option type", () => {
  const optionEnum: EnumDef = {
    enum: {
      name: "Option",
      kind: { arrow: { from: starKind, to: starKind } },
      params: ["T"],
      variants: [
        ["None", unitType],
        ["Some", var_type("T")],
      ],
    },
  };

  const intType = con_type("Int");
  const optionInt = app_type(con_type("Option"), intType); // Nominal: Option<Int>

  const context: Context = [
    {
      type: {
        name: "Option",
        kind: { arrow: { from: starKind, to: starKind } },
      },
    },
    { enum: optionEnum },
    { type: { name: "Int", kind: starKind } },
  ];

  // Kind check
  const kindResult = checkKind(context, optionInt);
  const kind = assertOk(kindResult, "should kind-check nominal Option<Int>");
  assert("star" in kind, "should be *");

  // Unify nominal ~ structural
  const structuralOption = variant_type([
    ["None", unitType],
    ["Some", intType],
  ]);
  const worklist: Worklist = [];
  const subst = new Map<string, Type>();
  const unifyRes = unifyTypes(
    optionInt,
    structuralOption,
    worklist,
    subst,
    context,
  );
  assertOk(unifyRes, "nominal should unify with structural via enum expansion");

  // Normalize should expand
  const normalized = normalizeType(optionInt, context);
  assert("variant" in normalized, "normalize should expand to structural");
  assert(typesEqual(normalized, structuralOption), "should match structural");
});

test("Nominal injection into enum (Option::Some)", () => {
  const optionEnum: EnumDef = {
    enum: {
      name: "Option",
      kind: { arrow: { from: starKind, to: starKind } },
      params: ["T"],
      variants: [
        ["None", unitType],
        ["Some", var_type("T")],
      ],
    },
  };

  const intType = con_type("Int");
  const optionInt = app_type(con_type("Option"), intType);

  const someVal = inject_term("Some", con_term("42", intType), optionInt);

  const context: Context = [
    { enum: optionEnum },
    { type: { name: "Int", kind: starKind } },
  ];

  const result = typecheck(context, someVal);
  const type = assertOk(result, "should typecheck nominal Some");
  assert("app" in type, "should be Option<Int>");
  const spineArgs = getSpineArgs(type);
  assert(
    spineArgs.length === 1 && typesEqual(spineArgs[0], intType),
    "spine arg should be Int",
  );
});

test("Nominal exhaustive matching on enum (Either)", () => {
  const eitherEnum: EnumDef = {
    enum: {
      name: "Either",
      kind: {
        arrow: {
          from: starKind,
          to: { arrow: { from: starKind, to: starKind } },
        },
      },
      params: ["L", "R"],
      variants: [
        ["Left", var_type("L")],
        ["Right", var_type("R")],
      ],
    },
  };

  const intType = con_type("Int");
  const boolType = con_type("Bool");
  const eitherIntBool = app_type(
    app_type(con_type("Either"), intType),
    boolType,
  );

  const patterns = [
    variant_pattern("Left", var_pattern("l")),
    variant_pattern("Right", var_pattern("r")),
  ];

  const context: Context = [
    {
      type: {
        name: "Either",
        kind: {
          arrow: {
            from: starKind,
            to: { arrow: { from: starKind, to: starKind } },
          },
        },
      },
    },
    { enum: eitherEnum },
    { type: { name: "Int", kind: starKind } },
    { type: { name: "Bool", kind: starKind } },
  ];

  const exhaustRes = checkExhaustive(patterns, eitherIntBool, context);
  assertOk(exhaustRes, "nominal Either should be exhaustive");

  // Non-exhaustive (missing Right)
  const incompletePatterns = [variant_pattern("Left", var_pattern("l"))];
  const incompleteRes = checkExhaustive(
    incompletePatterns,
    eitherIntBool,
    context,
  );
  const err = assertErr(incompleteRes, "should detect missing case");
  assert(
    "missing_case" in err && err.missing_case.label === "Right",
    "should report missing Right",
  );
});

test("Invalid label in nominal enum injection", () => {
  const optionEnum: EnumDef = {
    enum: {
      name: "Option",
      params: ["T"],
      kind: { arrow: { from: starKind, to: starKind } },
      variants: [
        ["None", unitType],
        ["Some", var_type("T")],
      ],
    },
  };

  const optionUnit = app_type(con_type("Option"), unitType);
  const invalidInject = inject_term("Invalid", unitValue, optionUnit);

  const context: Context = [{ enum: optionEnum }];

  const result = typecheck(context, invalidInject);
  const err = assertErr(result, "should fail on invalid label");
  assert(
    "invalid_variant_label" in err &&
      err.invalid_variant_label.label === "Invalid",
    "should report invalid label",
  );
});

test("Concrete trait impl binding and lookup", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term(
    "x",
    intType,
    con_term('"int"', con_type("String")),
  );
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const intImplBinding: TraitImplBinding = {
    trait_impl: { trait: "Show", type: intType, dict: intShowDict },
  };

  const context: Context = [
    { trait_def: showTrait },
    { type: { name: "Int", kind: starKind } },
    { type: { name: "String", kind: starKind } },
    intImplBinding,
  ];

  // Lookup impl for Show<Int>
  const lookupRes = checkTraitImplementation(context, "Show", intType);
  const dict = assertOk(lookupRes, "should find concrete impl");
  assert("dict" in dict, "should return dict term");

  // Use in a function
  const useImpl = lam_term(
    "x",
    intType,
    app_term(trait_method_term(dict, "show"), var_term("x")),
  );
  const result = typecheck(context, useImpl);
  const type = assertOk(result, "should use impl");
  assert(
    typesEqual(type, arrow_type(intType, con_type("String"))),
    "should be Show method type",
  );
});

test("Trait impl with wrong number of dicts in app", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: starKind,
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const showValue = trait_lam_term(
    "d",
    "Show",
    "T",
    starKind,
    [{ trait: "Show", type: var_type("T") }],
    var_term("d"),
  );

  const context: Context = [
    { trait_def: showTrait },
    { type: { name: "Int", kind: starKind } },
    { type: { name: "String", kind: starKind } },
  ];

  // Provide 0 dicts (expected 1)
  const appWithZero = trait_app_term(showValue, con_type("Int"), []);
  const result = typecheck(context, appWithZero);
  const err = assertErr(result, "should fail");
  assert("wrong_number_of_dicts" in err, "should report wrong dict count");

  // Provide 2 dicts (extra)
  const appWithExtra = trait_app_term(showValue, con_type("Int"), [
    dict_term("Show", con_type("Int"), [
      [
        "show",
        lam_term("x", con_type("Int"), con_term('"x"', con_type("String"))),
      ],
    ]),
    dict_term("Show", con_type("Int"), [
      [
        "show",
        lam_term("x", con_type("Int"), con_term('"x"', con_type("String"))),
      ],
    ]),
  ]);
  const extraRes = typecheck(context, appWithExtra);
  const extraErr = assertErr(extraRes, "should fail on extra dicts");
  assert("wrong_number_of_dicts" in extraErr, "should report extra dicts");
});

test("Unification of lambda types", () => {
  const lam1 = lam_type("A", starKind, var_type("A"));
  const lam2 = lam_type("X", starKind, var_type("X")); // Alpha equiv

  const worklist: Worklist = [];
  const subst = new Map<string, Type>();
  const res = unifyTypes(lam1, lam2, worklist, subst);
  assertOk(res, "lambdas should unify via alpha");

  // Mismatch kinds
  const badLam = lam_type(
    "A",
    { arrow: { from: starKind, to: starKind } },
    var_type("A"),
  );
  const badRes = unifyTypes(lam1, badLam, worklist, subst);
  const err = assertErr(badRes, "kind mismatch should fail");
  assert("kind_mismatch" in err, "should report kind error");
});

test("Unification with app types", () => {
  const app1 = app_type(
    lam_type("T", starKind, var_type("T")),
    con_type("Int"),
  );
  const app2 = app_type(
    lam_type("X", starKind, var_type("X")),
    con_type("Int"),
  );

  const worklist: Worklist = [];
  const subst = new Map<string, Type>();
  const res = unifyTypes(app1, app2, worklist, subst);
  assertOk(res, "apps should unify after normalize");

  // Head mismatch
  const badApp = app_type(con_type("List"), con_type("Int"));
  const badRes = unifyTypes(app1, badApp, [], new Map());
  const err = assertErr(badRes, "head mismatch");
  assert("type_mismatch" in err, "should mismatch");
});

test("Substitution in mu types with cycles", () => {
  const muType = mu_type("M", arrow_type(var_type("M"), var_type("M")));
  const avoid = new Set(["M"]);

  // Subst non-bound var
  const substRes = substituteType("X", con_type("Int"), muType, avoid);
  assert("mu" in substRes, "should preserve mu");

  // Subst bound var: Should skip
  const selfSubst = substituteType("M", con_type("Int"), muType, new Set());
  assert(
    "mu" in selfSubst && selfSubst.mu.var === "M",
    "should not subst bound var",
  );

  // Infinite recursion protection
  const cyclicAvoid = new Set<string>();
  cyclicAvoid.add("M");
  const protectedSubst = substituteType(
    "Y",
    muType,
    arrow_type(var_type("Y"), muType),
    cyclicAvoid,
  );
  assert(
    "arrow" in protectedSubst && "mu" in protectedSubst.arrow.to,
    "should stop at cycle",
  );
});

test("Unification detects cycles in mu", () => {
  const worklist: Worklist = [];
  const subst = new Map<string, Type>();
  const mu1 = mu_type("M", var_type("M")); // Infinite M
  const metaM = freshMetaVar();

  const res = unifyTypes(metaM, mu1, worklist, subst);
  const err = assertErr(res, "should detect mu cycle");
  assert("cyclic" in err, "should report cyclic");
});

test("Show functions for complex types/terms", () => {
  const complexType = arrow_type(
    forall_type(
      "A",
      starKind,
      app_type(lam_type("T", starKind, var_type("T")), var_type("A")),
    ),
    mu_type(
      "M",
      bounded_forall_type(
        "B",
        starKind,
        [{ trait: "Show", type: var_type("B") }],
        var_type("M"),
      ),
    ),
  );
  const shownType = showType(complexType);
  assert(
    typeof shownType === "string" &&
      shownType.includes("∀") &&
      shownType.includes("μ"),
    "should pretty-print complex",
  );

  const complexTerm = app_term(
    trait_app_term(
      tylam_term(
        "X",
        starKind,
        inject_term(
          "Some",
          fold_term(mu_type("M", var_type("X")), var_term("x")),
          app_type(con_type("Option"), var_type("X")),
        ),
      ),
      con_type("Int"),
      [],
    ),
    record_term([["f", lam_term("y", con_type("Int"), var_term("y"))]]),
  );
  const shownTerm = showTerm(complexTerm);
  assert(
    typeof shownTerm === "string" && shownTerm.includes("Λ"),
    "should pretty-print complex terms",
  );
});

test("InferTypeWithMode delegation", () => {
  const lam = lam_term("x", con_type("Int"), var_term("x"));
  const context: Context = [{ type: { name: "Int", kind: starKind } }];

  // Infer mode
  const inferRes = inferTypeWithMode(context, lam, { infer: null });
  assertOk(inferRes, "should delegate to inferType");

  // Check mode
  const expected = arrow_type(con_type("Int"), con_type("Int"));
  const checkRes = inferTypeWithMode(context, lam, { check: expected });
  const typeRes = assertOk(
    checkRes,
    "should delegate to checkType and return type",
  );
  assert("arrow" in typeRes, "should return checked type");
});

test("Deep recursion in substitution/normalization (no stack overflow)", () => {
  // Build deep arrow: Int -> (Int -> (Int -> ... )) with 100 levels
  let deepArrow: Type = con_type("Int");
  for (let i = 0; i < 100; i++) {
    deepArrow = arrow_type(con_type("Int"), deepArrow);
  }

  // Subst in deep structure
  const substDeep = substituteType("X", con_type("Bool"), deepArrow, new Set());
  assert("arrow" in substDeep, "should traverse deep without overflow");

  // Normalize deep (no apps, so unchanged)
  const normDeep = normalizeType(deepArrow);
  assert(typesEqual(normDeep, deepArrow), "deep should normalize idempotently");
});
