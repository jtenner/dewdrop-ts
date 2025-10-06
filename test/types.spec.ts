// tests.ts
import {
  Type,
  Term,
  Kind,
  Pattern,
  Context,
  typecheck,
  checkKind,
  showType,
  showKind,
  typesEqual,
  var_type,
  arrow_type,
  forall_type,
  con_type,
  record_type,
  variant_type,
  var_term,
  lam_term,
  app_term,
  tylam_term,
  tyapp_term,
  con_term,
  record_term,
  project_term,
  inject_term,
  match_term,
  var_pattern,
  wildcard_pattern,
  variant_pattern,
  record_pattern,
  unitType,
  unitValue,
  mu_type,
  fold_term,
  unfold_term,
} from "../src/types_system_f_omega";
import { test } from "bun:test";

// Simple test framework
type TestResult = { passed: number; failed: number; errors: string[] };

function assert(condition: boolean, message: string): asserts condition {
  if (!condition) {
    throw new Error(`Assertion failed: ${message}`);
  }
}

function assertOk<T>(result: { ok: T } | { err: any }, message: string): T {
  if ("err" in result) {
    throw new Error(`Expected ok but got error: ${JSON.stringify(result.err)} - ${message}`);
  }
  return result.ok;
}

function assertErr<E>(result: { ok: any } | { err: E }, message: string): E {
  if ("ok" in result) {
    throw new Error(`Expected error but got ok: ${JSON.stringify(result.ok)} - ${message}`);
  }
  return result.err;
}



test("Type variable kinding", () => {
  const ctx: Context = [{ type: { name: "T", kind: { star: null } } }];
  const result = checkKind(ctx, var_type("T"));
  const kind = assertOk(result, "should infer kind *");
  assert("star" in kind, "should be star kind");
});

test("Constant type kinding", () => {
  const result = checkKind([], con_type("Int"));
  const kind = assertOk(result, "should infer kind *");
  assert("star" in kind, "should be star kind");
});

test("Arrow type kinding", () => {
  const intType = con_type("Int");
  const boolType = con_type("Bool");
  const arrowTy = arrow_type(intType, boolType);
  const result = checkKind([], arrowTy);
  const kind = assertOk(result, "should infer kind *");
  assert("star" in kind, "should be star kind");
});

test("Higher-kinded type", () => {
  const ctx: Context = [
    { type: { name: "F", kind: { arrow: { from: { star: null }, to: { star: null } } } } },
  ];
  const result = checkKind(ctx, var_type("F"));
  const kind = assertOk(result, "should infer kind * -> *");
  assert("arrow" in kind, "should be arrow kind");
});

test("Identity function", () => {
  const intType = con_type("Int");
  const identity = lam_term("x", intType, var_term("x"));
  const result = typecheck([], identity);
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
  const result = typecheck([], app);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, intType), "result should be Int");
});

test("Function composition", () => {
  const intType = con_type("Int");
  const strType = con_type("String");
  const boolType = con_type("Bool");

  // f: Int -> String
  const f = lam_term("x", intType, con_term("\"str\"", strType));
  // g: String -> Bool  
  const g = lam_term("y", strType, con_term("true", boolType));
  // compose g f: Int -> Bool
  const composed = lam_term("z", intType, app_term(g, app_term(f, var_term("z"))));

  const result = typecheck([], composed);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "argument should be Int");
  assert(typesEqual(type.arrow.to, boolType), "return should be Bool");
});

test("Unbound variable error", () => {
  const result = typecheck([], var_term("x"));
  const err = assertErr(result, "should fail");
  assert("unbound" in err, "should be unbound variable error");
});

test("Type mismatch in application", () => {
  const intType = con_type("Int");
  const strType = con_type("String");
  const f = lam_term("x", intType, var_term("x"));
  const arg = con_term("\"hello\"", strType);
  const result = typecheck([], app_term(f, arg));
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});


test("Polymorphic identity", () => {
  const polyId = tylam_term("T", { star: null }, lam_term("x", var_type("T"), var_term("x")));
  const result = typecheck([], polyId);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be forall type");
  assert(type.forall.var === "T", "should quantify over T");
});

test("Type application", () => {
  const intType = con_type("Int");
  const polyId = tylam_term("T", { star: null }, lam_term("x", var_type("T"), var_term("x")));
  const intId = tyapp_term(polyId, intType);
  const result = typecheck([], intId);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.from, intType), "should be Int -> Int");
});

test("Polymorphic constant function", () => {
  const constFn = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
      lam_term(
        "x",
        var_type("A"),
        lam_term("y", var_type("B"), var_term("x"))
      )
    )
  );
  const result = typecheck([], constFn);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Simple record", () => {
  const personType = record_type([
    ["name", con_type("String")],
    ["age", con_type("Int")],
  ]);

  const person = record_term([
    ["name", con_term("\"Alice\"", con_type("String"))],
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
    ["name", con_term("\"Alice\"", con_type("String"))],
    ["age", con_term("30", con_type("Int"))],
  ]);

  const getName = project_term(person, "name");
  const result = typecheck([], getName);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, con_type("String")), "should be String");
});

test("Missing field projection", () => {
  const person = record_term([
    ["name", con_term("\"Alice\"", con_type("String"))],
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
    ["name", con_term("\"Bob\"", con_type("String"))],
    [
      "address",
      record_term([
        ["street", con_term("\"123 Main\"", con_type("String"))],
        ["city", con_term("\"Boston\"", con_type("String"))],
      ]),
    ],
  ]);

  const getCity = project_term(project_term(person, "address"), "city");
  const result = typecheck([], getCity);
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

test("Option type", () => {
  const optionInt = variant_type([
    ["None", unitType],
    ["Some", con_type("Int")],
  ]);

  const someVal = inject_term("Some", con_term("42", con_type("Int")), optionInt);
  const result = typecheck([], someVal);
  const type = assertOk(result, "should typecheck");
  assert("variant" in type, "should be variant type");
  assert(type.variant.length === 2, "should have 2 cases");
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

  const wrongType = inject_term("Some", con_term("\"str\"", con_type("String")), optionInt);
  const result = typecheck([], wrongType);
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
      [variant_pattern("True", wildcard_pattern()), inject_term("False", unitValue, boolType)],
      [variant_pattern("False", wildcard_pattern()), inject_term("True", unitValue, boolType)],
    ])
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
      [variant_pattern("None", wildcard_pattern()), con_term("0", con_type("Int"))],
      [variant_pattern("Some", var_pattern("x")), var_term("x")],
    ])
  );

  const result = typecheck([], unwrap);
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
    ])
  );

  const result = typecheck([], getX);
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

  const result = typecheck([], unwrapAll);
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
      [variant_pattern("None", wildcard_pattern()), con_term("0", con_type("Int"))],
    ])
  );

  const result = typecheck([], incomplete);
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
      [variant_pattern("None", wildcard_pattern()), con_term("0", con_type("Int"))],
      [variant_pattern("Some", var_pattern("x")), con_term("\"str\"", con_type("String"))],
    ])
  );

  const result = typecheck([], inconsistent);
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});

test("Polymorphic map for Option", () => {
  const mapOption = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
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
                ])
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
                ])
              ),
            ],
          ])
        )
      )
    )
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
    ["Cons", record_type([["head", con_type("Int")], ["tail", unitType]])],
  ]);

  const sumList = lam_term(
    "list",
    listInt,
    match_term(var_term("list"), [
      [variant_pattern("Nil", wildcard_pattern()), con_term("0", con_type("Int"))],
      [
        variant_pattern(
          "Cons",
          record_pattern([
            ["head", var_pattern("h")],
            ["tail", wildcard_pattern()],
          ])
        ),
        var_term("h"), // Simplified - would normally recurse
      ],
    ])
  );

  const result = typecheck([], sumList);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("State monad type", () => {
  // State s a = s -> (a, s)
  const stateType = (s: Type, a: Type): Type =>
    arrow_type(s, record_type([["value", a], ["state", s]]));

  const returnState = tylam_term(
    "S",
    { star: null },
    tylam_term(
      "A",
      { star: null },
      lam_term(
        "x",
        var_type("A"),
        lam_term(
          "s",
          var_type("S"),
          record_term([
            ["value", var_term("x")],
            ["state", var_term("s")],
          ])
        )
      )
    )
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
    { star: null },
    tylam_term(
      "B",
      { star: null },
      tylam_term(
        "C",
        { star: null },
        tylam_term(
          "D",
          { star: null },
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
                      eitherType(var_type("C"), var_type("D"))
                    ),
                  ],
                  [
                    variant_pattern("Right", var_pattern("y")),
                    inject_term(
                      "Right",
                      app_term(var_term("g"), var_term("y")),
                      eitherType(var_type("C"), var_type("D"))
                    ),
                  ],
                ])
              )
            )
          )
        )
      )
    )
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
    ])
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
    ])
  );

  const zero = fold_term(
    natType,
    inject_term("Zero", unitValue, variant_type([
      ["Zero", unitType],
      ["Succ", natType],
    ]))
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
    ])
  );

  const succ = lam_term(
    "n",
    natType,
    fold_term(
      natType,
      inject_term("Succ", var_term("n"), variant_type([
        ["Zero", unitType],
        ["Succ", natType],
      ]))
    )
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
    ])
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
    ])
  );

  const kind = checkKind([], listInt);
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
    ])
  );

  const emptyList = fold_term(
    listInt,
    inject_term("Nil", unitValue, variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", listInt],
        ]),
      ],
    ]))
  );

  const result = typecheck([], emptyList);
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
    ])
  );

  const emptyList = fold_term(
    listInt,
    inject_term("Nil", unitValue, variant_type([
      ["Nil", unitType],
      [
        "Cons",
        record_type([
          ["head", con_type("Int")],
          ["tail", listInt],
        ]),
      ],
    ]))
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
      ])
    )
  );

  const result = typecheck([], oneElementList);
  const type = assertOk(result, "should typecheck");
  assert(typesEqual(type, listInt), "should be List Int type");
});

