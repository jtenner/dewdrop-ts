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
  tuple_type,
  tuple_term,
  tuple_project_term,
  tuple_pattern,
  app_type,
  lam_type,
  let_term,
  normalizeType,
  substituteType,
  MuType,
  TraitDef,
  TraitConstraint,
  bounded_forall_type,
  trait_lam_term,
  trait_app_term,
  dict_term,
  trait_method_term,
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


test("Simple tuple", () => {
  const tupleType = tuple_type([con_type("Int"), con_type("String")]);
  const tuple = tuple_term([
    con_term("42", con_type("Int")),
    con_term("\"hello\"", con_type("String")),
  ]);

  const result = typecheck([], tuple);
  const type = assertOk(result, "should typecheck");
  assert("tuple" in type, "should be tuple type");
  assert(type.tuple.length === 2, "should have 2 elements");
});

test("Tuple projection", () => {
  const tuple = tuple_term([
    con_term("42", con_type("Int")),
    con_term("\"hello\"", con_type("String")),
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
    con_term("\"hello\"", con_type("String")),
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
    con_term("\"outer\"", con_type("String")),
  ]);

  const getInnerFirst = tuple_project_term(
    tuple_project_term(outerTuple, 0),
    0
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
    ])
  );

  const result = typecheck([], swap);
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
    ])
  );

  const result = typecheck([], getFirst);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
  assert(typesEqual(type.arrow.to, con_type("Int")), "should return Int");
});

test("Polymorphic fst function", () => {
  const fst = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
      lam_term(
        "p",
        tuple_type([var_type("A"), var_type("B")]),
        tuple_project_term(var_term("p"), 0)
      )
    )
  );

  const result = typecheck([], fst);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Polymorphic snd function", () => {
  const snd = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
      lam_term(
        "p",
        tuple_type([var_type("A"), var_type("B")]),
        tuple_project_term(var_term("p"), 1)
      )
    )
  );

  const result = typecheck([], snd);
  const type = assertOk(result, "should typecheck");
  assert("forall" in type, "should be polymorphic");
});

test("Map function", () => {
  const map = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
      lam_term(
        "f",
        arrow_type(var_type("A"), var_type("B")),
        lam_term(
          "x",
          var_type("A"),
          app_term(var_term("f"), var_term("x"))
        )
      )
    )
  );

  const result = typecheck([], map);
  assertOk(result, "should typecheck");
});

test("Compose function", () => {
  const compose = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
      tylam_term(
        "C",
        { star: null },
        lam_term(
          "f",
          arrow_type(var_type("B"), var_type("C")),
          lam_term(
            "g",
            arrow_type(var_type("A"), var_type("B")),
            lam_term(
              "x",
              var_type("A"),
              app_term(
                var_term("f"),
                app_term(var_term("g"), var_term("x"))
              )
            )
          )
        )
      )
    )
  );

  const result = typecheck([], compose);
  assertOk(result, "should typecheck");
});

test("Flip function", () => {
  const flip = tylam_term(
    "A",
    { star: null },
    tylam_term(
      "B",
      { star: null },
      tylam_term(
        "C",
        { star: null },
        lam_term(
          "f",
          arrow_type(var_type("A"), arrow_type(var_type("B"), var_type("C"))),
          lam_term(
            "b",
            var_type("B"),
            lam_term(
              "a",
              var_type("A"),
              app_term(app_term(var_term("f"), var_term("a")), var_term("b"))
            )
          )
        )
      )
    )
  );

  const result = typecheck([], flip);
  assertOk(result, "should typecheck");
});

test("Type constructor application", () => {
  const listCon = lam_type(
    "T",
    { star: null },
    mu_type(
      "L",
      variant_type([
        ["Nil", unitType],
        ["Cons", record_type([["head", var_type("T")], ["tail", var_type("L")]])],
      ])
    )
  );

  const listInt = app_type(listCon, con_type("Int"));
  const kind = checkKind([], listInt);
  assertOk(kind, "should have valid kind");
});

test("Type constructor kind mismatch", () => {
  const ctx: Context = [
    { type: { name: "F", kind: { arrow: { from: { star: null }, to: { star: null } } } } },
  ];
  
  // Try to apply F to something that's not kind *
  const badApp = app_type(
    var_type("F"),
    lam_type("X", { star: null }, var_type("X"))
  );
  
  const result = checkKind(ctx, badApp);
  const err = assertErr(result, "should fail");
  assert("kind_mismatch" in err, "should be kind mismatch");
});

test("Self-application (should fail)", () => {
  // λx. x x - this should fail in simply typed lambda calculus
  const selfApp = lam_term(
    "x",
    arrow_type(var_type("T"), var_type("T")),
    app_term(var_term("x"), var_term("x"))
  );

  const result = typecheck([], selfApp);
  // This might succeed in System F with proper polymorphism,
  // but worth testing the behavior
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

test("Large record", () => {
  const fields: [string, Term][] = [];
  for (let i = 0; i < 100; i++) {
    fields.push([`field${i}`, con_term(`${i}`, con_type("Int"))]);
  }
  
  const largeRecord = record_term(fields);
  const result = typecheck([], largeRecord);
  assertOk(result, "should handle large records");
});

test("Shadowed variables", () => {
  const shadowed = lam_term(
    "x",
    con_type("Int"),
    lam_term(
      "x",
      con_type("String"),
      var_term("x")
    )
  );

  const result = typecheck([], shadowed);
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
    ])
  );

  const kind = checkKind([], treeInt);
  assertOk(kind, "should have kind *");
});

test("Infinite list (stream) type", () => {
  const streamInt = mu_type(
    "S",
    record_type([
      ["head", con_type("Int")],
      ["tail", var_type("S")],
    ])
  );

  const kind = checkKind([], streamInt);
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
    ])
  );

  const result = typecheck([], toInt);
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
    ])
  );

  const result = typecheck([], extract);
  assertOk(result, "should typecheck");
});

test("let term", () => {
  let intType = con_type("Int");
  const context: Context = [
    { term: { name: "+", type: arrow_type(intType, arrow_type(intType, intType)) } }
  ];
  
  const testTerm = let_term(
    "x",
    con_term("5", intType),
    // Fix: Apply "+" to "x" first, then apply the result to "3"
    app_term(
      app_term(
        var_term("+"),
        var_term("x")
      ),
      con_term("3", intType)
    )
  );
  
  const result = typecheck(context, testTerm);
  assertOk(result, "should typecheck");
});

test("type normalization", () => {
  // Test 1: Beta-reduction of type application
  const idType = lam_type("T", { star: null }, var_type("T"));
  const intType = con_type("Int");
  const appliedType = app_type(idType, intType);
  
  const normalized = normalizeType(appliedType);
  const expected = intType;
  
  assert(
    typesEqual(normalized, expected),
    `Test 1 failed: Expected ${showType(expected)} but got ${showType(normalized)}`
  );
  
  // Test 2: Nested beta-reductions
  const doubleApp = lam_type("A", { star: null }, 
    lam_type("B", { star: null }, 
      arrow_type(var_type("A"), var_type("B"))
    )
  );
  const applied = app_type(
    app_type(doubleApp, con_type("Int")), 
    con_type("Bool")
  );
  
  const normalized2 = normalizeType(applied);
  const expected2 = arrow_type(con_type("Int"), con_type("Bool"));
  
  assert(
    typesEqual(normalized2, expected2),
    `Test 2 failed: Expected ${showType(expected2)} but got ${showType(normalized2)}`
  );
  
  // Test 3: Trivial forall elimination (unused type variable)
  const trivialForall = forall_type(
    "X", 
    { star: null }, 
    con_type("Int")
  );
  
  const normalized3 = normalizeType(trivialForall);
  const expected3 = con_type("Int");
  
  assert(
    typesEqual(normalized3, expected3),
    `Test 3 failed: Expected ${showType(expected3)} but got ${showType(normalized3)}`
  );
  
  // Test 4: Mu types should NOT unfold during normalization
  const listType = mu_type("L", variant_type([
    ["Nil", unitType],
    ["Cons", tuple_type([con_type("Int"), var_type("L")])]
  ]));
  
  const normalized4 = normalizeType(listType);
  
  // It should still be a mu type after normalization
  assert(
    "mu" in normalized4,
    `Test 4 failed: Mu type should not unfold during normalization`
  );
  assert(
    typesEqual(normalized4, listType),
    `Test 4 failed: Mu type should remain unchanged`
  );
  
  // Test 5: Single-element tuple simplification
  const singleTuple = tuple_type([con_type("Int")]);
  const normalized5 = normalizeType(singleTuple);
  const expected5 = con_type("Int");
  
  assert(
    typesEqual(normalized5, expected5),
    `Test 5 failed: Expected ${showType(expected5)} but got ${showType(normalized5)}`
  );
  
  // Test 6: Empty record simplification
  const emptyRecord = record_type([]);
  const normalized6 = normalizeType(emptyRecord);
  
  assert(
    typesEqual(normalized6, unitType),
    `Test 6 failed: Empty record should normalize to Unit`
  );
  
  // Test 7: Complex nested application
  const constType = lam_type("A", { star: null },
    lam_type("B", { star: null },
      var_type("A")
    )
  );
  const applied7 = app_type(
    app_type(constType, con_type("String")),
    con_type("Bool")
  );
  
  const normalized7 = normalizeType(applied7);
  const expected7 = con_type("String");
  
  assert(
    typesEqual(normalized7, expected7),
    `Test 7 failed: Expected ${showType(expected7)} but got ${showType(normalized7)}`
  );
  
  // Test 8: Normalization preserves used forall variables
  const usedForall = forall_type(
    "T",
    { star: null },
    arrow_type(var_type("T"), var_type("T"))
  );
  
  const normalized8 = normalizeType(usedForall);
  
  assert(
    "forall" in normalized8,
    `Test 8 failed: Forall with used variable should be preserved`
  );
  assert(
    typesEqual(normalized8, usedForall),
    `Test 8 failed: Used forall should remain unchanged`
  );
});

// ============= TRAIT SYSTEM TESTS =============

test("Simple trait definition", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
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
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term("\"42\"", con_type("String")));

  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, intShowDict);
  const type = assertOk(result, "should typecheck");
  assert("con" in type, "should be dictionary type");
});

test("Dictionary with missing method", () => {
  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: { star: null },
    methods: [
      ["eq", arrow_type(var_type("Self"), arrow_type(var_type("Self"), con_type("Bool")))],
      ["neq", arrow_type(var_type("Self"), arrow_type(var_type("Self"), con_type("Bool")))],
    ],
  };

  const intType = con_type("Int");
  const eqImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("true", con_type("Bool")))
  );

  // Missing neq method
  const incompleteDict = dict_term("Eq", intType, [["eq", eqImpl]]);

  const context: Context = [{ trait_def: eqTrait }];

  const result = typecheck(context, incompleteDict);
  const err = assertErr(result, "should fail");
  assert("missing_method" in err, "should be missing method error");
});

test("Dictionary with wrong method type", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  // Wrong: returns Int instead of String
  const wrongImpl = lam_term("x", intType, con_term("42", intType));

  const wrongDict = dict_term("Show", intType, [["show", wrongImpl]]);

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, wrongDict);
  const err = assertErr(result, "should fail");
  assert("type_mismatch" in err, "should be type mismatch error");
});

test("Trait method access from dictionary variable", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
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
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term("\"42\"", con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const context: Context = [{ trait_def: showTrait }];

  const methodAccess = trait_method_term(intShowDict, "show");

  const result = typecheck(context, methodAccess);
  const type = assertOk(result, "should typecheck");
  assert("arrow" in type, "should be function type");
});

test("Trait method access with wrong method name", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
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
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // Λ T where Show<T>. λx:T. show(dict, x)
  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(
        trait_method_term(var_term("showDict"), "show"),
        var_term("x")
      )
    )
  );

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, showValue);
  const type = assertOk(result, "should typecheck");
  assert("bounded_forall" in type, "should be bounded forall type");
  assert(type.bounded_forall.constraints.length === 1, "should have one constraint");
  assert(
    type.bounded_forall.constraints[0]?.trait === "Show",
    "should have Show constraint"
  );
});

test("Trait application with concrete type", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term("\"42\"", con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(
        trait_method_term(var_term("showDict"), "show"),
        var_term("x")
      )
    )
  );

  const context: Context = [
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
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const showValue = trait_lam_term(
    "showDict",
    "Show",
    "T",
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "x",
      var_type("T"),
      app_term(
        trait_method_term(var_term("showDict"), "show"),
        var_term("x")
      )
    )
  );

  const context: Context = [{ trait_def: showTrait }];

  const boolType = con_type("Bool");
  // No Show implementation for Bool provided
  const applied = trait_app_term(showValue, boolType, []);

  const result = typecheck(context, applied);
  const err = assertErr(result, "should fail");
  assert(
    "missing_trait_impl" in err || "wrong_number_of_dicts" in err,
    "should be missing implementation error"
  );
});

test("Multiple trait constraints", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: { star: null },
    methods: [
      ["eq", arrow_type(var_type("Self"), arrow_type(var_type("Self"), con_type("Bool")))],
    ],
  };

  // Λ T where Show<T>, Eq<T>. λx:T. λy:T. ...
  const compareAndShow = trait_lam_term(
    "showDict",
    "Show",
    "T",
    { star: null },
    [
      { trait: "Show", type: var_type("T") },
      { trait: "Eq", type: var_type("T") },
    ],
    lam_term(
      "x",
      var_type("T"),
      lam_term("y", var_type("T"), con_term("\"result\"", con_type("String")))
    )
  );

  const context: Context = [
    { trait_def: showTrait },
    { trait_def: eqTrait },
  ];

  const result = typecheck(context, compareAndShow);
  const type = assertOk(result, "should typecheck");
  assert("bounded_forall" in type, "should be bounded forall type");
  assert(type.bounded_forall.constraints.length === 2, "should have two constraints");
});

test("Functor trait with map", () => {
  const functorTrait: TraitDef = {
    name: "Functor",
    type_param: "F",
    kind: { arrow: { from: { star: null }, to: { star: null } } },
    methods: [
      [
        "map",
        forall_type(
          "A",
          { star: null },
          forall_type(
            "B",
            { star: null },
            arrow_type(
              arrow_type(var_type("A"), var_type("B")),
              arrow_type(
                app_type(var_type("F"), var_type("A")),
                app_type(var_type("F"), var_type("B"))
              )
            )
          )
        ),
      ],
    ],
  };

  const context: Context = [{ trait_def: functorTrait }];

  // Just verify the trait definition typechecks
  const binding = context.find((b) => "trait_def" in b);
  assert(binding !== undefined, "trait should be in context");
});

test("Ord trait with superclass", () => {
  const eqTrait: TraitDef = {
    name: "Eq",
    type_param: "Self",
    kind: { star: null },
    methods: [
      ["eq", arrow_type(var_type("Self"), arrow_type(var_type("Self"), con_type("Bool")))],
    ],
  };

  const ordTrait: TraitDef = {
    name: "Ord",
    type_param: "Self",
    kind: { star: null },
    methods: [
      ["compare", arrow_type(var_type("Self"), arrow_type(var_type("Self"), con_type("Int")))],
    ],
  };

  const intType = con_type("Int");

  const eqImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("true", con_type("Bool")))
  );

  const ordImpl = lam_term(
    "x",
    intType,
    lam_term("y", intType, con_term("0", intType))
  );

  const eqDict = dict_term("Eq", intType, [["eq", eqImpl]]);
  const ordDict = dict_term("Ord", intType, [["compare", ordImpl]]);

  const context: Context = [
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
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  // Λ T where Show<T>. λxs: List<T>. ...
  const showList = trait_lam_term(
    "showDict",
    "Show",
    "T",
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "xs",
      mu_type(
        "L",
        variant_type([
          ["Nil", unitType],
          ["Cons", record_type([["head", var_type("T")], ["tail", var_type("L")]])],
        ])
      ),
      con_term("\"[...]\"", con_type("String"))
    )
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
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const listInt = mu_type(
    "L",
    variant_type([
      ["Nil", unitType],
      ["Cons", record_type([["head", con_type("Int")], ["tail", var_type("L")]])],
    ])
  );

  const showListImpl = lam_term(
    "list",
    listInt,
    con_term("\"[1,2,3]\"", con_type("String"))
  );

  const listShowDict = dict_term("Show", listInt, [["show", showListImpl]]);

  const context: Context = [{ trait_def: showTrait }];

  const result = typecheck(context, listShowDict);
  assertOk(result, "should typecheck");
});

test("Monad trait structure", () => {
  const monadTrait: TraitDef = {
    name: "Monad",
    type_param: "M",
    kind: { arrow: { from: { star: null }, to: { star: null } } },
    methods: [
      [
        "return",
        forall_type(
          "A",
          { star: null },
          arrow_type(var_type("A"), app_type(var_type("M"), var_type("A")))
        ),
      ],
      [
        "bind",
        forall_type(
          "A",
          { star: null },
          forall_type(
            "B",
            { star: null },
            arrow_type(
              app_type(var_type("M"), var_type("A")),
              arrow_type(
                arrow_type(var_type("A"), app_type(var_type("M"), var_type("B"))),
                app_type(var_type("M"), var_type("B"))
              )
            )
          )
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
    { star: null },
    [constraint],
    arrow_type(var_type("T"), con_type("String"))
  );

  const type2 = bounded_forall_type(
    "T",
    { star: null },
    [constraint],
    arrow_type(var_type("T"), con_type("String"))
  );

  assert(typesEqual(type1, type2), "identical bounded foralls should be equal");
});

test("Bounded forall with different constraints not equal", () => {
  const type1 = bounded_forall_type(
    "T",
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String"))
  );

  const type2 = bounded_forall_type(
    "T",
    { star: null },
    [{ trait: "Eq", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String"))
  );

  assert(!typesEqual(type1, type2), "different constraints should not be equal");
});

test("Trait substitution in constraints", () => {
  const showTrait: TraitDef = {
    name: "Show",
    type_param: "Self",
    kind: { star: null },
    methods: [["show", arrow_type(var_type("Self"), con_type("String"))]],
  };

  const intType = con_type("Int");
  const showImpl = lam_term("x", intType, con_term("\"42\"", con_type("String")));
  const intShowDict = dict_term("Show", intType, [["show", showImpl]]);

  // Λ T where Show<T>. λf: T -> T. λx: T. x
  const polyFunc = trait_lam_term(
    "showDict",
    "Show",
    "T",
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    lam_term(
      "f",
      arrow_type(var_type("T"), var_type("T")),
      lam_term("x", var_type("T"), var_term("x"))
    )
  );

  const context: Context = [
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
    { star: null },
    [{ trait: "Show", type: var_type("T") }],
    arrow_type(var_type("T"), con_type("String"))
  );

  const result = checkKind([], polyType);
  const kind = assertOk(result, "should have valid kind");
  assert("star" in kind, "bounded forall should have kind *");
});

test("Kind mismatch in trait constraint type", () => {
  // Constraint type must have kind *, not * -> *
  const badType = bounded_forall_type(
    "F",
    { arrow: { from: { star: null }, to: { star: null } } },
    [{ trait: "Show", type: var_type("F") }], // F has kind * -> *, but Show expects *
    var_type("F")
  );

  const result = checkKind([], badType);
  const err = assertErr(result, "should fail");
  assert("kind_mismatch" in err, "should be kind mismatch error");
});
