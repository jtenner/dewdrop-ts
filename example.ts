import { Context, Kind, Type, Term, typecheck, showType, unitType, unitValue } from "./src/types_system_f_omega";

const boolType: Type = {
  variant: {
    cases: [
      ["True", unitType],
      ["False", unitType],
    ],
  },
};

const trueValue: Term = {
  inject: {
    label: "True",
    value: unitValue,
    variantType: boolType,
  },
};

const result = typecheck([], trueValue);
if ("ok" in result) {
  console.log("Type:", showType(result.ok)); // <False: Unit | True: Unit>
}

// Example 2: Option type (like Rust's Option<T>)
const optionType = (t: Type): Type => ({
  variant: {
    cases: [
      ["None", unitType],
      ["Some", t],
    ],
  },
});

const someInt: Term = {
  inject: {
    label: "Some",
    value: { con: { name: "42", type: { con: "Int" } } },
    variantType: optionType({ con: "Int" }),
  },
};

const optResult = typecheck([], someInt);
if ("ok" in optResult) {
  console.log("Type:", showType(optResult.ok)); // <None: Unit | Some: Int>
}

// Example 3: Pattern matching
const isNone: Term = {
  lam: {
    arg: "opt",
    type: optionType({ con: "Int" }),
    body: {
      match: {
        scrutinee: { var: "opt" },
        cases: [
          ["None", {
            binder: "_",
            body: {
              inject: {
                label: "True",
                value: unitValue,
                variantType: boolType,
              },
            },
          }],
          ["Some", {
            binder: "_",
            body: {
              inject: {
                label: "False",
                value: unitValue,
                variantType: boolType,
              },
            },
          }],
        ],
      },
    },
  },
};

const isNoneType = typecheck([], isNone);
if ("ok" in isNoneType) {
  console.log("Type:", showType(isNoneType.ok));
  // <None: Unit | Some: Int> → <False: Unit | True: Unit>
}

// Example 4: Polymorphic Option type
// Λα::*.λopt:<None:Unit|Some:α>.λdefault:α.
//   match opt { None(_) => default | Some(x) => x }
const unwrapOr: Term = {
  tylam: {
    var: "α",
    kind: { star: null },
    body: {
      lam: {
        arg: "opt",
        type: optionType({ var: "α" }),
        body: {
          lam: {
            arg: "default",
            type: { var: "α" },
            body: {
              match: {
                scrutinee: { var: "opt" },
                cases: [
                  ["None", {
                    binder: "_",
                    body: { var: "default" },
                  }],
                  ["Some", {
                    binder: "x",
                    body: { var: "x" },
                  }],
                ],
              },
            },
          },
        },
      },
    },
  },
};

const unwrapOrType = typecheck([], unwrapOr);
if ("ok" in unwrapOrType) {
  console.log("Type:", showType(unwrapOrType.ok));
  // ∀α::*.<None: Unit | Some: α> → (α → α)
}

// Example 5: Result type (like Rust's Result<T, E>)
const resultType = (t: Type, e: Type): Type => ({
  variant: {
    cases: [
      ["Ok", t],
      ["Err", e],
    ],
  },
});

const okValue: Term = {
  inject: {
    label: "Ok",
    value: { con: { name: "42", type: { con: "Int" } } },
    variantType: resultType({ con: "Int" }, { con: "String" }),
  },
};

// Example 6: Binary tree
const treeType = (t: Type): Type => ({
  variant: {
    cases: [
      ["Leaf", unitType],
      ["Node", {
        record: {
          fields: [
            ["value", t],
            ["left", { var: "Tree" }],
            ["right", { var: "Tree" }],
          ]
        },
      }]
    ],
  },
});

// Example 7: Map over option
const mapOption: Term = {
  tylam: {
    var: "α",
    kind: { star: null },
    body: {
      tylam: {
        var: "β",
        kind: { star: null },
        body: {
          lam: {
            arg: "f",
            type: { arrow: { from: { var: "α" }, to: { var: "β" } } },
            body: {
              lam: {
                arg: "opt",
                type: optionType({ var: "α" }),
                body: {
                  match: {
                    scrutinee: { var: "opt" },
                    cases: [
                      ["None", {
                        binder: "_",
                        body: {
                          inject: {
                            label: "None",
                            value: unitValue,
                            variantType: optionType({ var: "β" }),
                          },
                        },
                      }],
                      ["Some", {
                        binder: "x",
                        body: {
                          inject: {
                            label: "Some",
                            value: {
                              app: {
                                callee: { var: "f" },
                                arg: { var: "x" },
                              },
                            },
                            variantType: optionType({ var: "β" }),
                          },
                        },
                      },
                    ],
                  ]
                  },
                },
              },
            },
          },
        },
      },
    },
  },
};

const mapOptionType = typecheck([], mapOption);
if ("ok" in mapOptionType) {
  console.log("Type:", showType(mapOptionType.ok));
  // ∀α::*.∀β::*.(α → β) → (<None: Unit | Some: α> → <None: Unit | Some: β>)
}