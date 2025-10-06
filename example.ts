import { Context, Kind, Type, Term, typecheck, showType } from "./src/types_system_f_omega";


// Helper to create contexts easily
function mkContext(bindings: Array<[string, Type] | [string, Kind]>): Context {
  return bindings.map(([name, typeOrKind]) => {
    if ("star" in typeOrKind || "arrow" in typeOrKind) {
      return { type: { name, kind: typeOrKind as Kind } };
    }
    return { term: { name, type: typeOrKind as Type } };
  });
}

const intType: Type = { con: "Int" };

// λx:Int. x
const identityInt: Term = {
  lam: {
    arg: "x",
    type: intType,
    body: { var: "x" },
  },
};

// Example with context
const ctx = mkContext([
  ["x", intType],
  ["Bool", { star: null }],
]);

const xVar: Term = { var: "x" };
const xType = typecheck(ctx, identityInt);
if ("ok" in xType) {
  console.log("x has type:", showType(xType.ok)); // Int
}
