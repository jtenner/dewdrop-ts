// utility function
const first = <T, U>(tuple: [T, U]) => tuple[0];

// Kinds (types of types)
export type Kind =
  | { star: null } // * (base kind for proper types)
  | { arrow: { from: Kind; to: Kind } }; // κ₁ → κ₂

// Types
export type Type =
  | { var: string } // type variable α
  | { arrow: { from: Type; to: Type } } // τ₁ → τ₂
  | { forall: { var: string; kind: Kind; body: Type } } // ∀α::κ.τ
  | { app: { func: Type; arg: Type } } // type application F τ
  | { lam: { var: string; kind: Kind; body: Type } } // λα::κ.τ
  | { con: string } // type constant (Int, Bool, etc.)
  | { record: { fields: [string, Type][] } } // {l₁:τ₁, l₂:τ₂, ...}
  | { variant: { cases: [string, Type][] } }; // <l₁:τ₁ | l₂:τ₂ | ...>

// Terms
export type Term =
  | { var: string } // variable x
  | { lam: { arg: string; type: Type; body: Term } } // λx:τ.e
  | { app: { callee: Term; arg: Term } } // e₁ e₂
  | { tylam: { var: string; kind: Kind; body: Term } } // Λα::κ.e
  | { tyapp: { term: Term; type: Type } } // e [τ]
  | { con: { name: string; type: Type } } // constants with their types
  | { record: { fields: [string, Term][] } } // {l₁=e₁, l₂=e₂, ...}
  | { project: { record: Term; label: string } } // e.l
  | { inject: { label: string; value: Term; variantType: Type } } // <l=e> as T
  | {
      match: {
        scrutinee: Term;
        cases: [string, { binder: string; body: Term }][];
      };
    }; // match e { l₁(x₁) => e₁ | l₂(x₂) => e₂ }

// Context entries for type checking
export type Binding =
  | { term: { name: string; type: Type } } // x : τ
  | { type: { name: string; kind: Kind } }; // α :: κ

export type Context = Binding[];

export type TypeError =
  | { unbound: string }
  | { kind_mismatch: { expected: Kind; actual: Kind } }
  | { type_mismatch: { expected: Type; actual: Type } }
  | { not_a_function: Type }
  | { not_a_type_function: Type }
  | { cyclic: string }
  | { not_a_record: Type }
  | { missing_field: { record: Type; label: string } }
  | { not_a_variant: Type }
  | { invalid_variant_label: { variant: Type; label: string } }
  | { missing_case: { label: string } }
  | { extra_case: { label: string } };

export type Result<Err, T> = { ok: T } | { err: Err };

export type Constraint =
  | { type_eq: { left: Type; right: Type } }
  | { kind_eq: { left: Kind; right: Kind } }
  | { has_kind: { ty: Type; kind: Kind; context: Context } }
  | { has_type: { term: Term; ty: Type; context: Context } };

export type Worklist = Constraint[];
export type Substitution = Map<string, Type>;

// Pretty printing
export function showKind(k: Kind): string {
  if ("star" in k) return "*";
  if ("arrow" in k)
    return `(${showKind(k.arrow.from)} → ${showKind(k.arrow.to)})`;
  return "unknown";
}

export function showType(t: Type): string {
  if ("var" in t) return t.var;
  if ("arrow" in t)
    return `(${showType(t.arrow.from)} → ${showType(t.arrow.to)})`;
  if ("forall" in t)
    return `∀${t.forall.var}::${showKind(t.forall.kind)}.${showType(t.forall.body)}`;
  if ("app" in t) return `(${showType(t.app.func)} ${showType(t.app.arg)})`;
  if ("lam" in t)
    return `λ${t.lam.var}::${showKind(t.lam.kind)}.${showType(t.lam.body)}`;
  if ("con" in t) return t.con;
  if ("record" in t) {
    const fields = t.record.fields
      .map(([label, type]) => `${label}: ${showType(type)}`)
      .join(", ");
    return `{${fields}}`;
  }
  if ("variant" in t) {
    const cases = t.variant.cases
      .map(([label, type]) => `${label}: ${showType(type)}`)
      .join(" | ");
    return `<${cases}>`;
  }
  return "unknown";
}

function showTerm(t: Term): string {
  if ("var" in t) return t.var;
  if ("lam" in t)
    return `λ${t.lam.arg}:${showType(t.lam.type)}.${showTerm(t.lam.body)}`;
  if ("app" in t) return `(${showTerm(t.app.callee)} ${showTerm(t.app.arg)})`;
  if ("tylam" in t)
    return `Λ${t.tylam.var}::${showKind(t.tylam.kind)}.${showTerm(t.tylam.body)}`;
  if ("tyapp" in t)
    return `${showTerm(t.tyapp.term)} [${showType(t.tyapp.type)}]`;
  if ("con" in t) return t.con.name;
  if ("record" in t) {
    const fields = t.record.fields
      .map(([label, term]) => `${label} = ${showTerm(term)}`)
      .join(", ");
    return `{${fields}}`;
  }
  if ("project" in t) return `${showTerm(t.project.record)}.${t.project.label}`;
  if ("inject" in t)
    return `<${t.inject.label}=${showTerm(t.inject.value)}> as ${showType(t.inject.variantType)}`;
  if ("match" in t) {
    const cases = t.match.cases
      .map(
        ([label, { binder, body }]) =>
          `${label}(${binder}) => ${showTerm(body)}`,
      )
      .join(" | ");
    return `match ${showTerm(t.match.scrutinee)} { ${cases} }`;
  }
  return "unknown";
}

export function substituteType(
  target: string,
  replacement: Type,
  inType: Type,
): Type {
  if ("var" in inType) return inType.var === target ? replacement : inType;
  if ("arrow" in inType)
    return {
      arrow: {
        from: substituteType(target, replacement, inType.arrow.from),
        to: substituteType(target, replacement, inType.arrow.to),
      },
    };
  if ("forall" in inType && inType.forall.var !== target)
    return {
      forall: {
        var: inType.forall.var,
        kind: inType.forall.kind,
        body: substituteType(target, replacement, inType.forall.body),
      },
    };
  if ("app" in inType)
    return {
      app: {
        func: substituteType(target, replacement, inType.app.func),
        arg: substituteType(target, replacement, inType.app.arg),
      },
    };
  if ("lam" in inType)
    return {
      lam: {
        body: substituteType(target, replacement, inType.lam.body),
        kind: inType.lam.kind,
        var: inType.lam.var,
      },
    };
  if ("record" in inType) {
    const fields: [string, Type][] = [];
    for (const [label, fieldType] of inType.record.fields) {
      fields.push([label, substituteType(target, replacement, fieldType)]);
    }
    return { record: { fields } };
  }
  if ("variant" in inType) {
    const cases: [string, Type][] = [];
    for (const [label, caseType] of inType.variant.cases) {
      cases.push([label, substituteType(target, replacement, caseType)]);
    }
    return { variant: { cases } };
  }
  return inType;
}

export function isStarKind(kind: Kind): boolean {
  return "star" in kind;
}

export function kindsEqual(left: Kind, right: Kind): boolean {
  if ("star" in left && "star" in right) return true;

  if ("arrow" in left && "arrow" in right) {
    return (
      kindsEqual(left.arrow.from, right.arrow.from) &&
      kindsEqual(left.arrow.to, right.arrow.to)
    );
  }

  return false;
}

// Kinding judgment: Γ ⊢ τ :: κ
export function checkKind(
  context: Context,
  type: Type,
): Result<TypeError, Kind> {
  if ("var" in type) {
    const binding = context.find(
      (b) => "type" in b && b.type.name === type.var,
    );
    if (!binding || !("type" in binding)) {
      return { err: { unbound: type.var } };
    }
    return { ok: binding.type.kind };
  }

  if ("con" in type) {
    // Base types have kind *
    return { ok: { star: null } };
  }

  if ("arrow" in type) {
    const fromKind = checkKind(context, type.arrow.from);
    if ("err" in fromKind) return fromKind;

    const toKind = checkKind(context, type.arrow.to);
    if ("err" in toKind) return toKind;

    // Both operands must have kind *
    if (!isStarKind(fromKind.ok) || !isStarKind(toKind.ok)) {
      return {
        err: {
          kind_mismatch: { expected: { star: null }, actual: fromKind.ok },
        },
      };
    }

    return { ok: { star: null } };
  }

  if ("forall" in type) {
    const extendedContext: Context = [
      ...context,
      { type: { name: type.forall.var, kind: type.forall.kind } },
    ];

    const bodyKind = checkKind(extendedContext, type.forall.body);
    if ("err" in bodyKind) return bodyKind;

    if (!isStarKind(bodyKind.ok)) {
      return {
        err: {
          kind_mismatch: { expected: { star: null }, actual: bodyKind.ok },
        },
      };
    }

    return { ok: { star: null } };
  }

  if ("lam" in type) {
    const extendedContext: Context = [
      ...context,
      { type: { name: type.lam.var, kind: type.lam.kind } },
    ];

    const bodyKind = checkKind(extendedContext, type.lam.body);
    if ("err" in bodyKind) return bodyKind;

    return {
      ok: { arrow: { from: type.lam.kind, to: bodyKind.ok } },
    };
  }

  if ("app" in type) {
    const funcKind = checkKind(context, type.app.func);
    if ("err" in funcKind) return funcKind;

    const argKind = checkKind(context, type.app.arg);
    if ("err" in argKind) return argKind;

    if (!("arrow" in funcKind.ok)) {
      return {
        err: { not_a_type_function: type.app.func },
      };
    }

    if (!kindsEqual(funcKind.ok.arrow.from, argKind.ok)) {
      return {
        err: {
          kind_mismatch: {
            expected: funcKind.ok.arrow.from,
            actual: argKind.ok,
          },
        },
      };
    }

    return { ok: funcKind.ok.arrow.to };
  }

  if ("record" in type) {
    // All field types must have kind *
    for (const [_, fieldType] of type.record.fields) {
      const fieldKind = checkKind(context, fieldType);
      if ("err" in fieldKind) return fieldKind;

      if (!isStarKind(fieldKind.ok)) {
        return {
          err: {
            kind_mismatch: { expected: { star: null }, actual: fieldKind.ok },
          },
        };
      }
    }

    return { ok: { star: null } };
  }

  if ("variant" in type) {
    // All case types must have kind *
    for (const [_, caseType] of type.variant.cases) {
      const caseKind = checkKind(context, caseType);
      if ("err" in caseKind) return caseKind;

      if (!isStarKind(caseKind.ok)) {
        return {
          err: {
            kind_mismatch: { expected: { star: null }, actual: caseKind.ok },
          },
        };
      }
    }

    return { ok: { star: null } };
  }

  throw new Error(`Unknown type: ${Object.keys(type)[0]}`);
}

export function typesEqual(left: Type, right: Type): boolean {
  if ("var" in left && "var" in right && left.var === right.var) return true;

  if ("con" in left && "con" in right && left.con === right.con) return true;

  if (
    "arrow" in left &&
    "arrow" in right &&
    typesEqual(left.arrow.from, right.arrow.from) &&
    typesEqual(left.arrow.to, right.arrow.to)
  )
    return true;

  if ("forall" in left && "forall" in right) {
    const r = right as { forall: { var: string; kind: Kind; body: Type } };
    // Kind must match
    if (!kindsEqual(left.forall.kind, r.forall.kind)) return false;

    // Since bound variables are alpha‑equivalent, rename `r`’s var
    const renamedBody = alphaRename(
      r.forall.var,
      left.forall.var,
      r.forall.body,
    );
    return typesEqual(left.forall.body, renamedBody);
  }

  if ("lam" in left && "lam" in right) {
    const r = right as { lam: { var: string; kind: Kind; body: Type } };
    if (!kindsEqual(left.lam.kind, r.lam.kind)) return false;

    // Alpha‑equivalence: rename RHS variable
    const renamedBody = alphaRename(r.lam.var, left.lam.var, r.lam.body);
    return typesEqual(left.lam.body, renamedBody);
  }

  if ("app" in left && "app" in right) {
    const r = right as { app: { func: Type; arg: Type } };
    return (
      typesEqual(left.app.func, r.app.func) &&
      typesEqual(left.app.arg, r.app.arg)
    );
  }

  if ("record" in left && "record" in right) {
    const leftFields = left.record.fields;
    const rightFields = right.record.fields;

    const leftLabels = leftFields.map(first).sort();
    const rightLabels = rightFields.map(first).sort();

    // Must have same labels
    if (leftLabels.length !== rightLabels.length) return false;
    if (!leftLabels.every((l, i) => l === rightLabels[i])) return false;

    // All field types must be equal
    return leftLabels.every((label) =>
      typesEqual(
        leftFields.find((t) => t[0] === label)![1],
        rightFields.find((t) => t[0] === label)![1],
      ),
    );
  }

  if ("variant" in left && "variant" in right) {
    const leftCases = left.variant.cases;
    const rightCases = right.variant.cases;

    const leftLabels = leftCases.map(first).sort();
    const rightLabels = rightCases.map(first).sort();

    // Must have same labels
    if (leftLabels.length !== rightLabels.length) return false;
    if (!leftLabels.every((l, i) => l === rightLabels[i])) return false;

    // All case types must be equal
    return leftLabels.every((label) =>
      typesEqual(
        leftCases.find((t) => t[0] === label)![1],
        rightCases.find((t) => t[0] === label)![1],
      ),
    );
  }

  return false;
}

export function alphaRename(from: string, to: string, type: Type): Type {
  if (from === to) return type; // no need to rename

  if ("var" in type) return type.var === from ? { var: to } : type;

  if ("arrow" in type)
    return {
      arrow: {
        from: alphaRename(from, to, type.arrow.from),
        to: alphaRename(from, to, type.arrow.to),
      },
    };

  if ("forall" in type) {
    if (type.forall.var === from) return type; // shadowed, stop
    return {
      forall: {
        var: type.forall.var,
        kind: type.forall.kind,
        body: alphaRename(from, to, type.forall.body),
      },
    };
  }

  if ("lam" in type) {
    if (type.lam.var === from) return type; // shadowed
    return {
      lam: {
        var: type.lam.var,
        kind: type.lam.kind,
        body: alphaRename(from, to, type.lam.body),
      },
    };
  }

  if ("app" in type) {
    return {
      app: {
        func: alphaRename(from, to, type.app.func),
        arg: alphaRename(from, to, type.app.arg),
      },
    };
  }

  if ("record" in type) {
    const fields: [string, Type][] = [];
    for (const [label, fieldType] of type.record.fields) {
      fields.push([label, alphaRename(from, to, fieldType)]);
    }
    return { record: { fields } };
  }

  if ("variant" in type) {
    const cases: [string, Type][] = [];
    for (const [label, caseType] of type.variant.cases) {
      cases.push([label, alphaRename(from, to, caseType)]);
    }
    return { variant: { cases } };
  }

  return type;
}

export function unifyTypes(
  left: Type,
  right: Type,
  worklist: Worklist,
  subst: Substitution,
): Result<TypeError, null> {
  if (typesEqual(left, right)) {
    return { ok: null };
  }

  // Variable cases
  if ("var" in left) {
    return unifyVariable(left.var, right, subst);
  }

  if ("var" in right) {
    return unifyVariable(right.var, left, subst);
  }

  // Structural cases
  if ("arrow" in left && "arrow" in right) {
    worklist.push({
      type_eq: { left: left.arrow.from, right: right.arrow.from },
    });
    worklist.push({ type_eq: { left: left.arrow.to, right: right.arrow.to } });
    return { ok: null };
  }

  if ("forall" in left && "forall" in right) {
    worklist.push({
      kind_eq: { left: left.forall.kind, right: right.forall.kind },
    });

    // Alpha-rename if necessary and unify bodies
    const renamedRight = alphaRename(
      right.forall.var,
      left.forall.var,
      right.forall.body,
    );
    worklist.push({ type_eq: { left: left.forall.body, right: renamedRight } });

    return { ok: null };
  }

  if ("app" in left && "app" in right) {
    worklist.push({ type_eq: { left: left.app.func, right: right.app.func } });
    worklist.push({ type_eq: { left: left.app.arg, right: right.app.arg } });
    return { ok: null };
  }

  if ("lam" in left && "lam" in right) {
    worklist.push({ kind_eq: { left: left.lam.kind, right: right.lam.kind } });

    const renamedRight = alphaRename(
      right.lam.var,
      left.lam.var,
      right.lam.body,
    );
    worklist.push({ type_eq: { left: left.lam.body, right: renamedRight } });

    return { ok: null };
  }

  if ("record" in left && "record" in right) {
    const leftFields = left.record.fields;
    const rightFields = right.record.fields;

    const leftLabels = leftFields.map(first).sort();
    const rightLabels = rightFields.map(first).sort();

    // Must have same labels
    if (leftLabels.length !== rightLabels.length) {
      return {
        err: { type_mismatch: { expected: left, actual: right } },
      };
    }

    for (let i = 0; i < leftLabels.length; i++) {
      if (leftLabels[i] !== rightLabels[i]) {
        return {
          err: { type_mismatch: { expected: left, actual: right } },
        };
      }
    }

    // Unify all field types
    for (const label of leftLabels) {
      worklist.push({
        type_eq: {
          left: leftFields.find((t) => t[0] === label)![1],
          right: rightFields.find((t) => t[0] === label)![1],
        },
      });
    }

    return { ok: null };
  }

  if ("variant" in left && "variant" in right) {
    const leftCases = left.variant.cases;
    const rightCases = right.variant.cases;

    const leftLabels = leftCases.map(first).sort();
    const rightLabels = rightCases.map(first).sort();

    // Must have same labels
    if (leftLabels.length !== rightLabels.length) {
      return {
        err: { type_mismatch: { expected: left, actual: right } },
      };
    }

    for (let i = 0; i < leftLabels.length; i++) {
      if (leftLabels[i] !== rightLabels[i]) {
        return {
          err: { type_mismatch: { expected: left, actual: right } },
        };
      }
    }

    // Unify all case types
    for (const label of leftLabels) {
      worklist.push({
        type_eq: {
          left: leftCases.find((t) => t[0] === label)![1],
          right: rightCases.find((t) => t[0] === label)![1],
        },
      });
    }

    return { ok: null };
  }

  return {
    err: { type_mismatch: { expected: left, actual: right } },
  };
}

export function unifyVariable(
  varName: string,
  type: Type,
  subst: Substitution,
): Result<TypeError, null> {
  if (subst.has(varName)) {
    const existing = subst.get(varName)!;
    if (!typesEqual(existing, type)) {
      return {
        err: { type_mismatch: { expected: existing, actual: type } },
      };
    }
    return { ok: null };
  }

  // Occurs check
  if (occursCheck(varName, type)) {
    return {
      err: { cyclic: varName },
    };
  }

  subst.set(varName, type);
  return { ok: null };
}

export function unifyKinds(left: Kind, right: Kind): Result<TypeError, null> {
  if (kindsEqual(left, right)) {
    return { ok: null };
  }

  return {
    err: { kind_mismatch: { expected: left, actual: right } },
  };
}

export function occursCheck(varName: string, type: Type): boolean {
  if ("var" in type) return type.var === varName;

  if ("con" in type) return false;

  if ("arrow" in type)
    return (
      occursCheck(varName, type.arrow.from) ||
      occursCheck(varName, type.arrow.to)
    );

  if ("forall" in type) {
    if (type.forall.var === varName) return false;
    return occursCheck(varName, type.forall.body);
  }

  if ("lam" in type) {
    if (type.lam.var === varName) return false;
    return occursCheck(varName, type.lam.body);
  }

  if ("app" in type) {
    return (
      occursCheck(varName, type.app.func) || occursCheck(varName, type.app.arg)
    );
  }

  if ("record" in type) {
    return type.record.fields.some((fieldType) =>
      occursCheck(varName, fieldType[1]),
    );
  }

  if ("variant" in type) {
    return type.variant.cases.some((caseType) =>
      occursCheck(varName, caseType[1]),
    );
  }

  return false;
}

export function applySubstitution(subst: Substitution, type: Type): Type {
  if ("var" in type) {
    const replacement = subst.get(type.var);
    return replacement ? replacement : type;
  }

  if ("con" in type) return type;

  if ("arrow" in type)
    return {
      arrow: {
        from: applySubstitution(subst, type.arrow.from),
        to: applySubstitution(subst, type.arrow.to),
      },
    };

  if ("forall" in type) {
    const newSubst = new Map(subst);
    newSubst.delete(type.forall.var);
    return {
      forall: {
        var: type.forall.var,
        kind: type.forall.kind,
        body: applySubstitution(newSubst, type.forall.body),
      },
    };
  }

  if ("lam" in type) {
    const newSubst = new Map(subst);
    newSubst.delete(type.lam.var);
    return {
      lam: {
        var: type.lam.var,
        kind: type.lam.kind,
        body: applySubstitution(newSubst, type.lam.body),
      },
    };
  }

  if ("app" in type) {
    return {
      app: {
        func: applySubstitution(subst, type.app.func),
        arg: applySubstitution(subst, type.app.arg),
      },
    };
  }

  if ("record" in type) {
    const fields: [string, Type][] = [];
    for (const [label, fieldType] of type.record.fields) {
      fields.push([label, applySubstitution(subst, fieldType)]);
    }
    return { record: { fields } };
  }

  if ("variant" in type) {
    const cases: [string, Type][] = [];
    for (const [label, caseType] of type.variant.cases) {
      cases.push([label, applySubstitution(subst, caseType)]);
    }
    return { variant: { cases } };
  }

  return type;
}

// Typing judgment: Γ ⊢ e : τ
export function inferType(
  context: Context,
  term: Term,
): Result<TypeError, Type> {
  if ("var" in term) {
    const binding = context.find(
      (b) => "term" in b && b.term.name === term.var,
    );
    if (!binding || !("term" in binding)) {
      return { err: { unbound: term.var } };
    }
    return { ok: binding.term.type };
  }

  if ("con" in term) {
    return { ok: term.con.type };
  }

  if ("lam" in term) {
    // Check that the argument type is well-kinded
    const argKind = checkKind(context, term.lam.type);
    if ("err" in argKind) return argKind;

    if (!isStarKind(argKind.ok)) {
      return {
        err: {
          kind_mismatch: { expected: { star: null }, actual: argKind.ok },
        },
      };
    }

    const extendedContext: Context = [
      ...context,
      { term: { name: term.lam.arg, type: term.lam.type } },
    ];

    const bodyType = inferType(extendedContext, term.lam.body);
    if ("err" in bodyType) return bodyType;

    return {
      ok: { arrow: { from: term.lam.type, to: bodyType.ok } },
    };
  }

  if ("app" in term) {
    const calleeType = inferType(context, term.app.callee);
    if ("err" in calleeType) return calleeType;

    const argType = inferType(context, term.app.arg);
    if ("err" in argType) return argType;

    if (!("arrow" in calleeType.ok)) {
      return {
        err: { not_a_function: calleeType.ok },
      };
    }

    if (!typesEqual(calleeType.ok.arrow.from, argType.ok)) {
      return {
        err: {
          type_mismatch: {
            expected: calleeType.ok.arrow.from,
            actual: argType.ok,
          },
        },
      };
    }

    return { ok: calleeType.ok.arrow.to };
  }

  if ("tylam" in term) {
    const extendedContext: Context = [
      ...context,
      { type: { name: term.tylam.var, kind: term.tylam.kind } },
    ];

    const bodyType = inferType(extendedContext, term.tylam.body);
    if ("err" in bodyType) return bodyType;

    return {
      ok: {
        forall: {
          var: term.tylam.var,
          kind: term.tylam.kind,
          body: bodyType.ok,
        },
      },
    };
  }

  if ("tyapp" in term) {
    const termType = inferType(context, term.tyapp.term);
    if ("err" in termType) return termType;

    if (!("forall" in termType.ok)) {
      return {
        err: {
          type_mismatch: {
            expected: termType.ok,
            actual: term.tyapp.type,
          },
        },
      };
    }

    // Check that the type argument has the expected kind
    const argKind = checkKind(context, term.tyapp.type);
    if ("err" in argKind) return argKind;

    if (!kindsEqual(termType.ok.forall.kind, argKind.ok)) {
      return {
        err: {
          kind_mismatch: {
            expected: termType.ok.forall.kind,
            actual: argKind.ok,
          },
        },
      };
    }

    // Substitute the type argument in the body
    const substituted = substituteType(
      termType.ok.forall.var,
      term.tyapp.type,
      termType.ok.forall.body,
    );

    return { ok: substituted };
  }

  if ("record" in term) {
    const fields: [string, Type][] = [];

    for (const [label, fieldTerm] of term.record.fields) {
      const fieldType = inferType(context, fieldTerm);
      if ("err" in fieldType) return fieldType;

      fields.push([label, fieldType.ok]);
    }

    return { ok: { record: { fields } } };
  }

  if ("project" in term) {
    const recordType = inferType(context, term.project.record);
    if ("err" in recordType) return recordType;

    if (!("record" in recordType.ok)) {
      return {
        err: { not_a_record: recordType.ok },
      };
    }

    const fieldType = recordType.ok.record.fields.find(
      (t) => t[0] === term.project.label,
    );
    if (!fieldType) {
      return {
        err: {
          missing_field: {
            record: recordType.ok,
            label: term.project.label,
          },
        },
      };
    }

    return { ok: fieldType[1] };
  }

  if ("inject" in term) {
    // Check that the variant type is well-formed
    const variantKind = checkKind(context, term.inject.variantType);
    if ("err" in variantKind) return variantKind;

    if (!("variant" in term.inject.variantType)) {
      return {
        err: { not_a_variant: term.inject.variantType },
      };
    }

    // Check that the label exists in the variant type
    const expectedType = term.inject.variantType.variant.cases.find(
      (t) => t[0] === term.inject.label,
    );
    if (!expectedType) {
      return {
        err: {
          invalid_variant_label: {
            variant: term.inject.variantType,
            label: term.inject.label,
          },
        },
      };
    }

    // Check that the value has the expected type
    const valueType = inferType(context, term.inject.value);
    if ("err" in valueType) return valueType;

    if (!typesEqual(expectedType[1], valueType.ok)) {
      return {
        err: {
          type_mismatch: {
            expected: expectedType[1],
            actual: valueType.ok,
          },
        },
      };
    }

    return { ok: term.inject.variantType };
  }

  if ("match" in term) {
    // Infer the type of the scrutinee
    const scrutineeType = inferType(context, term.match.scrutinee);
    if ("err" in scrutineeType) return scrutineeType;

    if (!("variant" in scrutineeType.ok)) {
      return {
        err: { not_a_variant: scrutineeType.ok },
      };
    }

    const variantCases = scrutineeType.ok.variant.cases;
    const matchCases = term.match.cases;

    // Check that all variant cases are covered
    const variantLabels = Object.keys(variantCases).sort();
    const matchLabels = Object.keys(matchCases).sort();

    for (const label of variantLabels) {
      if (!(label in matchCases)) {
        return {
          err: { missing_case: { label } },
        };
      }
    }

    // Check for extra cases
    for (const label of matchLabels) {
      if (!(label in variantCases)) {
        return {
          err: { extra_case: { label } },
        };
      }
    }

    // Type check each case and ensure they all return the same type
    let resultType: Type | null = null;

    for (const [label, { binder, body }] of matchCases) {
      const caseType = variantCases.find((t) => t[0] === label)![1];

      const extendedContext: Context = [
        ...context,
        { term: { name: binder, type: caseType } },
      ];

      const bodyType = inferType(extendedContext, body);
      if ("err" in bodyType) return bodyType;

      if (resultType === null) {
        resultType = bodyType.ok;
      } else if (!typesEqual(resultType, bodyType.ok)) {
        return {
          err: {
            type_mismatch: {
              expected: resultType,
              actual: bodyType.ok,
            },
          },
        };
      }
    }

    return { ok: resultType! };
  }

  throw new Error(`Unknown term: ${Object.keys(term)[0]}`);
}

// Worklist constraint solver
export function solveConstraints(
  worklist: Worklist,
  subst: Substitution = new Map(),
): Result<TypeError, Substitution> {
  while (worklist.length > 0) {
    const constraint = worklist.shift()!;

    const result = processConstraint(constraint, worklist, subst);
    if ("err" in result) return result;
  }

  return { ok: subst };
}

export function processConstraint(
  constraint: Constraint,
  worklist: Worklist,
  subst: Substitution,
): Result<TypeError, null> {
  if ("type_eq" in constraint) {
    const left = applySubstitution(subst, constraint.type_eq.left);
    const right = applySubstitution(subst, constraint.type_eq.right);

    return unifyTypes(left, right, worklist, subst);
  }

  if ("kind_eq" in constraint) {
    return unifyKinds(constraint.kind_eq.left, constraint.kind_eq.right);
  }

  if ("has_kind" in constraint) {
    const type = applySubstitution(subst, constraint.has_kind.ty);
    const kindResult = checkKind(constraint.has_kind.context, type);

    if ("err" in kindResult) {
      return kindResult;
    }

    worklist.push({
      kind_eq: { left: kindResult.ok, right: constraint.has_kind.kind },
    });

    return { ok: null };
  }

  if ("has_type" in constraint) {
    const typeResult = inferType(
      constraint.has_type.context,
      constraint.has_type.term,
    );

    if ("err" in typeResult) {
      return typeResult;
    }

    worklist.push({
      type_eq: { left: typeResult.ok, right: constraint.has_type.ty },
    });

    return { ok: null };
  }

  throw new Error("Unknown constraint kind");
}

// Top-level type checking function
export function typecheck(
  context: Context,
  term: Term,
): Result<TypeError, Type> {
  return inferType(context, term);
}

// Type checking with constraint solving (for more complex scenarios)
export function typecheckWithConstraints(
  context: Context,
  term: Term,
): Result<TypeError, Type> {
  const worklist: Worklist = [
    { has_type: { term, ty: { var: "$result" }, context } },
  ];

  const subst = new Map<string, Type>();
  const result = solveConstraints(worklist, subst);

  if ("err" in result) return result;

  const resultType = subst.get("$result");
  if (!resultType) {
    return inferType(context, term);
  }

  return { ok: resultType };
}

// export type Type =
//   | { var: string } // type variable α
//   | { arrow: { from: Type; to: Type } } // τ₁ → τ₂
//   | { forall: { var: string; kind: Kind; body: Type } } // ∀α::κ.τ
//   | { app: { func: Type; arg: Type } } // type application F τ
//   | { lam: { var: string; kind: Kind; body: Type } } // λα::κ.τ
//   | { con: string }; // type constant (Int, Bool, etc.)
//   | { record: { fields: [string, Type][] } } // {l₁:τ₁, l₂:τ₂, ...}
//   | { variant: { cases: [string, Type][] } }; // <l₁:τ₁ | l₂:τ₂ | ...>
export const var_type = (name: string) => ({ var: name });
export const arrow_type = (from: Type, to: Type) => ({ arrow: { from, to } });
export const forall_type = (name: string, kind: Kind, body: Type) => ({
  forall: { var: name, kind, body },
});
export const app_type = (func: Type, arg: Type) => ({ app: { func, arg } });
export const lam_type = (name: string, kind: Kind, body: Type) => ({
  lam: { var: name, kind, body },
});
export const con_type = (con: string) => ({ con });
export const record_type = (fields: [string, Type][]) => ({
  records: { fields },
});
export const variant_type = (cases: [string, Type][]) => ({
  variant: { cases },
});

// // Terms
// export type Term =
//   | { var: string } // variable x
//   | { lam: { arg: string; type: Type; body: Term } } // λx:τ.e
//   | { app: { callee: Term; arg: Term } } // e₁ e₂
//   | { tylam: { var: string; kind: Kind; body: Term } } // Λα::κ.e
//   | { tyapp: { term: Term; type: Type } } // e [τ]
//   | { con: { name: string; type: Type } } // constants with their types
//   | { record: { fields: [string, Term][] } } // {l₁=e₁, l₂=e₂, ...}
//   | { project: { record: Term; label: string } } // e.l
//   | { inject: { label: string; value: Term; variantType: Type } } // <l=e> as T
//   | {
//       match: {
//         scrutinee: Term;
//         cases: [string, { binder: string; body: Term }][];
//       };
//     }; // match e { l₁(x₁) => e₁ | l₂(x₂) => e₂ }
export const var_term = (name: string) => ({ var: name });
export const lam_term = (arg: string, type: Type, body: Term) => ({
  lam: { arg, type, body },
});
export const app_term = (callee: Term, arg: Term) => ({ app: { callee, arg } });
export const tylam_term = (name: string, kind: Kind, body: Term) => ({
  tylam: { var: name, kind, body },
});
export const tyapp_term = (term: Term, type: Type) => ({
  tyapp: { term, type },
});
export const con_term = (name: string, type: Type) => ({ con: { name, type } });
export const record_term = (fields: [string, Term][]) => ({
  record: { fields },
});
export const project_term = (record: Term, label: string) => ({
  project: { record, label },
});
export const inject_term = (label: string, value: Term, variantType: Type) => ({
  inject: { label, value, variantType },
});
export const match_term = (
  scrutinee: Term,
  cases: [string, { binder: string; body: Term }][],
) => ({
  match: { scrutinee, cases },
});

export const unitType: Type = { con: "Unit" };
export const unitValue: Term = { con: { name: "()", type: unitType } };
