// utility function
const first = <T, U>(tuple: [T, U]) => tuple[0];

// Kinds (types of types)
export type Kind =
  | { star: null } // * (base kind for proper types)
  | { arrow: { from: Kind; to: Kind } }; // κ₁ → κ₂

// Pattern expressions for match cases
export type VarPattern = { var: string };
export type WildcardPattern = { wildcard: null };
export type ConPattern = { con: { name: string; type: Type } };
export type RecordPattern = { record: [string, Pattern][] };
export type VariantPattern = { variant: { label: string; pattern: Pattern } };
export type TuplePattern = { tuple: Pattern[] };
export type Pattern =
  | VarPattern // x - bind variable
  | WildcardPattern // _ - match anything
  | ConPattern // literal constant
  | RecordPattern // { l1: p1, l2: p2 }
  | VariantPattern // Label(pattern)
  | TuplePattern; // #(...patterns)

// Types
export type VarType = { var: string };
export type ArrowType = { arrow: { from: Type; to: Type } };
export type ForallType = { forall: { var: string; kind: Kind; body: Type } };
export type AppType = { app: { func: Type; arg: Type } };
export type LamType = { lam: { var: string; kind: Kind; body: Type } };
export type ConType = { con: string };
export type RecordType = { record: [string, Type][] };
export type VariantType = { variant: [string, Type][] };
export type MuType = { mu: { var: string; body: Type } };
export type TupleType = { tuple: Type[] };

export type Type =
  | VarType // type variable α
  | ArrowType // τ₁ → τ₂
  | ForallType // ∀α::κ.τ
  | AppType // type application F τ
  | LamType // λα::κ.τ
  | ConType // type constant (Int, Bool, etc.)
  | RecordType // {l₁:τ₁, l₂:τ₂, ...}
  | VariantType // <l₁:τ₁ | l₂:τ₂ | ...>
  | MuType // μα.τ - recursive type
  | TupleType; // tuples;
// Terms
export type VarTerm = { var: string };
export type LamTerm = { lam: { arg: string; type: Type; body: Term } };
export type AppTerm = { app: { callee: Term; arg: Term } };
export type TyLamTerm = { tylam: { var: string; kind: Kind; body: Term } };
export type TyAppTerm = { tyapp: { term: Term; type: Type } };
export type ConTerm = { con: { name: string; type: Type } };
export type RecordTerm = { record: [string, Term][] };
export type ProjectTerm = { project: { record: Term; label: string } };
export type InjectTerm = {
  inject: { label: string; value: Term; variant_type: Type };
};
export type MatchTerm = {
  match: { scrutinee: Term; cases: [Pattern, Term][] };
};
export type FoldTerm = { fold: { type: Type; term: Term } };
export type UnfoldTerm = { unfold: Term };
export type TupleTerm = { tuple: Term[] };
export type TupleProjectTerm = { tupleProject: { tuple: Term; index: number } };
export type Term =
  | VarTerm // variable x
  | LamTerm // λx:τ.e
  | AppTerm // e₁ e₂
  | TyLamTerm // Λα::κ.e
  | TyAppTerm // e [τ]
  | ConTerm // constants with their types
  | RecordTerm // {l₁=e₁, l₂=e₂, ...}
  | ProjectTerm // e.l
  | InjectTerm // <l=e> as T
  | MatchTerm // match e { l₁(x₁) => e₁ | l₂(x₂) => e₂ }
  | FoldTerm // fold: τ[μα.τ/α] → μα.τ
  | UnfoldTerm // unfold: μα.τ → τ[μα.τ/α]
  | TupleTerm // tuple: #(l₁, l₂, ...)
  | TupleProjectTerm; // tuple[0]

// Context entries for type checking
export type TermBinding = { term: { name: string; type: Type } };
export type TypeBinding = { type: { name: string; kind: Kind } };
export type Binding =
  | TermBinding // x : τ
  | TypeBinding; // α :: κ

export type Context = Binding[];

export type TypingError =
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
  | { extra_case: { label: string } }
  | { not_a_tuple: Type }
  | { tuple_index_out_of_bounds: { tuple: Type; index: number } };

export type Result<Err, T> = { ok: T } | { err: Err };

export type Constraint =
  | { type_eq: { left: Type; right: Type } }
  | { kind_eq: { left: Kind; right: Kind } }
  | { has_kind: { ty: Type; kind: Kind; context: Context } }
  | { has_type: { term: Term; ty: Type; context: Context } };

export type Worklist = Constraint[];
export type Substitution = Map<string, Type>;

// Show patterns for debugging
export function showPattern(p: Pattern): string {
  if ("var" in p) return p.var;
  if ("wildcard" in p) return "_";
  if ("con" in p) return p.con.name;
  if ("record" in p) {
    const fields = p.record
      .map(([label, pat]) => `${label}: ${showPattern(pat)}`)
      .join(", ");
    return `{${fields}}`;
  }
  if ("variant" in p) {
    return `${p.variant.label}(${showPattern(p.variant.pattern)})`;
  }
  if ("tuple" in p) {
    const elements = p.tuple.map(showPattern).join(", ");
    return `(${elements})`;
  }
  return "unknown";
}

// Extract all variable bindings from a pattern
export function patternBindings(pattern: Pattern): [string, Type][] {
  if ("var" in pattern) {
    // We'll need the type from context during type checking
    return [[pattern.var, { var: "$unknown" }]]; // placeholder
  }
  if ("wildcard" in pattern) {
    return [];
  }
  if ("con" in pattern) {
    return [];
  }
  if ("record" in pattern) {
    const bindings: [string, Type][] = [];
    for (const [_, subPattern] of pattern.record) {
      bindings.push(...patternBindings(subPattern));
    }
    return bindings;
  }
  if ("variant" in pattern) {
    return patternBindings(pattern.variant.pattern);
  }
  if ("tuple" in pattern) {
    const bindings: [string, Type][] = [];
    for (const subPattern of pattern.tuple) {
      bindings.push(...patternBindings(subPattern));
    }
    return bindings;
  }
  return [];
}

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
    const fields = t.record
      .map(([label, type]) => `${label}: ${showType(type)}`)
      .join(", ");
    return `{${fields}}`;
  }
  if ("variant" in t) {
    const cases = t.variant
      .map(([label, type]) => `${label}: ${showType(type)}`)
      .join(" | ");
    return `<${cases}>`;
  }
  if ("mu" in t) {
    return `μ${t.mu.var}.${showType(t.mu.body)}`;
  }
  if ("tuple" in t) {
    const elements = t.tuple.map(showType).join(", ");
    return `(${elements})`;
  }
  return "unknown";
}

export function showTerm(t: Term): string {
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
    const fields = t.record
      .map(([label, term]) => `${label} = ${showTerm(term)}`)
      .join(", ");
    return `{${fields}}`;
  }
  if ("project" in t) return `${showTerm(t.project.record)}.${t.project.label}`;
  if ("inject" in t)
    return `<${t.inject.label}=${showTerm(t.inject.value)}> as ${showType(t.inject.variant_type)}`;
  if ("match" in t) {
    const cases = t.match.cases
      .map(([pattern, body]) => `${showPattern(pattern)} => ${showTerm(body)}`)
      .join(" | ");
    return `match ${showTerm(t.match.scrutinee)} { ${cases} }`;
  }
  if ("fold" in t) {
    return `fold[${showType(t.fold.type)}](${showTerm(t.fold.term)})`;
  }
  if ("unfold" in t) {
    return `unfold(${showTerm(t.unfold)})`;
  }
  if ("tuple" in t) {
    const elements = t.tuple.map(showTerm).join(", ");
    return `(${elements})`;
  }
  if ("tupleProject" in t) {
    return `${showTerm(t.tupleProject.tuple)}.${t.tupleProject.index}`;
  }
  return "unknown";
}

// Check if patterns are exhaustive (simplified version)
export function checkExhaustive(
  patterns: Pattern[],
  type: Type,
): Result<TypingError, null> {
  if ("variant" in type) {
    const variantLabels = new Set(type.variant.map(first));
    const coveredLabels = new Set<string>();

    for (const pattern of patterns) {
      if ("wildcard" in pattern || "var" in pattern) {
        // Wildcard or variable covers everything
        return { ok: null };
      }
      if ("variant" in pattern) {
        coveredLabels.add(pattern.variant.label);
      }
    }

    for (const label of variantLabels) {
      if (!coveredLabels.has(label)) {
        return { err: { missing_case: { label } } };
      }
    }
  }

  return { ok: null };
}

// Check if a pattern matches a type and extract bindings
export function checkPattern(
  pattern: Pattern,
  type: Type,
  context: Context,
): Result<TypingError, Context> {
  if ("var" in pattern) {
    // Variable pattern binds the whole value
    return { ok: [{ term: { name: pattern.var, type } }] };
  }

  if ("wildcard" in pattern) {
    // Wildcard matches anything, no bindings
    return { ok: [] };
  }

  if ("con" in pattern) {
    // Constant pattern must match type exactly
    if (!typesEqual(pattern.con.type, type)) {
      return {
        err: {
          type_mismatch: { expected: type, actual: pattern.con.type },
        },
      };
    }
    return { ok: [] };
  }

  if ("record" in pattern) {
    if (!("record" in type)) {
      return { err: { not_a_record: type } };
    }

    const bindings: Context = [];
    const patternLabels = pattern.record.map(first).sort();
    const typeLabels = type.record.map(first).sort();

    // Check that pattern labels match type labels
    if (patternLabels.length !== typeLabels.length) {
      return {
        err: {
          type_mismatch: {
            expected: type,
            actual: { record: pattern.record.map(([l, _]) => [l, unitType]) },
          },
        },
      };
    }

    for (let i = 0; i < patternLabels.length; i++) {
      if (patternLabels[i] !== typeLabels[i]) {
        return {
          err: {
            missing_field: { record: type, label: patternLabels[i]! },
          },
        };
      }
    }

    // Check each field pattern
    for (const [label, subPattern] of pattern.record) {
      const fieldType = type.record.find((t) => t[0] === label);
      if (!fieldType) {
        return {
          err: { missing_field: { record: type, label } },
        };
      }

      const subResult = checkPattern(subPattern, fieldType[1], context);
      if ("err" in subResult) return subResult;

      bindings.push(...subResult.ok);
    }

    return { ok: bindings };
  }

  if ("variant" in pattern) {
    if (!("variant" in type)) {
      return { err: { not_a_variant: type } };
    }

    const caseType = type.variant.find((t) => t[0] === pattern.variant.label);
    if (!caseType) {
      return {
        err: {
          invalid_variant_label: {
            variant: type,
            label: pattern.variant.label,
          },
        },
      };
    }

    return checkPattern(pattern.variant.pattern, caseType[1], context);
  }

  if ("tuple" in pattern) {
    if (!("tuple" in type)) {
      return { err: { not_a_tuple: type } };
    }

    if (pattern.tuple.length !== type.tuple.length) {
      return {
        err: {
          type_mismatch: {
            expected: type,
            actual: { tuple: pattern.tuple.map(() => unitType) },
          },
        },
      };
    }

    const bindings: Context = [];
    for (let i = 0; i < pattern.tuple.length; i++) {
      const subPattern = pattern.tuple[i]!;
      const elementType = type.tuple[i]!;

      const subResult = checkPattern(subPattern, elementType, context);
      if ("err" in subResult) return subResult;

      bindings.push(...subResult.ok);
    }

    return { ok: bindings };
  }

  throw new Error(`Unknown pattern: ${Object.keys(pattern)[0]}`);
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
    const record: [string, Type][] = [];
    for (const [label, fieldType] of inType.record) {
      record.push([label, substituteType(target, replacement, fieldType)]);
    }
    return { record };
  }

  if ("variant" in inType) {
    const variant: [string, Type][] = [];
    for (const [label, caseType] of inType.variant) {
      variant.push([label, substituteType(target, replacement, caseType)]);
    }
    return { variant };
  }

  if ("mu" in inType) {
    if (inType.mu.var === target) return inType; // bound variable, don't substitute
    return {
      mu: {
        var: inType.mu.var,
        body: substituteType(target, replacement, inType.mu.body),
      },
    };
  }

  if ("tuple" in inType) {
    return {
      tuple: inType.tuple.map((t) => substituteType(target, replacement, t)),
    };
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
): Result<TypingError, Kind> {
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
    for (const [_, fieldType] of type.record) {
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
    for (const [_, caseType] of type.variant) {
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

  if ("mu" in type) {
    // μα.τ has kind * if τ has kind * in context extended with α::*
    const extendedContext: Context = [
      ...context,
      { type: { name: type.mu.var, kind: { star: null } } },
    ];

    const bodyKind = checkKind(extendedContext, type.mu.body);
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

  if ("tuple" in type) {
    // All element types must have kind *
    for (const elementType of type.tuple) {
      const elementKind = checkKind(context, elementType);
      if ("err" in elementKind) return elementKind;

      if (!isStarKind(elementKind.ok)) {
        return {
          err: {
            kind_mismatch: {
              expected: { star: null },
              actual: elementKind.ok,
            },
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
    const leftFields = left.record;
    const rightFields = right.record;

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
    const leftCases = left.variant;
    const rightCases = right.variant;

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

  if ("mu" in left && "mu" in right) {
    // μα.τ₁ = μβ.τ₂ if τ₁ = τ₂[β/α]
    const renamedBody = alphaRename(right.mu.var, left.mu.var, right.mu.body);
    return typesEqual(left.mu.body, renamedBody);
  }

  if ("tuple" in left && "tuple" in right) {
    if (left.tuple.length !== right.tuple.length) return false;

    return left.tuple.every((leftElem, i) =>
      typesEqual(leftElem, right.tuple[i]!),
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
    const record: [string, Type][] = [];
    for (const [label, fieldType] of type.record) {
      record.push([label, alphaRename(from, to, fieldType)]);
    }
    return { record };
  }

  if ("variant" in type) {
    const variant: [string, Type][] = [];
    for (const [label, caseType] of type.variant) {
      variant.push([label, alphaRename(from, to, caseType)]);
    }
    return { variant };
  }

  if ("mu" in type) {
    if (type.mu.var === from) return type; // shadowed
    return {
      mu: {
        var: type.mu.var,
        body: alphaRename(from, to, type.mu.body),
      },
    };
  }

  if ("tuple" in type) {
    return {
      tuple: type.tuple.map((t) => alphaRename(from, to, t)),
    };
  }

  return type;
}

export function unifyTypes(
  left: Type,
  right: Type,
  worklist: Worklist,
  subst: Substitution,
): Result<TypingError, null> {
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
    const leftFields = left.record;
    const rightFields = right.record;

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
    const leftCases = left.variant;
    const rightCases = right.variant;

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

  if ("mu" in left && "mu" in right) {
    // Unify bodies after alpha-renaming
    const renamedRight = alphaRename(right.mu.var, left.mu.var, right.mu.body);
    worklist.push({ type_eq: { left: left.mu.body, right: renamedRight } });
    return { ok: null };
  }

  if ("tuple" in left && "tuple" in right) {
    if (left.tuple.length !== right.tuple.length) {
      return {
        err: { type_mismatch: { expected: left, actual: right } },
      };
    }

    // Unify all element types
    for (let i = 0; i < left.tuple.length; i++) {
      worklist.push({
        type_eq: {
          left: left.tuple[i]!,
          right: right.tuple[i]!,
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
): Result<TypingError, null> {
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

export function unifyKinds(left: Kind, right: Kind): Result<TypingError, null> {
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
    return type.record.some((fieldType) => occursCheck(varName, fieldType[1]));
  }

  if ("variant" in type) {
    return type.variant.some((caseType) => occursCheck(varName, caseType[1]));
  }

  if ("mu" in type) {
    if (type.mu.var === varName) return false; // bound
    return occursCheck(varName, type.mu.body);
  }

  if ("tuple" in type) {
    return type.tuple.some((elementType) => occursCheck(varName, elementType));
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
    const record: [string, Type][] = [];
    for (const [label, fieldType] of type.record) {
      record.push([label, applySubstitution(subst, fieldType)]);
    }
    return { record };
  }

  if ("variant" in type) {
    const variant: [string, Type][] = [];
    for (const [label, caseType] of type.variant) {
      variant.push([label, applySubstitution(subst, caseType)]);
    }
    return { variant };
  }

  if ("mu" in type) {
    const newSubst = new Map(subst);
    newSubst.delete(type.mu.var);
    return {
      mu: {
        var: type.mu.var,
        body: applySubstitution(newSubst, type.mu.body),
      },
    };
  }

  if ("tuple" in type) {
    return {
      tuple: type.tuple.map((t) => applySubstitution(subst, t)),
    };
  }

  return type;
}

// Typing judgment: Γ ⊢ e : τ
export function inferType(
  context: Context,
  term: Term,
): Result<TypingError, Type> {
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
    const record: [string, Type][] = [];

    for (const [label, fieldTerm] of term.record) {
      const fieldType = inferType(context, fieldTerm);
      if ("err" in fieldType) return fieldType;

      record.push([label, fieldType.ok]);
    }

    return { ok: { record } };
  }

  if ("project" in term) {
    const recordType = inferType(context, term.project.record);
    if ("err" in recordType) return recordType;

    if (!("record" in recordType.ok)) {
      return {
        err: { not_a_record: recordType.ok },
      };
    }

    const fieldType = recordType.ok.record.find(
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
    const variantKind = checkKind(context, term.inject.variant_type);
    if ("err" in variantKind) return variantKind;

    if (!("variant" in term.inject.variant_type)) {
      return {
        err: { not_a_variant: term.inject.variant_type },
      };
    }

    // Check that the label exists in the variant type
    const expectedType = term.inject.variant_type.variant.find(
      (t) => t[0] === term.inject.label,
    );
    if (!expectedType) {
      return {
        err: {
          invalid_variant_label: {
            variant: term.inject.variant_type,
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

    return { ok: term.inject.variant_type };
  }

  if ("match" in term) {
    // Infer the type of the scrutinee
    const scrutineeType = inferType(context, term.match.scrutinee);
    if ("err" in scrutineeType) return scrutineeType;

    // Check exhaustiveness (simplified check)
    const patterns = term.match.cases.map(first);
    const exhaustCheck = checkExhaustive(patterns, scrutineeType.ok);
    if ("err" in exhaustCheck) return exhaustCheck;

    // Type check each case
    let resultType: Type | null = null;

    for (const [pattern, body] of term.match.cases) {
      // Check pattern and get bindings
      const patternResult = checkPattern(pattern, scrutineeType.ok, context);
      if ("err" in patternResult) return patternResult;

      // Extend context with pattern bindings
      const extendedContext: Context = [...context, ...patternResult.ok];

      // Type check the body
      const bodyType = inferType(extendedContext, body);
      if ("err" in bodyType) return bodyType;

      // Ensure all branches return the same type
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

  if ("fold" in term) {
    // fold: τ[μα.τ/α] → μα.τ
    // Check that the type is well-formed
    const muKind = checkKind(context, term.fold.type);
    if ("err" in muKind) return muKind;

    if (!("mu" in term.fold.type)) {
      return {
        err: {
          type_mismatch: { expected: term.fold.type, actual: term.fold.type },
        },
      };
    }

    // Compute the unfolded type: τ[μα.τ/α]
    const unfoldedType = substituteType(
      term.fold.type.mu.var,
      term.fold.type,
      term.fold.type.mu.body,
    );

    // Type check the term against the unfolded type
    const termType = inferType(context, term.fold.term);
    if ("err" in termType) return termType;

    if (!typesEqual(unfoldedType, termType.ok)) {
      return {
        err: {
          type_mismatch: {
            expected: unfoldedType,
            actual: termType.ok,
          },
        },
      };
    }

    return { ok: term.fold.type };
  }

  if ("unfold" in term) {
    // unfold: μα.τ → τ[μα.τ/α]
    const termType = inferType(context, term.unfold);
    if ("err" in termType) return termType;

    if (!("mu" in termType.ok)) {
      return {
        err: { not_a_function: termType.ok },
      };
    }

    // Compute the unfolded type
    const unfoldedType = substituteType(
      termType.ok.mu.var,
      termType.ok,
      termType.ok.mu.body,
    );

    return { ok: unfoldedType };
  }

  if ("tuple" in term) {
    const elementTypes: Type[] = [];

    for (const element of term.tuple) {
      const elementType = inferType(context, element);
      if ("err" in elementType) return elementType;

      elementTypes.push(elementType.ok);
    }

    return { ok: { tuple: elementTypes } };
  }

  if ("tupleProject" in term) {
    const tupleType = inferType(context, term.tupleProject.tuple);
    if ("err" in tupleType) return tupleType;

    if (!("tuple" in tupleType.ok)) {
      return {
        err: { not_a_tuple: tupleType.ok },
      };
    }

    const index = term.tupleProject.index;
    if (index < 0 || index >= tupleType.ok.tuple.length) {
      return {
        err: {
          tuple_index_out_of_bounds: {
            tuple: tupleType.ok,
            index,
          },
        },
      };
    }

    return { ok: tupleType.ok.tuple[index]! };
  }

  throw new Error(`Unknown term: ${Object.keys(term)[0]}`);
}

// Worklist constraint solver
export function solveConstraints(
  worklist: Worklist,
  subst: Substitution = new Map(),
): Result<TypingError, Substitution> {
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
): Result<TypingError, null> {
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
): Result<TypingError, Type> {
  return inferType(context, term);
}

// Type checking with constraint solving (for more complex scenarios)
export function typecheckWithConstraints(
  context: Context,
  term: Term,
): Result<TypingError, Type> {
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

// Type Constructors:
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
export const record_type = (record: [string, Type][]) => ({ record });
export const variant_type = (variant: [string, Type][]) => ({ variant });
export const mu_type = (var_name: string, body: Type): Type => ({
  mu: { var: var_name, body },
});
export const tuple_type = (elements: Type[]): Type => ({ tuple: elements });

// Term Constructors:
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
export const record_term = (record: [string, Term][]) => ({ record });
export const project_term = (record: Term, label: string) => ({
  project: { record, label },
});
export const inject_term = (
  label: string,
  value: Term,
  variant_type: Type,
) => ({
  inject: { label, value, variant_type },
});
export const match_term = (
  scrutinee: Term,
  cases: [Pattern, Term][],
): Term => ({
  match: { scrutinee, cases },
});
export const fold_term = (type: Type, term: Term): Term => ({
  fold: { type, term },
});
export const unfold_term = (term: Term): Term => ({
  unfold: term,
});
export const tuple_term = (elements: Term[]): Term => ({ tuple: elements });
export const tuple_project_term = (tuple: Term, index: number): Term => ({
  tupleProject: { tuple, index },
});

// Pattern Constructors
export const var_pattern = (name: string): Pattern => ({ var: name });
export const wildcard_pattern = (): Pattern => ({ wildcard: null });
export const con_pattern = (name: string, type: Type): Pattern => ({
  con: { name, type },
});
export const record_pattern = (fields: [string, Pattern][]): Pattern => ({
  record: fields,
});
export const variant_pattern = (label: string, pattern: Pattern): Pattern => ({
  variant: { label, pattern },
});
export const tuple_pattern = (elements: Pattern[]): Pattern => ({
  tuple: elements,
});

export const unitType: Type = { con: "Unit" };
export const unitValue: Term = { con: { name: "()", type: unitType } };
