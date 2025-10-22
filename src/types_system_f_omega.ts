// ./src/types_system_f_omega.ts

// utility function
const first = <T, U>(tuple: [T, U]) => tuple[0];

// Trait definition - defines required methods
export type TraitDef = {
  name: string;
  type_param: string; // e.g., "Self"
  kind: Kind;
  methods: [string, Type][]; // method name -> method type
};

// Trait constraint
export type TraitConstraint = {
  trait: string;
  type: Type;
};

// Add to Type union
export type BoundedForallType = {
  bounded_forall: {
    var: string;
    kind: Kind;
    constraints: TraitConstraint[];
    body: Type;
  };
};

// Kinds (types of types)
export type StarKind = { star: null };
export type ArrowKind = { arrow: { from: Kind; to: Kind } };
export type Kind =
  | StarKind // * (base kind for proper types)
  | ArrowKind; // κ₁ → κ₂

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
  | BoundedForallType // trait type
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
export type LetTerm = { let: { name: string; value: Term; body: Term } };
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
// Trait dictionary (evidence of implementation)
export type DictTerm = {
  dict: {
    trait: string;
    type: Type;
    methods: [string, Term][]; // method implementations
  };
};
// Access trait method with evidence
export type TraitMethodTerm = {
  trait_method: {
    dict: Term; // dictionary/evidence
    method: string;
  };
};
// Lambda that takes trait evidence
export type TraitLamTerm = {
  trait_lam: {
    trait_var: string; // dict variable name
    trait: string;
    type_var: string;
    kind: Kind;
    constraints: TraitConstraint[];
    body: Term;
  };
};
// Application with trait evidence
export type TraitAppTerm = {
  trait_app: {
    term: Term;
    type: Type;
    dicts: Term[]; // evidence for each constraint
  };
};
export type Term =
  | AppTerm // e₁ e₂
  | ConTerm // constants with their types
  | DictTerm // trait dict
  | FoldTerm // fold: τ[μα.τ/α] → μα.τ
  | InjectTerm // <l=e> as T
  | LamTerm // λx:τ.e
  | LetTerm
  | MatchTerm // match e { l₁(x₁) => e₁ | l₂(x₂) => e₂ }
  | ProjectTerm // e.l
  | RecordTerm // {l₁=e₁, l₂=e₂, ...}
  | TraitAppTerm // trat application
  | TraitLamTerm // trait lambda
  | TraitMethodTerm // trait method
  | TupleProjectTerm // tuple[0]
  | TupleTerm // tuple: #(l₁, l₂, ...)
  | TyAppTerm // e [τ]
  | TyLamTerm // Λα::κ.e
  | UnfoldTerm // unfold: μα.τ → τ[μα.τ/α]
  | VarTerm; // variable x

// Context entries for type checking
export type TermBinding = { term: { name: string; type: Type } };
export type TypeBinding = { type: { name: string; kind: Kind } };
export type TraitDefBinding = {
  trait_def: TraitDef;
};

export type TraitImplBinding = {
  trait_impl: {
    trait: string;
    type: Type;
    dict: Term; // dictionary term
  };
};

export type DictBinding = {
  dict: {
    name: string; // dictionary variable
    trait: string;
    type: Type;
  };
};
export type Binding =
  | TermBinding // x : τ
  | TypeBinding // α :: κ
  | TraitDefBinding // trait definition
  | TraitImplBinding // trait impl
  | DictBinding; // dict binding
export type Context = Binding[];

export type UnboundTypeError = { unbound: string };
export type KindMismatchTypeError = {
  kind_mismatch: { expected: Kind; actual: Kind };
};
export type TypeMismatchTypeError = {
  type_mismatch: { expected: Type; actual: Type };
};
export type NotAFunctionTypeError = { not_a_function: Type };
export type NotATypeFunctionTypeError = { not_a_type_function: Type };
export type CyclicTypeError = { cyclic: string };
export type NotARecordTypeError = { not_a_record: Type };
export type MissingFieldTypeError = {
  missing_field: { record: Type; label: string };
};
export type NotAVariantTypeError = { not_a_variant: Type };
export type InvalidVariantTypeError = {
  invalid_variant_label: { variant: Type; label: string };
};
export type MissingCaseTypeError = { missing_case: { label: string } };
export type ExtraCaseTypeError = { extra_case: { label: string } };
export type NotATupleTypeError = { not_a_tuple: Type };
export type TupleIndexOutofBoundsTypeError = {
  tuple_index_out_of_bounds: { tuple: Type; index: number };
};
export type MissingTraitImplError = {
  missing_trait_impl: { trait: string; type: Type };
};
export type MissingMethodError = {
  missing_method: { trait: string; method: string };
};
export type WrongNumberOfDictsError = {
  wrong_number_of_dicts: { expected: number; actual: number };
};
export type TypingError =
  | CyclicTypeError
  | ExtraCaseTypeError
  | InvalidVariantTypeError
  | KindMismatchTypeError
  | MissingCaseTypeError
  | MissingFieldTypeError
  | MissingMethodError
  | MissingTraitImplError
  | NotAFunctionTypeError
  | NotARecordTypeError
  | NotATupleTypeError
  | NotATypeFunctionTypeError
  | NotAVariantTypeError
  | TupleIndexOutofBoundsTypeError
  | TypeMismatchTypeError
  | UnboundTypeError
  | WrongNumberOfDictsError;

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
  if ("bounded_forall" in t) {
    const constraints = t.bounded_forall.constraints
      .map((c) => `${c.trait}<${showType(c.type)}>`)
      .join(", ");
    return `∀${t.bounded_forall.var}::${showKind(t.bounded_forall.kind)} where ${constraints}.${showType(t.bounded_forall.body)}`;
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
  if ("let" in t)
    return `let ${t.let.name} = ${showTerm(t.let.value)} in ${showTerm(t.let.body)}`;

  if ("record" in t) {
    const fields = t.record
      .map(([label, term]) => `${label} = ${showTerm(term)}`)
      .join(", ");
    return `{${fields}}`;
  }
  if ("trait_lam" in t) {
    const constraints = t.trait_lam.constraints
      .map((c) => `${c.trait}<${showType(c.type)}>`)
      .join(", ");
    return `Λ${t.trait_lam.type_var}::${showKind(t.trait_lam.kind)} where ${constraints}. ${showTerm(t.trait_lam.body)}`;
  }

  if ("trait_app" in t) {
    const dicts = t.trait_app.dicts.map(showTerm).join(", ");
    return `${showTerm(t.trait_app.term)} [${showType(t.trait_app.type)}] {${dicts}}`;
  }

  if ("dict" in t) {
    const methods = t.dict.methods
      .map(([name, impl]) => `${name} = ${showTerm(impl)}`)
      .join(", ");
    return `dict ${t.dict.trait}<${showType(t.dict.type)}> { ${methods} }`;
  }

  if ("trait_method" in t) {
    return `${showTerm(t.trait_method.dict)}.${t.trait_method.method}`;
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

// Check if a type implements a trait
export function checkTraitImplementation(
  context: Context,
  trait: string,
  type: Type,
): Result<TypingError, Term> {
  // Look for trait implementation in context
  const impl = context.find(
    (b) =>
      "trait_impl" in b &&
      b.trait_impl.trait === trait &&
      typesEqual(b.trait_impl.type, type),
  );

  if (impl && "trait_impl" in impl) {
    return { ok: impl.trait_impl.dict };
  }

  return {
    err: {
      missing_trait_impl: { trait, type },
    } as TypingError,
  };
}

// Verify all trait constraints are satisfied
export function checkTraitConstraints(
  context: Context,
  constraints: TraitConstraint[],
): Result<TypingError, Term[]> {
  const dicts: Term[] = [];

  for (const constraint of constraints) {
    const result = checkTraitImplementation(
      context,
      constraint.trait,
      constraint.type,
    );

    if ("err" in result) return result;
    dicts.push(result.ok);
  }

  return { ok: dicts };
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
  avoidInfinite: Set<string> = new Set(),
): Type {
  if ("var" in inType) return inType.var === target ? replacement : inType;

  if ("arrow" in inType)
    return {
      arrow: {
        from: substituteType(
          target,
          replacement,
          inType.arrow.from,
          avoidInfinite,
        ),
        to: substituteType(target, replacement, inType.arrow.to, avoidInfinite),
      },
    };

  if ("bounded_forall" in inType && inType.bounded_forall.var !== target) {
    return {
      bounded_forall: {
        var: inType.bounded_forall.var,
        kind: inType.bounded_forall.kind,
        constraints: inType.bounded_forall.constraints.map((c) => ({
          trait: c.trait,
          type: substituteType(target, replacement, c.type, avoidInfinite),
        })),
        body: substituteType(
          target,
          replacement,
          inType.bounded_forall.body,
          avoidInfinite,
        ),
      },
    };
  }

  if ("forall" in inType && inType.forall.var !== target)
    return {
      forall: {
        var: inType.forall.var,
        kind: inType.forall.kind,
        body: substituteType(
          target,
          replacement,
          inType.forall.body,
          avoidInfinite,
        ),
      },
    };

  if ("app" in inType)
    return {
      app: {
        func: substituteType(
          target,
          replacement,
          inType.app.func,
          avoidInfinite,
        ),
        arg: substituteType(target, replacement, inType.app.arg, avoidInfinite),
      },
    };

  if ("lam" in inType)
    return {
      lam: {
        body: substituteType(
          target,
          replacement,
          inType.lam.body,
          avoidInfinite,
        ),
        kind: inType.lam.kind,
        var: inType.lam.var,
      },
    };

  if ("record" in inType) {
    const record: [string, Type][] = [];
    for (const [label, fieldType] of inType.record) {
      record.push([
        label,
        substituteType(target, replacement, fieldType, avoidInfinite),
      ]);
    }
    return { record };
  }

  if ("variant" in inType) {
    const variant: [string, Type][] = [];
    for (const [label, caseType] of inType.variant) {
      variant.push([
        label,
        substituteType(target, replacement, caseType, avoidInfinite),
      ]);
    }
    return { variant };
  }

  if ("mu" in inType) {
    if (inType.mu.var === target) return inType; // bound variable, don't substitute

    // Check if we're about to create an infinite recursion
    if (avoidInfinite.has(inType.mu.var)) {
      return inType; // Stop recursion
    }

    const newAvoidInfinite = new Set(avoidInfinite);
    newAvoidInfinite.add(inType.mu.var);

    return {
      mu: {
        var: inType.mu.var,
        body: substituteType(
          target,
          replacement,
          inType.mu.body,
          newAvoidInfinite,
        ),
      },
    };
  }

  if ("tuple" in inType) {
    return {
      tuple: inType.tuple.map((t) =>
        substituteType(target, replacement, t, avoidInfinite),
      ),
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

  if ("bounded_forall" in type) {
    const extendedContext: Context = [
      ...context,
      {
        type: {
          name: type.bounded_forall.var,
          kind: type.bounded_forall.kind,
        },
      },
    ];

    // Check that all constraint types are well-kinded
    for (const constraint of type.bounded_forall.constraints) {
      const constraintKind = checkKind(extendedContext, constraint.type);
      if ("err" in constraintKind) return constraintKind;

      if (!isStarKind(constraintKind.ok)) {
        return {
          err: {
            kind_mismatch: {
              expected: { star: null },
              actual: constraintKind.ok,
            },
          },
        };
      }
    }

    const bodyKind = checkKind(extendedContext, type.bounded_forall.body);
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
  left = normalizeType(left);
  right = normalizeType(right);

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

  if ("bounded_forall" in left && "bounded_forall" in right) {
    if (!kindsEqual(left.bounded_forall.kind, right.bounded_forall.kind))
      return false;

    // Check constraints match
    if (
      left.bounded_forall.constraints.length !==
      right.bounded_forall.constraints.length
    )
      return false;

    for (let i = 0; i < left.bounded_forall.constraints.length; i++) {
      const lc = left.bounded_forall.constraints[i]!;
      const rc = right.bounded_forall.constraints[i]!;

      if (lc.trait !== rc.trait) return false;

      const renamedConstraintType = alphaRename(
        right.bounded_forall.var,
        left.bounded_forall.var,
        rc.type,
      );

      if (!typesEqual(lc.type, renamedConstraintType)) return false;
    }
    const renamedBody = alphaRename(
      right.bounded_forall.var,
      left.bounded_forall.var,
      right.bounded_forall.body,
    );
    return typesEqual(left.bounded_forall.body, renamedBody);
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

  if ("bounded_forall" in type) {
    if (type.bounded_forall.var === from) return type;
    return {
      bounded_forall: {
        var: type.bounded_forall.var,
        kind: type.bounded_forall.kind,
        constraints: type.bounded_forall.constraints.map((c) => ({
          trait: c.trait,
          type: alphaRename(from, to, c.type),
        })),
        body: alphaRename(from, to, type.bounded_forall.body),
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

  if ("bounded_forall" in left && "bounded_forall" in right) {
    worklist.push({
      kind_eq: {
        left: left.bounded_forall.kind,
        right: right.bounded_forall.kind,
      },
    });

    // Check constraints match
    if (
      left.bounded_forall.constraints.length !==
      right.bounded_forall.constraints.length
    ) {
      return {
        err: { type_mismatch: { expected: left, actual: right } },
      };
    }

    for (let i = 0; i < left.bounded_forall.constraints.length; i++) {
      const lc = left.bounded_forall.constraints[i]!;
      const rc = right.bounded_forall.constraints[i]!;

      if (lc.trait !== rc.trait) {
        return {
          err: { type_mismatch: { expected: left, actual: right } },
        };
      }

      const renamedConstraintType = alphaRename(
        right.bounded_forall.var,
        left.bounded_forall.var,
        rc.type,
      );

      worklist.push({
        type_eq: { left: lc.type, right: renamedConstraintType },
      });
    }

    const renamedRight = alphaRename(
      right.bounded_forall.var,
      left.bounded_forall.var,
      right.bounded_forall.body,
    );
    worklist.push({
      type_eq: { left: left.bounded_forall.body, right: renamedRight },
    });

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

  if ("bounded_forall" in type) {
    const newSubst = new Map(subst);
    newSubst.delete(type.bounded_forall.var);
    return {
      bounded_forall: {
        var: type.bounded_forall.var,
        kind: type.bounded_forall.kind,
        constraints: type.bounded_forall.constraints.map((c) => ({
          trait: c.trait,
          type: applySubstitution(subst, c.type),
        })),
        body: applySubstitution(newSubst, type.bounded_forall.body),
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
    // Check for term binding
    const termBinding = context.find(
      (b) => "term" in b && b.term.name === term.var,
    );
    if (termBinding && "term" in termBinding) {
      return { ok: termBinding.term.type };
    }

    // Check for dict binding
    const dictBinding = context.find(
      (b) => "dict" in b && b.dict.name === term.var,
    );
    if (dictBinding && "dict" in dictBinding) {
      // Return a dictionary type marker
      return {
        ok: {
          con: `Dict<${dictBinding.dict.trait}, ${showType(dictBinding.dict.type)}>`,
        },
      };
    }

    return { err: { unbound: term.var } };
  }

  if ("con" in term) {
    return { ok: term.con.type };
  }

  if ("lam" in term) {
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

  if ("let" in term) {
    const valueType = inferType(context, term.let.value);
    if ("err" in valueType) return valueType;

    const extendedContext: Context = [
      ...context,
      { term: { name: term.let.name, type: valueType.ok } },
    ];

    const bodyType = inferType(extendedContext, term.let.body);
    if ("err" in bodyType) return bodyType;

    return { ok: bodyType.ok };
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

    const substituted = substituteType(
      termType.ok.forall.var,
      term.tyapp.type,
      termType.ok.forall.body,
    );

    return { ok: substituted };
  }

  if ("dict" in term) {
    const traitDef = context.find(
      (b) => "trait_def" in b && b.trait_def.name === term.dict.trait,
    );

    if (!traitDef || !("trait_def" in traitDef)) {
      return { err: { unbound: term.dict.trait } };
    }

    const typeKind = checkKind(context, term.dict.type);
    if ("err" in typeKind) return typeKind;

    if (!kindsEqual(traitDef.trait_def.kind, typeKind.ok)) {
      return {
        err: {
          kind_mismatch: {
            expected: traitDef.trait_def.kind,
            actual: typeKind.ok,
          },
        },
      };
    }

    const requiredMethods = new Set(
      traitDef.trait_def.methods.map((m) => m[0]),
    );
    const providedMethods = new Set(term.dict.methods.map((m) => m[0]));

    for (const required of requiredMethods) {
      if (!providedMethods.has(required)) {
        return {
          err: {
            missing_method: { trait: term.dict.trait, method: required },
          },
        };
      }
    }

    for (const [methodName, methodImpl] of term.dict.methods) {
      const expectedMethod = traitDef.trait_def.methods.find(
        (m) => m[0] === methodName,
      );

      if (!expectedMethod) {
        return {
          err: {
            missing_method: { trait: term.dict.trait, method: methodName },
          },
        };
      }

      const expectedType = substituteType(
        traitDef.trait_def.type_param,
        term.dict.type,
        expectedMethod[1],
      );

      const implType = inferType(context, methodImpl);
      if ("err" in implType) return implType;

      if (!typesEqual(expectedType, implType.ok)) {
        return {
          err: {
            type_mismatch: {
              expected: expectedType,
              actual: implType.ok,
            },
          },
        };
      }
    }

    return {
      ok: {
        con: `Dict<${term.dict.trait}, ${showType(term.dict.type)}>`,
      },
    };
  }

  if ("trait_lam" in term) {
    // Add type variable to context
    const extendedContext: Context = [
      ...context,
      {
        type: {
          name: term.trait_lam.type_var,
          kind: term.trait_lam.kind,
        },
      },
      // Add dictionary variable for the trait constraint
      {
        dict: {
          name: term.trait_lam.trait_var,
          trait: term.trait_lam.trait,
          type: { var: term.trait_lam.type_var },
        },
      },
    ];

    // Verify trait exists
    const traitDef = context.find(
      (b) => "trait_def" in b && b.trait_def.name === term.trait_lam.trait,
    );

    if (!traitDef || !("trait_def" in traitDef)) {
      return { err: { unbound: term.trait_lam.trait } };
    }

    const bodyType = inferType(extendedContext, term.trait_lam.body);
    if ("err" in bodyType) return bodyType;

    return {
      ok: {
        bounded_forall: {
          var: term.trait_lam.type_var,
          kind: term.trait_lam.kind,
          constraints: term.trait_lam.constraints,
          body: bodyType.ok,
        },
      },
    };
  }

  // trait application
  if ("trait_app" in term) {
    const termType = inferType(context, term.trait_app.term);
    if ("err" in termType) return termType;

    if (!("bounded_forall" in termType.ok)) {
      return {
        err: {
          type_mismatch: {
            expected: termType.ok,
            actual: term.trait_app.type,
          },
        },
      };
    }

    // Check that the type argument has the expected kind
    const argKind = checkKind(context, term.trait_app.type);
    if ("err" in argKind) return argKind;
    const bounded_forall = termType.ok.bounded_forall;
    if (!kindsEqual(bounded_forall.kind, argKind.ok)) {
      return {
        err: {
          kind_mismatch: {
            expected: termType.ok.bounded_forall.kind,
            actual: argKind.ok,
          },
        },
      };
    }

    // Substitute type variable in constraints
    const instantiatedConstraints = termType.ok.bounded_forall.constraints.map(
      (c) => ({
        trait: c.trait,
        type: substituteType(bounded_forall.var, term.trait_app.type, c.type),
      }),
    );

    // Check that all trait constraints are satisfied
    const dictsResult = checkTraitConstraints(context, instantiatedConstraints);
    if ("err" in dictsResult) return dictsResult;

    // Verify the provided dictionaries match the expected ones
    if (term.trait_app.dicts.length !== dictsResult.ok.length) {
      return {
        err: {
          wrong_number_of_dicts: {
            expected: dictsResult.ok.length,
            actual: term.trait_app.dicts.length,
          },
        },
      };
    }

    // Type check each provided dictionary
    for (let i = 0; i < term.trait_app.dicts.length; i++) {
      const providedDict = term.trait_app.dicts[i]!;
      const dictType = inferType(context, providedDict);
      if ("err" in dictType) return dictType;

      // Verify it's actually a dictionary for the right trait/type
      // This is a simplified check - you might want more sophisticated checking
      if ("dict" in providedDict) {
        const constraint = instantiatedConstraints[i]!;
        if (
          providedDict.dict.trait !== constraint.trait ||
          !typesEqual(providedDict.dict.type, constraint.type)
        ) {
          return {
            err: {
              type_mismatch: {
                expected: { con: `Dict<${constraint.trait}>` },
                actual: dictType.ok,
              },
            },
          };
        }
      }
    }

    // Substitute the type argument in the body
    const resultType = substituteType(
      termType.ok.bounded_forall.var,
      term.trait_app.type,
      termType.ok.bounded_forall.body,
    );

    return { ok: resultType };
  }

  if ("trait_method" in term) {
    const dictType = inferType(context, term.trait_method.dict);
    if ("err" in dictType) return dictType;

    // Check if the dictionary is a variable bound in the context
    const dictTerm = term.trait_method.dict;

    if ("var" in dictTerm) {
      // dictTerm is a VarTerm
      const dictBinding = context.find(
        (b) => "dict" in b && b.dict.name === dictTerm.var,
      );

      if (dictBinding && "dict" in dictBinding) {
        // Look up the trait definition
        const traitDef = context.find(
          (b) =>
            "trait_def" in b && b.trait_def.name === dictBinding.dict.trait,
        );

        if (!traitDef || !("trait_def" in traitDef)) {
          return { err: { unbound: dictBinding.dict.trait } };
        }

        // Find the method in the trait definition
        const method = traitDef.trait_def.methods.find(
          (m) => m[0] === term.trait_method.method,
        );

        if (!method) {
          return {
            err: {
              missing_method: {
                trait: dictBinding.dict.trait,
                method: term.trait_method.method,
              },
            },
          };
        }

        // Substitute the type parameter with the concrete type
        const methodType = substituteType(
          traitDef.trait_def.type_param,
          dictBinding.dict.type,
          method[1],
        );

        return { ok: methodType };
      }
    }

    // If it's a concrete dictionary term
    if ("dict" in dictTerm) {
      // dictTerm is a DictTerm
      const dict = dictTerm.dict;

      // Look up the trait definition
      const traitDef = context.find(
        (b) => "trait_def" in b && b.trait_def.name === dict.trait,
      );

      if (!traitDef || !("trait_def" in traitDef)) {
        return { err: { unbound: dict.trait } };
      }

      // Find the method
      const methodImpl = dict.methods.find(
        (m) => m[0] === term.trait_method.method,
      );

      if (!methodImpl) {
        return {
          err: {
            missing_method: {
              trait: dict.trait,
              method: term.trait_method.method,
            },
          },
        };
      }

      // Return the type of the method implementation
      return inferType(context, methodImpl[1]);
    }

    return {
      err: {
        type_mismatch: {
          expected: { con: "Dictionary" },
          actual: dictType.ok,
        },
      },
    };
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
    let variantType = term.inject.variant_type;

    if ("mu" in variantType) {
      variantType = substituteType(
        variantType.mu.var,
        variantType,
        variantType.mu.body,
        new Set([variantType.mu.var]),
      );
    }

    const variantKind = checkKind(context, variantType);
    if ("err" in variantKind) return variantKind;

    if (!("variant" in variantType)) {
      return {
        err: { not_a_variant: variantType },
      };
    }

    const expectedType = variantType.variant.find(
      (t) => t[0] === term.inject.label,
    );
    if (!expectedType) {
      return {
        err: {
          invalid_variant_label: {
            variant: variantType,
            label: term.inject.label,
          },
        },
      };
    }

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
    const scrutineeType = inferType(context, term.match.scrutinee);
    if ("err" in scrutineeType) return scrutineeType;

    const patterns = term.match.cases.map(first);
    const exhaustCheck = checkExhaustive(patterns, scrutineeType.ok);
    if ("err" in exhaustCheck) return exhaustCheck;

    let resultType: Type | null = null;

    for (const [pattern, body] of term.match.cases) {
      const patternResult = checkPattern(pattern, scrutineeType.ok, context);
      if ("err" in patternResult) return patternResult;

      const extendedContext: Context = [...context, ...patternResult.ok];

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

  if ("fold" in term) {
    const muKind = checkKind(context, term.fold.type);
    if ("err" in muKind) return muKind;

    if (!("mu" in term.fold.type)) {
      return {
        err: {
          type_mismatch: { expected: term.fold.type, actual: term.fold.type },
        },
      };
    }

    const unfoldedType = substituteType(
      term.fold.type.mu.var,
      term.fold.type,
      term.fold.type.mu.body,
      new Set([term.fold.type.mu.var]),
    );

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
    const termType = inferType(context, term.unfold);
    if ("err" in termType) return termType;

    if (!("mu" in termType.ok)) {
      return {
        err: { not_a_function: termType.ok },
      };
    }

    const unfoldedType = substituteType(
      termType.ok.mu.var,
      termType.ok,
      termType.ok.mu.body,
      new Set([termType.ok.mu.var]),
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

    return unifyTypes(
      normalizeType(left),
      normalizeType(right),
      worklist,
      subst,
    );
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

export function normalizeType(type: Type, seen = new Set<string>()): Type {
  // First, normalize any subtypes
  const normalized = normalizeSubtypes(type, seen);

  // Then apply specific normalization rules
  return applyNormalizationRules(normalized, seen);
}

function normalizeSubtypes(type: Type, seen: Set<string>): Type {
  if ("var" in type || "con" in type) {
    return type;
  }

  if ("arrow" in type) {
    return {
      arrow: {
        from: normalizeType(type.arrow.from, seen),
        to: normalizeType(type.arrow.to, seen),
      },
    };
  }

  if ("forall" in type) {
    return {
      forall: {
        var: type.forall.var,
        kind: type.forall.kind,
        body: normalizeType(type.forall.body, seen),
      },
    };
  }

  if ("bounded_forall" in type) {
    return {
      bounded_forall: {
        var: type.bounded_forall.var,
        kind: type.bounded_forall.kind,
        constraints: type.bounded_forall.constraints.map((c) => ({
          trait: c.trait,
          type: normalizeType(c.type, seen),
        })),
        body: normalizeType(type.bounded_forall.body, seen),
      },
    };
  }

  if ("app" in type) {
    return {
      app: {
        func: normalizeType(type.app.func, seen),
        arg: normalizeType(type.app.arg, seen),
      },
    };
  }

  if ("lam" in type) {
    return {
      lam: {
        var: type.lam.var,
        kind: type.lam.kind,
        body: normalizeType(type.lam.body, seen),
      },
    };
  }

  if ("record" in type) {
    const record: [string, Type][] = [];
    for (const [label, fieldType] of type.record) {
      record.push([label, normalizeType(fieldType, seen)]);
    }
    return { record };
  }

  if ("variant" in type) {
    const variant: [string, Type][] = [];
    for (const [label, caseType] of type.variant) {
      variant.push([label, normalizeType(caseType, seen)]);
    }
    return { variant };
  }

  if ("mu" in type) {
    if (seen.has(type.mu.var)) {
      return type;
    }
    const newSeen = new Set(seen);
    newSeen.add(type.mu.var);

    return {
      mu: {
        var: type.mu.var,
        body: normalizeType(type.mu.body, newSeen),
      },
    };
  }

  if ("tuple" in type) {
    return {
      tuple: type.tuple.map((t) => normalizeType(t, seen)),
    };
  }

  return type;
}

function applyNormalizationRules(type: Type, seen: Set<string>): Type {
  // Rule 1: Beta-reduce type applications
  if ("app" in type && "lam" in type.app.func) {
    const lam = type.app.func;
    // Substitute the argument for the lambda variable in the body
    return normalizeType(
      substituteType(lam.lam.var, type.app.arg, lam.lam.body),
      seen,
    );
  }

  // Rule 2: Simplify trivial forall types
  if (
    "forall" in type &&
    !typeVariablesInType(type.forall.body, new Set([type.forall.var]))
  ) {
    // If the forall variable doesn't appear in the body, we can drop it
    return normalizeType(type.forall.body, seen);
  }

  // Rule 3: Simplify records with empty fields
  if ("record" in type && type.record.length === 0) {
    return unitType;
  }

  if (
    "bounded_forall" in type &&
    !typeVariablesInType(
      type.bounded_forall.body,
      new Set([type.bounded_forall.var]),
    )
  ) {
    return normalizeType(type.bounded_forall.body, seen);
  }

  // Rule 4: Simplify tuples with one element
  if ("tuple" in type && type.tuple.length === 1) {
    return normalizeType(type.tuple[0]!, seen);
  }

  return type;
}

function typeVariablesInType(type: Type, vars: Set<string>): boolean {
  if ("var" in type) {
    return vars.has(type.var);
  }

  if ("arrow" in type) {
    return (
      typeVariablesInType(type.arrow.from, vars) ||
      typeVariablesInType(type.arrow.to, vars)
    );
  }

  if ("forall" in type) {
    const newVars = new Set(vars);
    newVars.add(type.forall.var);
    return typeVariablesInType(type.forall.body, newVars);
  }

  if ("app" in type) {
    return (
      typeVariablesInType(type.app.func, vars) ||
      typeVariablesInType(type.app.arg, vars)
    );
  }

  if ("lam" in type) {
    const newVars = new Set(vars);
    newVars.add(type.lam.var);
    return typeVariablesInType(type.lam.body, newVars);
  }

  if ("record" in type) {
    return type.record.some(([_, fieldType]) =>
      typeVariablesInType(fieldType, vars),
    );
  }

  if ("variant" in type) {
    return type.variant.some(([_, caseType]) =>
      typeVariablesInType(caseType, vars),
    );
  }

  if ("mu" in type) {
    const newVars = new Set(vars);
    newVars.add(type.mu.var);
    return typeVariablesInType(type.mu.body, newVars);
  }

  if ("tuple" in type) {
    return type.tuple.some((t) => typeVariablesInType(t, vars));
  }

  return false;
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
export const bounded_forall_type = (
  name: string,
  kind: Kind,
  constraints: TraitConstraint[],
  body: Type,
): Type => ({
  bounded_forall: { var: name, kind, constraints, body },
});

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
export const let_term = (name: string, value: Term, body: Term): Term => ({
  let: { name, value, body },
});
export const trait_lam_term = (
  trait_var: string,
  trait: string,
  type_var: string,
  kind: Kind,
  constraints: TraitConstraint[],
  body: Term,
): Term => ({
  trait_lam: { trait_var, trait, type_var, kind, constraints, body },
});

export const trait_app_term = (
  term: Term,
  type: Type,
  dicts: Term[],
): Term => ({
  trait_app: { term, type, dicts },
});

export const dict_term = (
  trait: string,
  type: Type,
  methods: [string, Term][],
): Term => ({
  dict: { trait, type, methods },
});

export const trait_method_term = (dict: Term, method: string): Term => ({
  trait_method: { dict, method },
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
