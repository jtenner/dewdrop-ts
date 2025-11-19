import {
  appTerm,
  appType,
  arrowKind,
  arrowType,
  conPattern,
  conTerm,
  conType,
  forallType,
  freshMetaVar,
  lamTerm,
  matchTerm,
  type Pattern,
  recordType,
  starKind,
  state,
  type Term,
  type Type,
  tupleTerm,
  tupleType,
  tylamTerm,
  varTerm,
  varType,
  wildcardPattern,
  recordTerm,
  projectTerm,
  type EnumDefBinding,
  tuplePattern,
  varPattern,
  recordPattern,
  dictTerm,
} from "system-f-omega";
import {
  type ApplicationTypeExpression,
  type BlockExpression,
  type BoolExpression,
  type BuiltinDeclaration,
  type CallExpression,
  type ConstructorPatternExpression,
  type Declaration,
  type EnumDeclaration,
  type EnumVariant,
  type Expression,
  type FloatExpression,
  type FloatPatternExpression,
  type Fn,
  type FnDeclaration,
  type FnExpression,
  type FnImport,
  type FnTypeExpression,
  type IfExpression,
  type ImplDeclaration,
  type Import,
  type ImportDeclaration,
  type InfixExpression,
  type IntExpression,
  type IntPatternExpression,
  type LetDeclaration,
  type MatchExpression,
  type Module,
  type NameIdentifier,
  type PatternExpression,
  type PostfixExpression,
  type PrefixExpression,
  type RecordExpression,
  type RecordPatternExpression,
  type RecordTypeExpression,
  type SelectExpression,
  type SelectTypeExpression,
  showBodyExpression,
  showTypeExpression,
  type StringPatternExpression,
  type TraitDeclaration,
  type TraitFn,
  type TupleExpression,
  type TuplePatternExpression,
  type TupleTypeExpression,
  type TypeDeclaration,
  type TypeExpression,
  type TypeIdentifier,
} from "./parser.js";
import type {
  ASTNode,
  CompilerContext,
  Scope,
  ScopeElement,
  WorkItem,
} from "./util.js";
import type { StringToken } from "./lexer.js";
import type { CompilerError } from "./visitor.js";

type TypeContextState = ReturnType<typeof state> &
  CompilerContext & {
    constructors: Map<string, Type>;
    errors: CompilerError[];
  };

export function elaborate(module: Module, cc: CompilerContext) {
  const worklist = [] as WorkItem[];
  const ctx = {
    ...state(),
    constructors: new Map(),
    errors: [],
    ...cc,
  } satisfies TypeContextState;
  for (const decl of module.module) {
    elaborateDeclaration(decl, worklist, ctx);
  }
  return worklist;
}

export function elaborateDeclaration(
  decl: Declaration,
  worklist: WorkItem[],
  ctx: TypeContextState,
) {
  if ("builtin" in decl) elaborateBuiltinDeclaration(decl, worklist);
  else if ("enum" in decl) elaborateEnumDeclaration(decl, worklist, ctx);
  else if ("fn" in decl) elaborateFnDeclaration(decl, worklist, ctx);
  else if ("let_dec" in decl) elaborateLetDeclaration(decl, worklist, ctx);
  else if ("type_dec" in decl) elaborateTypeDeclaration(decl, worklist);
  else if ("trait" in decl) elaborateTraitDeclaration(decl, worklist);
  else if ("impl" in decl) elaborateImplDeclaration(decl, worklist, ctx);
  else if ("import_dec" in decl)
    elaborateImportDeclaration(decl, worklist, ctx);
  else throw new Error(`Declaration not supported.`);
}

export function elaborateImportDeclaration(
  decl: ImportDeclaration,
  worklist: WorkItem[],
  ctx: TypeContextState,
) {
  // TODO: There should be an import scope element for each import element
  // TODO: Find the worklist items related to the scope elements
  // TODO: Reimport those items
  // TODO: Figure out how to handle "Aliases" and term renaming
}

export function elaborateImplDeclaration(
  decl: ImplDeclaration,
  worklist: WorkItem[],
  ctx: TypeContextState,
) {
  // 1. The concrete type (Self)
  const forType = elaborateTypeExpression(decl.impl.for);

  // 2. The trait reference
  let traitType = conType(decl.impl.name.type) as Type;
  for (const param of decl.impl.trait_params) {
    traitType = appType(traitType, elaborateTypeExpression(param));
  }

  // 3. Elaborate functions, KEEPING the type
  const methods = decl.impl.fns.map((fn) => {
    const [fnType, fnTerm] = elaborateFn(fn, ctx);
    return {
      name: fn.name!.name,
      fnTerm,
      fnType,
    };
  });

  // 4. Build the dictionary term (runtime representation)
  // The dictTerm usually only cares about names and terms
  const dict = dictTerm(
    decl.impl.name.type,
    forType,
    methods.map((m) => [m.name, m.fnTerm]),
  );

  // 5. Push to worklist with FULL info
  worklist.push({
    impl: {
      traitName: decl.impl.name.type,
      forType,
      traitType,
      dict,
      methods, // Contains { name, term, userDeclaredType }
      typeParams: decl.impl.type_params.map((t) => t.name),
      node: decl,
    },
  });
}

export function elaborateTraitDeclaration(
  decl: TraitDeclaration,
  worklist: WorkItem[],
  // ctx: TypeContextState,
) {
  const name = decl.trait.id.type;
  const typeParams = decl.trait.type_params.map((t) => t.name);
  const fns = decl.trait.fns.map((t) => elaborateTraitFn(t));

  worklist.push({ trait: { name, typeParams, fns } });
}

export function elaborateTraitFn(fn: TraitFn) {
  let fnType = elaborateTypeExpression(fn.return_type);
  for (let i = fn.params.length - 1; i >= 0; i--) {
    const param = fn.params[i]!;
    const paramTy = elaborateTypeExpression(param.ty);
    fnType = arrowType(paramTy, fnType);
  }
  for (let i = fn.type_params.length - 1; i >= 0; i--) {
    const tyParam = fn.type_params[i]!.name;
    fnType = forallType(tyParam, starKind, fnType);
  }
  return {
    name: fn.name.name,
    fn: fnType,
  };
}

export function elaborateTypeDeclaration(
  decl: TypeDeclaration,
  worklist: WorkItem[],
  // ctx: TypeContextState,
) {
  worklist.push({
    type: {
      name: decl.type_dec.id.type,
      params: decl.type_dec.params.map((t) => t.name),
      body: elaborateTypeExpression(decl.type_dec.value),
      typeDecl: decl,
    },
  });
}

export function elaborateLetDeclaration(
  decl: LetDeclaration,
  worklist: WorkItem[],
  ctx: TypeContextState,
) {
  const pattern = elaboratePatternExpression(decl.let_dec.pattern, ctx);
  const term = elaborateExpression(decl.let_dec.value, ctx);
  const assert = decl.let_dec.assert;
  worklist.push({ let: { assert, pattern, term } });
}

export function elaborateEnumDeclaration(
  decl: EnumDeclaration,
  worklist: WorkItem[],
  ctx: TypeContextState,
) {
  // calculate the enum kind
  let enumKind = starKind;
  for (let i = 0; i < decl.enum.type_params.length; i++)
    enumKind = arrowKind(starKind, enumKind);

  const typeParams = decl.enum.type_params.map((t) => t.name);
  const variants = decl.enum.variants.map(elaborateEnumVariantType);

  worklist.push({
    enum_type: {
      enumKind,
      name: decl.enum.id.type,
      node: decl,
      typeParams,
      variants,
    },
  });

  // calculate the enum type
  let enumType = conType(decl.enum.id.type) as Type;
  for (const t of typeParams) enumType = appType(enumType, varType(t));

  // enums in match expressions need a polymorphic wrapped alias type
  let constructorEnumType = enumType;
  for (let i = typeParams.length - 1; i >= 0; i--) {
    constructorEnumType = forallType(
      typeParams[i]!,
      starKind,
      constructorEnumType,
    );
  }

  // calculate the constructor Terms
  for (const variant of decl.enum.variants) {
    let constructorType = enumType;
    const variantName =
      "values" in variant ? variant.values.id.type : variant.fields.id.type;
    ctx.constructors.set(variantName, constructorType);
    if ("values" in variant) {
      for (let i = variant.values.values.length - 1; i >= 0; i--) {
        const ty = variant.values.values[i]!;
        constructorType = arrowType(
          elaborateTypeExpression(ty),
          constructorType,
        );
      }
    } else throw new Error(`Not implemented.`);

    for (let i = typeParams.length - 1; i >= 0; i--) {
      const ty = typeParams[i]!;
      constructorType = forallType(ty, starKind, constructorType);
    }
    worklist.push({
      enum_constructor: {
        constructorType,
        name: variantName,
        node: variant,
        parent: decl,
      },
    });
  }
}

export function elaborateEnumVariantType(decl: EnumVariant) {
  if ("values" in decl) {
    const name = decl.values.id.type;
    const tys = decl.values.values.map(elaborateTypeExpression);
    const ty = tupleType(tys);
    return [name, ty] as [string, Type];
  } else throw new Error(`Not Implemented.`);
}

export function elaborateBuiltinDeclaration(
  decl: BuiltinDeclaration,
  worklist: WorkItem[],
) {
  const name = decl.builtin.alias.name;
  let fnType = elaborateTypeExpression(decl.builtin.return_type);
  for (let i = decl.builtin.params.length - 1; i >= 0; i--) {
    const param = decl.builtin.params[i]!;
    if (!param.guard) throw new Error(`All builtins must have type params.`);
    fnType = arrowType(elaborateTypeExpression(param.guard), fnType);
  }
  for (let i = decl.builtin.type_params.length - 1; i >= 0; i--) {
    const typeParam = decl.builtin.type_params[i]!;
    fnType = forallType(typeParam.name, starKind, fnType);
  }

  worklist.push({
    builtin: {
      fnType,
      name,
      node: decl,
    },
  });
}

export function elaborateFnDeclaration(
  decl: FnDeclaration,
  worklist: WorkItem[],
  ctx: TypeContextState,
) {
  const name = decl.fn.fn.name!.name;
  const [fnType, fnTerm] = elaborateFn(decl.fn.fn, ctx);

  worklist.push({
    fn: {
      fnType,
      name,
      node: decl,
      fnTerm,
    },
  });
}

export function elaborateFn(fn: Fn, ctx: TypeContextState) {
  const paramTypes = fn.params.map((p) =>
    p.guard ? elaborateTypeExpression(p.guard) : freshMetaVar(ctx.meta),
  );

  const retType = fn.return_type
    ? elaborateTypeExpression(fn.return_type)
    : freshMetaVar(ctx.meta);

  // function type is returnType + params + type params
  let fnType = retType;
  for (let i = paramTypes.length - 1; i >= 0; i--) {
    fnType = arrowType(paramTypes[i]!, fnType);
  }
  for (let i = fn.type_params.length - 1; i >= 0; i--) {
    fnType = forallType(fn.type_params[i]!.name, starKind, fnType);
  }

  // function body is body + lam params + type params
  const body = elaborateExpression(fn.body, ctx);
  let fnTerm = body;
  for (let i = paramTypes.length - 1; i >= 0; i--) {
    fnTerm = lamTerm(fn.params[i]!.name.name, paramTypes[i]!, fnTerm);
  }
  for (let i = fn.type_params.length - 1; i >= 0; i--) {
    fnTerm = tylamTerm(fn.type_params[i]!.name, starKind, fnTerm);
  }

  return [fnType, fnTerm] as [Type, Term];
}

export function elaborateExpression(
  expr: Expression,
  ctx: TypeContextState,
): Term {
  if ("block" in expr) return elaborateBlockExpression(expr, ctx);
  else if ("bool" in expr) return elaborateBoolExpression(expr);
  else if ("call" in expr) return elaborateCallExpression(expr, ctx);
  else if ("float" in expr) return elaborateFloatExpression(expr);
  else if ("fn" in expr) return elaborateFnExpression(expr, ctx);
  else if ("if_expr" in expr) return elaborateIfExpression(expr, ctx);
  else if ("int" in expr) return elaborateIntExpression(expr);
  else if ("string" in expr) return elaborateStringExpression(expr);
  else if ("name" in expr) return elaborateNameExpression(expr);
  else if ("tuple" in expr) return elaborateTupleExpression(expr, ctx);
  else if ("record" in expr) return elaborateRecordExpession(expr, ctx);
  else if ("select" in expr) return elaborateSelectExpression(expr, ctx);
  else if ("type" in expr) return elaborateConstructorExpression(expr);
  else if ("match" in expr) return elaborateMatchExpression(expr, ctx);
  else if ("prefix" in expr) return elaboratePrefixExpression(expr, ctx);
  else if ("infix" in expr) return elaborateInfixExpression(expr, ctx);
  else if ("postfix" in expr) return elaboratePostfixExpression(expr, ctx);
  else throw new Error(`Not supported expression: ${JSON.stringify(expr)}`);
}

export function elaboratePrefixExpression(
  _expr: PrefixExpression,
  _ctx: TypeContextState,
): Term {
  throw new Error(`Prefix expressions should be desugard, not elaborated.`);
}

export function elaborateInfixExpression(
  _expr: InfixExpression,
  _ctx: TypeContextState,
): Term {
  throw new Error(`Infix expressions should be desugard, not elaborated.`);
}

export function elaboratePostfixExpression(
  _expr: PostfixExpression,
  _ctx: TypeContextState,
): Term {
  throw new Error(`Infix expressions should be desugard, not elaborated.`);
}

export function elaborateMatchExpression(
  expr: MatchExpression,
  ctx: TypeContextState,
) {
  return matchTerm(
    elaborateExpression(expr.match[0], ctx),
    expr.match[1].map((t) => {
      const pattern = elaboratePatternExpression(t.pattern, ctx);
      const body = elaborateExpression(t.body, ctx);
      return [pattern, body];
    }),
  );
}

export function elaborateConstructorExpression(expr: TypeIdentifier) {
  return varTerm(expr.type);
}

export function elaborateSelectExpression(
  expr: SelectExpression,
  ctx: TypeContextState,
) {
  return projectTerm(
    elaborateExpression(expr.select[0], ctx),
    expr.select[1].name,
  );
}

export function elaborateRecordExpession(
  expr: RecordExpression,
  ctx: TypeContextState,
) {
  return recordTerm(
    expr.record.map((key) => [key[0].name, elaborateExpression(key[1], ctx)]),
  );
}

export function elaborateTupleExpression(
  expr: TupleExpression,
  ctx: TypeContextState,
) {
  return tupleTerm(expr.tuple.map((t) => elaborateExpression(t, ctx)));
}

export function elaborateNameExpression(expr: NameIdentifier) {
  return varTerm(expr.name);
}

export function elaborateStringExpression(expr: StringToken) {
  return conTerm(`"${expr.string}"`, conType("String"));
}

export function elaborateIntExpression(expr: IntExpression) {
  return conTerm(
    `${expr.int.value}`,
    conType(
      expr.int.size < 0
        ? `I${Math.abs(Number(expr.int.size))}`
        : `U${expr.int.size}`,
    ),
  );
}

export function elaborateIfExpression(
  expr: IfExpression,
  ctx: TypeContextState,
) {
  return matchTerm(
    elaborateExpression(expr.if_expr.cond, ctx),
    expr.if_expr.else_body
      ? [
          [
            conPattern("True", conType("Bool")),
            elaborateExpression(expr.if_expr.if_body, ctx),
          ] as [Pattern, Term],
          [
            conPattern("False", conType("Bool")),
            elaborateExpression(expr.if_expr.else_body, ctx),
          ] as [Pattern, Term],
        ]
      : [
          [
            conPattern("True", conType("Bool")),
            elaborateExpression(expr.if_expr.if_body, ctx),
          ] as [Pattern, Term],
        ],
  );
}

export function elaborateFnExpression(
  expr: FnExpression,
  ctx: TypeContextState,
): Term {
  const [, fnTerm] = elaborateFn(expr.fn, ctx);
  return fnTerm;
}

export function elaborateFloatExpression(expr: FloatExpression) {
  return conTerm(expr.float.value.toString(), conType(`F${expr.float.size}`));
}

export function elaborateCallExpression(
  expr: CallExpression,
  ctx: TypeContextState,
) {
  let result = elaborateExpression(expr.call[0], ctx);
  for (const arg of expr.call[1]) {
    result = appTerm(result, elaborateExpression(arg, ctx));
  }
  return result;
}

export function elaborateBoolExpression(expr: BoolExpression) {
  return conTerm(expr.bool ? "True" : "False", conType("Bool"));
}

export function elaborateBlockExpression(
  expr: BlockExpression,
  ctx: TypeContextState,
) {
  if (expr.block.length === 0)
    throw new Error(`Blocks must have an inner expression.`);

  const last = expr.block[expr.block.length - 1]!;

  let resultTerm =
    "expression" in last
      ? elaborateExpression(last.expression, ctx)
      : "let_bind" in last
        ? elaborateExpression(last.let_bind.expression, ctx)
        : null;
  if (!resultTerm)
    throw new Error(
      `Invalid body expression in return position ${showBodyExpression(last)}`,
    );
  for (let i = expr.block.length - 2; i >= 0; i--) {
    const next = expr.block[i]!;
    if ("expression" in next) {
      resultTerm = matchTerm(elaborateExpression(next.expression, ctx), [
        [wildcardPattern(), resultTerm],
      ]);
    } else if ("let_bind" in next) {
      const pattern = elaboratePatternExpression(next.let_bind.pattern, ctx);
      const value = elaborateExpression(next.let_bind.expression, ctx);
      resultTerm = matchTerm(value, [[pattern, resultTerm]]);
    } else
      throw new Error(
        `Invalid body expression in body position ${showBodyExpression(next)}`,
      );
  }

  return resultTerm;
}
export function elaboratePatternExpression(
  node: PatternExpression,
  ctx: TypeContextState,
): Pattern {
  if ("constr" in node) return elaborateConstructorPatternExpression(node, ctx);
  else if ("float" in node) return elaborateFloatPatternExpression(node);
  else if ("int" in node) return elaborateIntPatternExpression(node);
  else if ("tuple" in node) return elaborateTuplePatternExpression(node, ctx);
  else if ("name" in node) return elaborateNamePatternExpression(node);
  else if ("record" in node) return elaborateRecordPatternExpression(node, ctx);
  else if ("string" in node) return elaborateStringPatternExpression(node);
  else
    throw new Error(`Unsupported pattern expression: ${JSON.stringify(node)}`);
}

export function elaborateStringPatternExpression(
  node: StringPatternExpression,
) {
  return conPattern(`"${node.string}"`, conType("String"));
}

export function elaborateRecordPatternExpression(
  node: RecordPatternExpression,
  ctx: TypeContextState,
) {
  return recordPattern(
    node.record.map((t) => [t[0].name, elaboratePatternExpression(t[1], ctx)]),
  );
}

export function elaborateNamePatternExpression(node: NameIdentifier) {
  return varPattern(node.name);
}

export function elaborateTuplePatternExpression(
  node: TuplePatternExpression,
  ctx: TypeContextState,
) {
  return tuplePattern(
    node.tuple.map((t) => elaboratePatternExpression(t, ctx)),
  );
}

export function elaborateIntPatternExpression(node: IntPatternExpression) {
  return conPattern(
    node.int.value.toString(),
    conType(
      node.int.size < 0 ? `I${Math.abs(node.int.size)}` : `U${node.int.size}`,
    ),
  );
}

export function elaborateFloatPatternExpression(node: FloatPatternExpression) {
  return conPattern(
    node.float.value.toString(),
    conType(`F${node.float.size}`),
  );
}

export function elaborateConstructorPatternExpression(
  node: ConstructorPatternExpression,
  ctx: TypeContextState,
) {
  const p = ctx.constructors.get(node.constr.type.type);
  if (!p) throw new Error(`Constructor not found!`);
  return conPattern(node.constr.type.type, p);
}

export function elaborateTypeExpression(node: TypeExpression): Type {
  if ("app" in node) return elaborateApplicationType(node);
  else if ("fn" in node) return elaborateFnType(node);
  else if ("name" in node) return elaborateNamedType(node);
  else if ("record" in node) return elaborateRecordType(node);
  else if ("select" in node) return elaborateSelectType(node);
  else if ("tuple" in node) return elaborateTupleType(node);
  else if ("type" in node) return elaborateTypeNamedType(node);
  else throw new Error(`Invalid Type Expression: ${showTypeExpression(node)}`);
}

export function elaborateApplicationType(node: ApplicationTypeExpression) {
  let ty = elaborateTypeExpression(node.app.callee);
  for (const appTy of node.app.args) {
    ty = appType(ty, elaborateTypeExpression(appTy));
  }
  return ty;
}

export function elaborateFnType(node: FnTypeExpression) {
  let ty = elaborateTypeExpression(node.fn.ret);
  for (let i = node.fn.args.length - 1; i >= 0; i--) {
    ty = arrowType(elaborateTypeExpression(node.fn.args[i]!), ty);
  }
  return ty;
}

export function elaborateNamedType(node: NameIdentifier) {
  return varType(node.name);
}

export function elaborateRecordType(node: RecordTypeExpression) {
  const tys = [] as [string, Type][];
  for (const { name, ty } of node.record) {
    tys.push([name.name, elaborateTypeExpression(ty)]);
  }
  return recordType(tys);
}

export function elaborateSelectType(node: SelectTypeExpression): Type {
  throw new Error("Not Implemented.");
}

export function elaborateTupleType(node: TupleTypeExpression) {
  const tys = [] as Type[];
  for (const ty of node.tuple) {
    tys.push(elaborateTypeExpression(ty));
  }
  return tupleType(tys);
}

export function elaborateTypeNamedType(node: TypeIdentifier) {
  return conType(node.type);
}
