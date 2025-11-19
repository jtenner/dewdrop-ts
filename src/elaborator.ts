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
} from "system-f-omega";
import {
  type ApplicationTypeExpression,
  type BlockExpression,
  type BoolExpression,
  type BuiltinDeclaration,
  type CallExpression,
  type Declaration,
  type EnumDeclaration,
  type EnumVariant,
  type Expression,
  type FloatExpression,
  type Fn,
  type FnDeclaration,
  type FnExpression,
  type FnTypeExpression,
  type IfExpression,
  type InfixExpression,
  type IntExpression,
  type MatchExpression,
  type Module,
  type NameIdentifier,
  type PatternExpression,
  type RecordExpression,
  type RecordTypeExpression,
  type SelectExpression,
  type SelectTypeExpression,
  showBodyExpression,
  showExpression,
  showTypeExpression,
  type TupleExpression,
  type TupleTypeExpression,
  type TypeExpression,
  type TypeIdentifier,
} from "./parser.js";
import type { WorkItem } from "./util.js";
import type { StringToken } from "./lexer.js";

type TypeContextState = ReturnType<typeof state>;

export function elaborate(module: Module) {
  const worklist = [] as WorkItem[];
  const ctx = state();
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
  else if ("enum" in decl) elaborateEnumDeclaration(decl, worklist);
  else if ("fn" in decl) elaborateFnDeclaration(decl, worklist, ctx);
}

export function elaborateEnumDeclaration(
  decl: EnumDeclaration,
  worklist: WorkItem[],
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

  // calculate the constructor Terms
  for (const variant of decl.enum.variants) {
    let constructorType = enumType;
    const variantName =
      "values" in variant ? variant.values.id.type : variant.fields.id.type;
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
  // else if ("infix" in expr) return elaborateInfixExpression(expr, ctx);
}

export function elaborateMatchExpression(
  expr: MatchExpression,
  ctx: TypeContextState,
) {
  return matchTerm(
    elaborateExpression(expr.match[0], ctx),
    expr.match[1].map((t) => {
      const pattern = elaboratePatternExpression(t.pattern);
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
      const pattern = elaboratePatternExpression(next.let_bind.pattern);
      const value = elaborateExpression(next.let_bind.expression, ctx);
      resultTerm = matchTerm(value, [[pattern, resultTerm]]);
    } else
      throw new Error(
        `Invalid body expression in body position ${showBodyExpression(next)}`,
      );
  }

  return resultTerm;
}
export function elaboratePatternExpression(node: PatternExpression): Pattern {}

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
