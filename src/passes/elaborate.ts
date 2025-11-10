import {
  appTerm,
  appType,
  conTerm,
  conType,
  forallType,
  freshMetaVar,
  injectTerm,
  lamTerm,
  lamType,
  letTerm,
  matchTerm,
  muType,
  neverType,
  recordTerm,
  recordType,
  starKind,
  tuplePattern,
  tupleTerm,
  tupleType,
  tylamTerm,
  unitValue,
  variantPattern,
  variantType,
  varPattern,
  varTerm,
  varType,
  wildcardPattern,
  type Pattern,
  type Term,
  type Type,
} from "system-f-omega";
import {
  showBodyExpression,
  showEnumVariant,
  showExpression,
  showFn,
  showPatternExpression,
  showTypeExpression,
  type ApplicationTypeExpression,
  type BlockExpression,
  type BuiltinDeclaration,
  type CallExpression,
  type ConstructorPatternExpression,
  type EnumDeclaration,
  type EnumVariant,
  type Expression,
  type ExpressionBodyExpression,
  type Fn,
  type FnExpression,
  type FnTypeExpression,
  type IntExpression,
  type LetBindBodyExpression,
  type MatchExpression,
  type PatternExpression,
  type SelfExpression,
  type TupleExpression,
  type TupleTypeExpression,
  type TypeDeclaration,
  type TypeExpression,
} from "../parser.js";
import { BaseWalker } from "../visitor.js";
import type { NameToken, TypeToken } from "../lexer.js";

export class ElaboratePass extends BaseWalker {
  override walkExpression(node: Expression): void {
    super.walkExpression(node);
    if (!this.context.terms.has(node))
      throw new Error(`Node not elaborated: ${showExpression(node)}`);
  }

  override walkPatternExpression(node: PatternExpression): void {
    super.walkPatternExpression(node);
    if (!this.context.patterns.has(node))
      throw new Error(`Pattern not elaborated: ${showPatternExpression(node)}`);
  }

  override walkTypeExpression(node: TypeExpression): void {
    super.walkTypeExpression(node);
    if (!this.context.types.has(node))
      throw new Error(`Type not elaborated: ${showTypeExpression(node)}`);
  }

  override walkTypeDeclaration(node: TypeDeclaration): void {
    super.walkTypeDeclaration(node);
    let acc = this.context.types.get(node.type_dec.value);
    if (!acc)
      throw new Error(
        `Type not elaborated for TypeExpression: ${showTypeExpression(node.type_dec.value)}`,
      );

    // wrap the type in it's polymorphic form params
    for (let i = node.type_dec.params.length - 1; i >= 0; i--) {
      const name = node.type_dec.params[i]!.name;
      acc = forallType(name, starKind, acc);
    }
    this.context.types.set(node, acc);
  }

  override walkIntExpression(node: IntExpression): void {
    this.context.terms.set(
      node,
      conTerm(
        node.int.value.toString(),
        conType(
          node.int.size < 0
            ? `I${Math.abs(node.int.size)}`
            : `U${node.int.size}`,
        ),
      ),
    );
  }

  override walkSelfExpression(node: SelfExpression): void {
    this.context.terms.set(node, varType("self"));
  }

  override walkFn(node: Fn): void {
    super.walkFn(node);

    let bodyType = node.return_type
      ? this.context.types.get(node.return_type)
      : (freshMetaVar() as Type);
    if (!bodyType)
      throw new Error(
        `Return Type not elaborated: ${showTypeExpression(node.return_type!)}`,
      );
    let fnTerm = this.context.terms.get(node.body);
    if (!fnTerm)
      throw new Error(
        `Function body not elaborated: ${showExpression(node.body)}`,
      );

    // function parameters
    for (let i = node.params.length - 1; i >= 0; i--) {
      const { guard, name } = node.params[i]!;
      const guardType = guard ? this.context.types.get(guard) : freshMetaVar();

      if (!guardType)
        throw new Error(
          `Function Parameter Type Guard not elaborated: ${showTypeExpression(guard!)}`,
        );

      fnTerm = lamTerm(name.name, bodyType, fnTerm);
      bodyType = lamType(name.name, starKind, bodyType);
    }

    this.context.terms.set(node, fnTerm);
  }

  override walkFnExpression(node: FnExpression): void {
    super.walkFnExpression(node);
    const fn = this.context.terms.get(node.fn);
    if (!fn)
      throw new Error(`Function Expression not elaborated: ${showFn(node.fn)}`);
    this.context.terms.set(node, fn);
  }

  override walkFnTypeExpression(node: FnTypeExpression): void {
    super.walkFnTypeExpression(node);
    let fnType = this.context.types.get(node.fn.ret);
    if (!fnType)
      throw new Error(
        `Type not elaborated: ${showTypeExpression(node.fn.ret)}`,
      );

    for (let i = node.fn.args.length - 1; i >= 0; i--) {
      const argTy = this.context.types.get(node.fn.args[i]!);
      if (!argTy)
        throw new Error(
          `Type not elaborated: ${showTypeExpression(node.fn.args[i]!)}`,
        );
      fnType = lamType(`p${i}`, starKind, argTy);
    }

    this.context.types.set(node, fnType);
  }

  override walkBuiltinDeclaration(node: BuiltinDeclaration): void {
    super.walkBuiltinDeclaration(node);
    // build the polymorphic builtin type
    let acc = this.context.types.get(node.builtin.return_type);
    if (!acc)
      throw new Error(
        `Type not elaborated: ${showTypeExpression(node.builtin.return_type)}`,
      );

    // the function type itself has all the parameters
    for (let i = node.builtin.params.length - 1; i >= 0; i--) {
      const param = node.builtin.params[i]!;
      const paramType = param.guard
        ? this.context.types.get(param.guard)
        : freshMetaVar();

      if (!paramType)
        throw new Error(
          `Type not elaborated: ${showTypeExpression(param.guard!)}`,
        );

      acc = lamType(param.name.name, starKind, paramType);
    }

    // TODO: Maybe add type parameters here

    this.context.types.set(node, acc);
  }

  override walkEnumDeclaration(node: EnumDeclaration): void {
    super.walkEnumDeclaration(node);
    // generate the enumType
    const variants = [] as [string, Type][];
    for (const v of node.enum.variants) {
      const vty = this.context.types.get(v);
      if (!vty)
        throw new Error(
          `Enum Variant type not elaborated: ${showEnumVariant(v)}`,
        );
      variants.push(["fields" in v ? v.fields.id.type : v.values.id.type, vty]);
    }

    let enumType = variantType(variants) as Type;
    for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
      enumType = forallType(node.enum.type_params[i]!.name, starKind, enumType);
    }
    if (node.enum.recursive) enumType = muType(node.enum.id.type, enumType);

    // Generate a constructor function for each enum variant
    for (const v of node.enum.variants) {
      const vname = "fields" in v ? v.fields.id.type : v.values.id.type;
      // each enum variant created it's own function body at this point
      let body = this.context.terms.get(v);
      let vtype: Type;
      if (!body)
        throw new Error(
          `Enum Variant term body not elaborated: ${showEnumVariant(v)}`,
        );
      body = injectTerm(vname, body, enumType);

      // wrap in parameters backwards
      if ("fields" in v) {
        const fields = [] as [string, Type][];
        for (let i = v.fields.fields.length - 1; i >= 0; i--) {
          const field = v.fields.fields[i]!;
          const fieldName = field.name.name;
          const fieldType = this.context.types.get(field.ty);
          if (!fieldType)
            throw new Error(
              `Field type not elaborated: ${showTypeExpression(field.ty)}`,
            );
          fields.unshift([fieldName, fieldType]);
          body = lamTerm(fieldName, fieldType, body);
        }
        vtype = recordType(fields);
      } else {
        const fields = [] as Type[];
        for (let i = v.values.values.length - 1; i >= 0; i--) {
          const fieldName = `p${i}`;
          const fieldType = this.context.types.get(v.values.values[i]!);
          if (!fieldType)
            throw new Error(
              `Field type not elaborated: ${showTypeExpression(v.values.values[i]!)}`,
            );
          body = lamTerm(fieldName, fieldType, body);
          fields.unshift(fieldType);
        }
        vtype = tupleType(fields);
      }

      // wrap in type parameters and possibly a fold
      for (let i = node.enum.type_params.length - 1; i >= 0; i--) {
        const typeName = node.enum.type_params[i]!.name;
        body = tylamTerm(typeName, starKind, body);
      }

      this.context.types.set(v, vtype);
      this.context.terms.set(v, body);
    }

    this.context.types.set(node, enumType);
  }

  override walkEnumVariant(node: EnumVariant): void {
    super.walkEnumVariant(node);
    let valueType: Type;
    let valueTerm: Term;
    if ("fields" in node) {
      const types = [] as [string, Type][];
      const terms = [] as [string, Term][];
      for (const { name, ty } of node.fields.fields) {
        const fieldType = this.context.types.get(ty);
        if (!fieldType)
          throw new Error(
            `Field Type not elaborated: ${showTypeExpression(ty)}`,
          );
        types.push([name.name, fieldType]);
        terms.push([name.name, varTerm(name.name)]);
      }
      valueType = recordType(types);
      valueTerm = recordTerm(terms);
    } else {
      const types = [] as Type[];
      const terms = [] as Term[];
      for (let i = 0; i < node.values.values.length; i++) {
        const ty = node.values.values[i]!;
        const fieldType = this.context.types.get(ty);
        if (!fieldType)
          throw new Error(
            `Field Type not elaborated: ${showTypeExpression(ty)}`,
          );
        types.push(fieldType);
        terms.push(varTerm(`p${i}`));
      }
      valueType = tupleType(types);
      valueTerm = tupleTerm(terms);
    }
    this.context.types.set(node, valueType);
    this.context.terms.set(node, valueTerm);
  }

  override walkTupleTypeExpression(node: TupleTypeExpression): void {
    super.walkTupleTypeExpression(node);
    const elements = [] as Type[];
    for (const tupleElement of node.tuple) {
      const tupleElementType = this.context.types.get(tupleElement);
      if (!tupleElementType)
        throw new Error(
          `Type not elaborated: ${showTypeExpression(tupleElement)}`,
        );
      elements.push(tupleElementType);
    }
    const t = tupleType(elements);
    this.context.types.set(node, t);
  }

  override walkTupleExpression(node: TupleExpression): void {
    super.walkTupleExpression(node);
    const elements = [] as Term[];
    for (const tupleElement of node.tuple) {
      const tupleElementTerm = this.context.terms.get(tupleElement);
      if (!tupleElementTerm)
        throw new Error(`Term not elaborated ${showExpression(tupleElement)}`);
      elements.push(tupleElementTerm);
    }
    const t = tupleTerm(elements);
    this.context.terms.set(node, t);
  }

  override walkCallExpression(node: CallExpression): void {
    super.walkCallExpression(node);
    const calleeTerm = this.context.terms.get(node.call[0]);
    if (!calleeTerm)
      throw new Error(`Term not elaborated ${showExpression(node.call[0])}`);
    const args = [] as Term[];
    for (const arg of node.call[1]) {
      const argTerm = this.context.terms.get(arg);
      if (!argTerm)
        throw new Error(`Term not elaborated ${showExpression(arg)}`);
      args.push(argTerm);
    }

    let root = calleeTerm;
    for (const arg of args) {
      root = appTerm(root, arg);
    }

    this.context.terms.set(node, root);
  }

  override walkMatchExpression(node: MatchExpression): void {
    super.walkMatchExpression(node);
    const scrutinee = this.context.terms.get(node.match[0]);
    if (!scrutinee)
      throw new Error(
        `Expression not elaborated: ${showExpression(node.match[0])}`,
      );

    const arms = [] as [Pattern, Term][];

    for (const { body, pattern } of node.match[1]) {
      const p = this.context.patterns.get(pattern);
      if (!p)
        throw new Error(
          `Pattern not elaborated: ${showPatternExpression(pattern)}`,
        );
      const b = this.context.terms.get(body);
      if (!b)
        throw new Error(`Expression not elaborated: ${showExpression(body)}`);
      arms.push([p, b]);
    }
    this.context.terms.set(node, matchTerm(scrutinee, arms));
  }

  override walkExpressionBodyExpression(node: ExpressionBodyExpression): void {
    super.walkExpressionBodyExpression(node);
    const nodeTerm = this.context.terms.get(node.expression);
    if (!nodeTerm)
      throw new Error(
        `Expression not elaborated for term: ${showExpression(node.expression)}`,
      );
    this.context.terms.set(node, nodeTerm);
  }

  override walkBlockExpression(node: BlockExpression): void {
    super.walkBlockExpression(node);
    if (node.block.length === 0)
      throw new Error(`Invalid block expression (length 0)`);
    const last = node.block[node.block.length - 1]!;
    let acc = this.context.terms.get(last);
    if (!acc)
      throw new Error(
        `Term not elaborated for body expression: ${showBodyExpression(last)}`,
      );

    for (let i = node.block.length - 2; i >= 0; i--) {
      const blockExpr = node.block[i]!;

      const term = this.context.terms.get(blockExpr);
      if (!term)
        throw new Error(
          `Term not elaborated for body expression: ${showBodyExpression(blockExpr)}`,
        );
      if ("let_bind" in blockExpr) {
        const pattern = this.context.patterns.get(blockExpr);
        if (!pattern)
          throw new Error(
            `Pattern not elaborated for body expression: ${showBodyExpression(blockExpr)}`,
          );
        acc = matchTerm(
          term,
          blockExpr.let_bind.assert
            ? [
                [pattern, acc],
                [wildcardPattern(), appTerm(varTerm("unreachable"), unitValue)],
              ]
            : [[pattern, acc]],
        );
      } else if ("expression" in blockExpr) {
        acc = matchTerm(term, [[wildcardPattern(), acc]]);
      } else
        throw new Error(
          `Invalid body expression in this position. Did you use the desugar pass?`,
        );
      acc = letTerm("_", term, acc);
    }
    this.context.terms.set(node, acc);
  }

  override walkLetBindBodyExpression(node: LetBindBodyExpression): void {
    super.walkLetBindBodyExpression(node);
    const bindExpression = this.context.terms.get(node.let_bind.expression);
    if (!bindExpression)
      throw new Error(
        `Let Expression not elaborated: ${showExpression(node.let_bind.expression)}`,
      );

    const bindPattern = this.context.patterns.get(node.let_bind.pattern);
    if (!bindPattern)
      throw new Error(
        `Let Pattern not elaborated: ${showPatternExpression(node.let_bind.pattern)}`,
      );
    this.context.terms.set(node, bindExpression);
    this.context.patterns.set(node, bindPattern);
  }

  override walkApplicationTypeExpression(
    node: ApplicationTypeExpression,
  ): void {
    super.walkApplicationTypeExpression(node);
    const args = [] as Type[];
    for (const arg of node.app.args) {
      const argType = this.context.types.get(arg);
      if (!argType)
        throw new Error(`Type not elaborated: ${showTypeExpression(arg)}`);
      args.push(argType);
    }
    let root = this.context.types.get(node.app.callee);
    if (!root)
      throw new Error(
        `Type not elaborated ${showTypeExpression(node.app.callee)}`,
      );

    for (const arg of args) root = appType(root, arg);
    this.context.types.set(node, root);
  }

  override walkNameIdentifier(node: NameToken): void {
    if (this.idMode === "pattern")
      this.context.patterns.set(node, varPattern(node.name));
    else if (this.idMode === "type")
      this.context.types.set(node, varType(node.name));
    else if (this.idMode === "expression")
      this.context.terms.set(node, varTerm(node.name));
  }

  override walkTypeIdentifier(node: TypeToken): void {
    if (this.idMode === "type") {
      if (node.type === "Never") this.context.types.set(node, neverType);
      else this.context.types.set(node, varType(node.type));
    } else if (this.idMode === "expression")
      this.context.terms.set(node, varTerm(node.type));
  }

  override walkConstructorPatternExpression(
    node: ConstructorPatternExpression,
  ): void {
    super.walkConstructorPatternExpression(node);
    const args = [] as Pattern[];
    for (const arg of node.constr.patterns) {
      const argPattern = this.context.patterns.get(arg);
      if (!argPattern)
        throw new Error(`Pattern not elaborated ${showPatternExpression(arg)}`);
      args.push(argPattern);
    }

    this.context.patterns.set(
      node,
      variantPattern(node.constr.type.type, tuplePattern(args)),
    );
  }
}
