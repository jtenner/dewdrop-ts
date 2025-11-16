import {
  applySubstitution,
  appTerm,
  appType,
  arrowKind,
  arrowType,
  conTerm,
  conType,
  dictTerm,
  foldTerm,
  forallType,
  freshMetaVar,
  injectTerm,
  lamTerm,
  lamType,
  letTerm,
  matchTerm,
  muType,
  neverType,
  type Pattern,
  recordTerm,
  recordType,
  starKind,
  type Term,
  type Type,
  traitImplBinding,
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
  traitDefBinding,
  enumDefBinding,
  normalizeType,
  substituteType,
  typeAliasBinding,
  type Binding,
} from "system-f-omega";
import type { NameToken, StringToken, TypeToken } from "../lexer.js";
import {
  type ApplicationTypeExpression,
  type BlockExpression,
  type BuiltinDeclaration,
  type CallExpression,
  type ConstructorImport,
  type ConstructorPatternExpression,
  type Declaration,
  type EnumDeclaration,
  type EnumVariant,
  type Expression,
  type ExpressionBodyExpression,
  type Fn,
  type FnDeclaration,
  type FnExpression,
  type FnTypeExpression,
  type ImplDeclaration,
  type Import,
  type IntExpression,
  type LetBindBodyExpression,
  type LetDeclaration,
  type MatchExpression,
  type PatternExpression,
  type SelfExpression,
  showBodyExpression,
  showDeclaration,
  showEnumVariant,
  showExpression,
  showFn,
  showImport,
  showPatternExpression,
  showTypeExpression,
  type TraitDeclaration,
  type TupleExpression,
  type TupleTypeExpression,
  type TypeDeclaration,
  type TypeExpression,
  type TypeImport,
} from "../parser.js";
import { BaseWalker } from "../visitor.js";

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

  override walkFnDeclaration(node: FnDeclaration): void {
    super.walkFnDeclaration(node);
    const term = this.context.terms.get(node.fn.fn);
    if (!term) throw new Error(`Term not elaborated for ${showFn(node.fn.fn)}`);
    const type = this.context.types.get(node.fn.fn);
    if (!type) throw new Error(`Type not elaborated for ${showFn(node.fn.fn)}`);

    this.context.elaborated.set(node, {
      fn_decl: {
        fn: node,
        term,
        type,
      },
    });
  }

  override walkLetDeclaration(node: LetDeclaration): void {
    super.walkLetDeclaration(node);
    const pattern = this.context.patterns.get(node.let_dec.pattern);
    if (!pattern)
      throw new Error(
        `Pattern not elaborated for: ${showPatternExpression(node.let_dec.pattern)}`,
      );
    const term = this.context.terms.get(node.let_dec.value);
    if (!term)
      throw new Error(
        `Term not elaborated for : ${showExpression(node.let_dec.value)}`,
      );

    this.context.elaborated.set(node, { let_bind: { pattern, term } });
  }

  override walkTraitDeclaration(node: TraitDeclaration): void {
    super.walkTraitDeclaration(node);

    let kind = starKind;

    for (let i = node.trait.type_params.length - 1; i >= 0; i--) {
      kind = arrowKind(starKind, kind);
    }

    const methods = [] as [string, Type][];

    for (const fn of node.trait.fns) {
      // start with the return type and build up the function def for each param
      let fnTy = this.context.types.get(fn.return_type);
      if (!fnTy)
        throw new Error(
          `Return type not elaborated for trait def: ${showTypeExpression(fn.return_type)}`,
        );

      for (let i = fn.params.length - 1; i >= 0; i--) {
        const param = fn.params[i]!;
        const paramTy = this.context.types.get(param.ty);
        if (!paramTy)
          throw new Error(
            `Param type not elaborated for trait def: ${showTypeExpression(param.ty)}`,
          );
        fnTy = arrowType(paramTy, fnTy);
      }

      // build a type parameter for each trait type param
      for (let i = node.trait.type_params.length - 1; i >= 0; i--) {
        const param = node.trait.type_params[i]!;
        fnTy = forallType(param.name, starKind, fnTy);
      }

      methods.push([fn.name.name, fnTy]);
      this.context.types.set(fn, fnTy);
    }

    const trait_def = traitDefBinding(
      node.trait.id.type,
      "Self",
      starKind,
      methods,
    );
    this.context.elaborated.set(node, { trait_def });
  }

  override walkImplDeclaration(node: ImplDeclaration): void {
    super.walkImplDeclaration(node);
    const implFor = this.context.types.get(node.impl.for);
    if (!implFor)
      throw new Error(
        `Trait impl for type expression not elaborated: ${showTypeExpression(node.impl.for)}`,
      );

    const methods = [] as [string, Term][];
    const implTerm = this.lookupType(node, node.impl.name.type);
    if (!implTerm) {
      this.errors.push({
        unbound: {
          name: node.impl.name.type,
          scope: this.getScope(node)!,
        },
      });
      return;
    }

    if (!("trait" in implTerm)) {
      this.errors.push({
        invalid_name: node,
      });
      return;
    }

    const traitTypeParams = implTerm.trait.trait.type_params;
    if (traitTypeParams.length !== node.impl.trait_params.length) {
      this.errors.push({
        type_parameter_count: {
          node,
          actual: node.impl.trait_params.length,
          expected: traitTypeParams.length,
        },
      });
      return;
    }

    // build a trait substitution
    const traitSubstitution = new Map<string, Type>();
    for (let i = 0; i < traitTypeParams.length; i++) {
      const paramName = traitTypeParams[i]!.name;
      const paramTy = this.context.types.get(node.impl.trait_params[i]!);
      if (!paramTy)
        throw new Error(
          `Trait param type not elaborated for: ${showTypeExpression(node.impl.trait_params[i]!)}`,
        );
      traitSubstitution.set(paramName, paramTy);
    }

    for (const fn of node.impl.fns) {
      const fnTy = this.context.types.get(fn);
      if (!fnTy)
        throw new Error(`Function Type not elaborated for fn ${showFn(fn)}`);
      const methodTy = applySubstitution(traitSubstitution, fnTy);
      this.context.types.set(fn, methodTy);

      const fnTerm = this.context.terms.get(fn);
      if (!fnTerm)
        throw new Error(`Function term not elaborated for ${showFn(fn)}`);

      // Need to wait for type checking
      methods.push([fn.name!.name, fnTerm]);
    }

    let methodDict = dictTerm(node.impl.name.type, implFor, methods);
    for (let i = node.impl.type_params.length - 1; i >= 0; i--) {
      const p = node.impl.type_params[i]!;
      methodDict = tylamTerm(p.name, starKind, methodDict);
    }

    const trait_impl = traitImplBinding(
      node.impl.name.type,
      implFor,
      methodDict,
    );

    this.context.elaborated.set(node, { trait_impl });
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
    this.context.elaborated.set(node, { type: acc });
  }

  override walkImport(node: Import): void {
    super.walkImport(node);
    const elaborated =
      this.context.terms.has(node) ||
      this.context.types.has(node) ||
      this.context.bindings.has(node);

    if (!elaborated)
      throw new Error(
        `Term or type not elaborated for import: ${showImport(node)}`,
      );
  }

  override walkTypeImport(node: TypeImport): void {
    const element = this.lookupType(node, node.type.name.type);
    if (!element) {
      this.errors.push({
        unbound: { name: node.type.name.type, scope: this.getScope(node)! },
      });
    } else {
      if ("enum" in element) {
        const elaboratedImport = this.context.types.get(element.enum);
        if (!elaboratedImport)
          throw new Error(`Enum not elaborated for import ${showImport(node)}`);
        this.context.types.set(node, elaboratedImport);
      } else if ("trait" in element) {
        const elaboratedImport = this.context.bindings.get(element.trait);
        if (!elaboratedImport)
          throw new Error(
            `Bindings for import not elaborated ${showImport(node)}`,
          );
        this.context.bindings.set(node, elaboratedImport);
      } else if ("type" in element) {
        const elaboratedImport = this.context.types.get(element.type);
        if (!elaboratedImport)
          throw new Error(`Type not elaborated for import ${showImport(node)}`);
        this.context.types.set(node, elaboratedImport);
      } else
        throw new Error(`Invalid import at this point: ${showImport(node)}`);
    }
  }

  override walkConstructorImport(node: ConstructorImport): void {
    const element = this.lookupTerm(node, node.constr.name.type);
    if (!element)
      this.errors.push({
        unbound: { name: node.constr.name.type, scope: this.getScope(node)! },
      });
    else {
      if ("variant" in element) {
        const elaboratedImport = this.context.terms.get(element.variant);
        if (!elaboratedImport)
          throw new Error(
            `Constructor not elaborated for import: ${showImport(node)}`,
          );
        this.context.terms.set(node, elaboratedImport);
      } else
        throw new Error(`Invalid import at this point: ${showImport(node)}`);
    }
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

    let fnType = node.return_type
      ? this.context.types.get(node.return_type)
      : (freshMetaVar() as Type);
    if (!fnType)
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

      fnTerm = lamTerm(name.name, guardType, fnTerm);
      fnType = arrowType(guardType, fnType);
    }

    for (let i = node.type_params.length - 1; i >= 0; i--) {
      const typeParam = node.type_params[i]!.name;
      fnTerm = tylamTerm(typeParam, starKind, fnTerm);
      fnType = forallType(typeParam, starKind, fnType);
    }

    this.context.terms.set(node, fnTerm);
    this.context.types.set(node, fnType);
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

    for (let i = node.builtin.type_params.length - 1; i >= 0; i--) {
      const typeParam = node.builtin.type_params[i]!;
      acc = forallType(typeParam.name, starKind, acc);
    }

    this.context.elaborated.set(node, { builtin: { type: acc } });
  }

  override walkString(node: StringToken): void {
    this.context.terms.set(node, conTerm(node.string, conType("String")));
  }

  override walkEnumDeclaration(node: EnumDeclaration): void {
    super.walkEnumDeclaration(node);

    const { id, recursive, type_params, variants } = node.enum;
    const enumName = id.type;
    const variantDefs: [string, [Term, Type]][] = [];

    // Kind: * → * → ... → *
    let kind = starKind;
    for (let i = 0; i < type_params.length; i++)
      kind = arrowKind(starKind, kind);

    const paramNames = type_params.map((t) => t.name);

    // Field schemes
    const enumVariants: [string, Type][] = variants.map((v) => {
      const label = "fields" in v ? v.fields.id.type : v.values.id.type;
      const fieldScheme = this.context.types.get(v);
      if (!fieldScheme)
        throw new Error(
          `Type not elaborated for variant: ${showEnumVariant(v)}`,
        );
      return [label, fieldScheme];
    });

    const binding = enumDefBinding(enumName, kind, paramNames, enumVariants);
    let alias: Binding | null = null;

    // Precompute µ type + body for recursive enums
    let mu: Type | null = null;
    let variantBody: Type | null = null;

    if (recursive) {
      variantBody = variantType(enumVariants); // <Label: FieldScheme | ...>
      mu = muType(id.type, variantBody); // μList. <...>
      alias = typeAliasBinding(
        enumName,
        paramNames,
        paramNames.map(() => starKind),
        mu,
      );
    }

    for (const variantNode of variants) {
      const label =
        "fields" in variantNode
          ? variantNode.fields.id.type
          : variantNode.values.id.type;

      const fieldScheme = this.context.types.get(variantNode);
      if (!fieldScheme)
        throw new Error(
          `Type not elaborated for variant: ${showEnumVariant(variantNode)}`,
        );

      const def = this.buildEnumVariantConstructor(
        enumName,
        type_params,
        label,
        fieldScheme,
        recursive,
        mu,
        variantBody,
      );

      variantDefs.push([label, def]);
    }

    this.context.elaborated.set(node, {
      enum: { binding, alias, variantDefs },
    });
  }

  // Helper: build a term-level constructor for an enum variant.
  private buildEnumVariantConstructor(
    enumName: string,
    typeParams: NameToken[],
    variantLabel: string,
    fieldScheme: Type,
    recursive: boolean,
    muTypeDef: Type | null,
    variantBodyDef: Type | null,
  ): [Term, Type] {
    // Build external enum type: EnumName<tp0, tp1, ...>
    // This is the name/type the surface language sees.
    let enumApp: Type = conType(enumName);
    for (const tp of typeParams) {
      enumApp = appType(enumApp, varType(tp.name));
    }

    const payloadName = "payload";
    let body: Term;

    if (!recursive) {
      // Non-recursive: inject directly into the enum application;
      // downstream normalizeType will expand aliases/enums as needed.
      body = injectTerm(variantLabel, varTerm(payloadName), enumApp);
    } else {
      if (!muTypeDef || !variantBodyDef) {
        throw new Error(
          `Internal error: recursive enum ${enumName} missing µ definition`,
        );
      }

      // Instantiate µ and variant body with actual type parameters, if any.
      // Here we assume fieldScheme & enumVariants are already written in
      // terms of the enum's type params; enumApp is the instantiated outer type.

      // Example:
      //   muTypeDef      = μList. <Nil: (), Cons: (T, List)>
      //   enumApp        = List<T>
      //   variantBodyDef = <Nil: () | Cons: (T, List)>
      //
      // For the constructor, we:
      //   1) instantiate the variant body with params (that's already done
      //      in enumVariants/field schemes, so variantBodyDef is fine as-is),
      //   2) inject into the instantiated variant body,
      //   3) fold to the instantiated µ type (enumApp).

      const injected = injectTerm(
        variantLabel,
        varTerm(payloadName),
        variantBodyDef,
      );

      body = foldTerm(enumApp, injected);
    }

    let term: Term = lamTerm(payloadName, fieldScheme, body);

    // Quantify over type parameters
    for (let i = typeParams.length - 1; i >= 0; i--) {
      const tp = typeParams[i]!;
      term = tylamTerm(tp.name, starKind, term);
    }

    // Type: ∀tp. fieldScheme → EnumName<tp...>
    let ctorType: Type = arrowType(fieldScheme, enumApp);
    for (let i = typeParams.length - 1; i >= 0; i--) {
      const tp = typeParams[i]!;
      ctorType = forallType(tp.name, starKind, ctorType);
    }

    return [term, ctorType];
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
      else this.context.types.set(node, conType(node.type));
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

  override walkDeclaration(node: Declaration): void {
    super.walkDeclaration(node);

    const elaborated = this.context.elaborated.has(node);
    if (!elaborated)
      throw new Error(
        `Term or type not elaborated for declaration: ${showDeclaration(node)}`,
      );
  }
}
