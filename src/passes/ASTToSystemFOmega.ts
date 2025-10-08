// passes/ASTToSystemFOmega.ts
import type {
  TypeExpression,
  Expression,
  PatternExpression,
  EnumDeclaration,
  TypeDeclaration,
} from "../parser.js";
import type {
  Type,
  Term,
  Pattern,
  Kind,
  VariantType,
} from "../types_system_f_omega.js";
import {
  var_type,
  arrow_type,
  forall_type,
  app_type,
  con_type,
  record_type,
  variant_type,
  tuple_type,
  mu_type,
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
  tuple_term,
  tuple_project_term,
  var_pattern,
  wildcard_pattern,
  con_pattern,
  record_pattern,
  variant_pattern,
  tuple_pattern,
  showType,
} from "../types_system_f_omega.js";
import type { Result } from "../util.js";
export type EnumInfo = {
  name: string;
  params: string[];
  variants: Map<
    string,
    {
      payloadTypes: TypeExpression[];
    }
  >;
  enumType?: Type; // Cached enum type
  constructorTypes?: Map<string, Type>; // Cache constructor types
};

export class ASTToSystemFOmega {
  // Track which types we're currently converting (to detect recursion)
  private typeStack: Set<string> = new Set();

  // Track enum definitions separately
  private enumDefinitions = new Map<string, EnumInfo>();

  // Cache for type definitions (enum/type declarations)
  private typeDefinitions = new Map<
    string,
    {
      params: string[];
      body: TypeExpression;
      isRecursive: boolean;
    }
  >();

  // Track type aliases
  private typeAliases = new Map<
    string,
    {
      params: string[];
      body: TypeExpression;
      computedType?: Type;
    }
  >();

  // Register a type definition before converting
  registerTypeDeclaration(node: TypeDeclaration) {
    const name = node.type_dec.id.type;
    const params = node.type_dec.params?.map((p) => p.name) ?? [];

    this.typeDefinitions.set(name, {
      params,
      body: node.type_dec.value,
      isRecursive: false, // Will be detected during conversion
    });
  }

  registerEnumDeclaration(node: EnumDeclaration) {
    const name = node.enum.id.type;
    const params = node.enum.type_params?.map((p) => p.name) ?? [];

    const variants = new Map<
      string,
      {
        payloadTypes: TypeExpression[];
      }
    >();

    for (const variant of node.enum.variants) {
      if ("fields" in variant) {
        // Named fields: Variant { x: Int, y: String }
        const variantName = variant.fields.id.type;
        const payloadTypes = variant.fields.fields.map((f) => f.ty);
        variants.set(variantName, {
          payloadTypes,
        });
      } else {
        // Positional values: Variant(Int, String)
        const variantName = variant.values.id.type;
        const payloadTypes = variant.values.values;
        variants.set(variantName, {
          payloadTypes,
        });
      }
    }

    this.enumDefinitions.set(name, {
      name,
      params,
      variants,
    });
  }

  // Get constructor type - computes and caches on first access
  getConstructorType(
    enumName: string,
    variantName: string,
    typeArgs: Type[] = [],
  ): Result<string, Type> {
    const enumInfo = this.enumDefinitions.get(enumName);
    if (!enumInfo) return { err: `Unknown enum: ${enumName}` };

    const variantInfo = enumInfo.variants.get(variantName);
    if (!variantInfo)
      return { err: `Unknown variant: ${variantName} in enum ${enumName}` };

    // Create cache key for this specific instantiation
    const cacheKey =
      typeArgs.length > 0
        ? `${variantName}<${typeArgs.map(showType).join(",")}>`
        : variantName;

    // Check cache
    if (!enumInfo.constructorTypes) {
      enumInfo.constructorTypes = new Map();
    }

    const cached = enumInfo.constructorTypes.get(cacheKey);
    if (cached) return { ok: cached };

    // Compute constructor type
    const constructorType = this.computeConstructorType(
      enumName,
      variantName,
      typeArgs,
    );
    if ("err" in constructorType) return constructorType;
    // Cache it
    enumInfo.constructorTypes.set(cacheKey, constructorType.ok);

    return constructorType;
  }

  // Convert type with recursion tracking
  convertType(
    node: TypeExpression,
    context: string[] = [],
  ): Result<string, Type> {
    // Type identifier or name identifier
    if ("type" in node) {
      const typeName = node.type;

      // Check if this is a type parameter (from context)
      if (context.includes(typeName)) {
        return { ok: var_type(typeName) };
      }

      // Check if we're in the middle of converting this type (recursion!)
      if (this.typeStack.has(typeName)) {
        // We found a recursive reference - use a type variable
        return { ok: var_type(typeName) };
      }

      // Check if this is a defined type
      const def = this.typeDefinitions.get(typeName);
      if (def) {
        // Mark that we're converting this type
        this.typeStack.add(typeName);

        // Convert the body with type parameters in context
        const newContext = [...context, ...def.params];
        const bodyType = this.convertType(def.body, newContext);
        if ("err" in bodyType) return bodyType;
        this.typeStack.delete(typeName);

        // Check if the body contains a reference to this type
        if (this.typeContainsVar(bodyType.ok, typeName)) {
          // This is a recursive type - wrap in mu
          def.isRecursive = true;
          return { ok: mu_type(typeName, bodyType.ok) };
        }

        return bodyType;
      }

      // Otherwise it's a base type
      return { ok: con_type(typeName) };
    }

    if ("name" in node) {
      const name = node.name;

      // Check if it's a type parameter
      if (context.includes(name)) {
        return { ok: var_type(name) };
      }

      // Check for recursion
      if (this.typeStack.has(name)) {
        return { ok: var_type(name) };
      }

      // Check if it's a defined type
      const def = this.typeDefinitions.get(name);
      if (def) {
        this.typeStack.add(name);
        const newContext = [...context, ...def.params];
        const bodyType = this.convertType(def.body, newContext);
        if ("err" in bodyType) return bodyType;

        this.typeStack.delete(name);

        if (this.typeContainsVar(bodyType.ok, name)) {
          def.isRecursive = true;
          return { ok: mu_type(name, bodyType.ok) };
        }

        return bodyType;
      }

      return { ok: var_type(name) };
    }

    // Select type (e.g., Module.Type)
    if ("select" in node) {
      // For now, just convert the name part
      // In a full implementation, resolve the module
      return this.convertType(node.select.name, context);
    }

    // Application type (e.g., List<Int>)
    if ("app" in node) {
      const func = this.convertType(node.app.callee, context);
      if ("err" in func) return func;

      let result = func.ok;
      for (const arg of node.app.args) {
        const param = this.convertType(arg, context);
        if ("err" in param) return param;
        result = app_type(result, param.ok);
      }
      return { ok: result };
    }

    // Function type
    if ("fn" in node) {
      const args = [] as Type[];
      for (const a of node.fn.args) {
        const arg = this.convertType(a, context);
        if ("err" in arg) return arg;
        args.push(arg.ok);
      }
      const ret = this.convertType(node.fn.ret, context);
      if ("err" in ret) return ret;

      let result = ret.ok;
      for (let i = args.length - 1; i >= 0; i--) {
        result = arrow_type(args[i]!, result);
      }
      return { ok: result };
    }

    // Record type
    if ("record" in node) {
      const fields: [string, Type][] = [];
      for (const field of node.record) {
        const result = this.convertType(field.ty, context);
        if ("err" in result) return result;
        fields.push([field.name.name, result.ok]);
      }

      return { ok: record_type(fields) };
    }

    // Tuple type
    if ("tuple" in node) {
      const fields = [] as Type[];
      for (const field of node.tuple) {
        const result = this.convertType(field, context);
        if ("err" in result) return result;
        fields.push(result.ok);
      }
      return { ok: tuple_type(fields) };
    }

    return { err: `Unknown type expression: ${JSON.stringify(node)}` };
  }

  // Compute the actual enum type (variant type wrapped in μ if recursive)
  private computeEnumType(
    enumInfo: EnumInfo,
    typeArgs: Type[] = [],
  ): Result<string, Type> {
    if (enumInfo.enumType && typeArgs.length === 0) {
      return { ok: enumInfo.enumType };
    }

    const name = enumInfo.name;
    const params = enumInfo.params;

    // Build context with type parameters
    const context = [...params];

    // Build variant type from constructors
    const variantCases: [string, Type][] = [];

    for (const [variantName, variantInfo] of enumInfo.variants) {
      const payloadTypes = variantInfo.payloadTypes;

      let payloadType: Type;
      if (payloadTypes.length === 0) {
        // Unit variant (no payload)
        payloadType = con_type("Unit");
      } else if (payloadTypes.length === 1) {
        const single = this.convertType(payloadTypes[0]!, context);
        if ("err" in single) return single;
        // Single payload
        payloadType = single.ok;
      } else {
        const results = [] as Type[];
        for (const pt of payloadTypes) {
          const result = this.convertType(pt, context);
          if ("err" in result) return result;
          results.push(result.ok);
        }
        // Multiple payloads -> tuple
        payloadType = tuple_type(results);
      }

      variantCases.push([variantName, payloadType]);
    }

    let enumType: Type = variant_type(variantCases);

    // Check if this is recursive
    this.typeStack.add(name);
    const isRecursive = this.typeContainsVar(enumType, name);
    this.typeStack.delete(name);

    if (isRecursive) {
      enumType = mu_type(name, enumType);
    }

    // If no type arguments, cache it
    if (typeArgs.length === 0) {
      enumInfo.enumType = enumType;
    }

    // Apply type arguments if present
    if (typeArgs.length > 0 && params.length > 0) {
      // Substitute type parameters with actual arguments
      const subst = new Map<string, Type>();
      for (let i = 0; i < Math.min(params.length, typeArgs.length); i++) {
        subst.set(params[i]!, typeArgs[i]!);
      }
      enumType = this.substituteInType(enumType, subst);
    }

    return { ok: enumType };
  }

  private substituteInType(type: Type, subst: Map<string, Type>): Type {
    if ("var" in type) {
      return subst.get(type.var) ?? type;
    }

    if ("arrow" in type) {
      return arrow_type(
        this.substituteInType(type.arrow.from, subst),
        this.substituteInType(type.arrow.to, subst),
      );
    }

    if ("app" in type) {
      return app_type(
        this.substituteInType(type.app.func, subst),
        this.substituteInType(type.app.arg, subst),
      );
    }

    if ("record" in type) {
      return record_type(
        type.record.map(
          ([label, t]) =>
            [label, this.substituteInType(t, subst)] as [string, Type],
        ),
      );
    }

    if ("variant" in type) {
      return variant_type(
        type.variant.map(
          ([label, t]) =>
            [label, this.substituteInType(t, subst)] as [string, Type],
        ),
      );
    }

    if ("tuple" in type) {
      return tuple_type(type.tuple.map((t) => this.substituteInType(t, subst)));
    }

    if ("forall" in type) {
      const newSubst = new Map(subst);
      newSubst.delete(type.forall.var);
      return forall_type(
        type.forall.var,
        type.forall.kind,
        this.substituteInType(type.forall.body, newSubst),
      );
    }

    if ("mu" in type) {
      const newSubst = new Map(subst);
      newSubst.delete(type.mu.var);
      return mu_type(
        type.mu.var,
        this.substituteInType(type.mu.body, newSubst),
      );
    }

    return type;
  }

  // Compute constructor type for a variant
  private computeConstructorType(
    enumName: string,
    variantName: string,
    typeArgs: Type[] = [],
  ): Result<string, Type> {
    const enumInfo = this.enumDefinitions.get(enumName);
    if (!enumInfo) return { err: `Unknown enum: ${enumName}` };

    const variantInfo = enumInfo.variants.get(variantName);
    if (!variantInfo)
      return { err: `Unknown variant: ${variantName} in enum ${enumName}` };

    // Get the enum type
    const enumType = this.computeEnumType(enumInfo, typeArgs);
    if ("err" in enumType) return enumType;

    // Build constructor type
    const context = enumInfo.params;
    const payloadTypes = [] as Type[];

    for (const pt of variantInfo.payloadTypes) {
      const result = this.convertType(pt, context);
      if ("err" in result) return result;
      payloadTypes.push(result.ok);
    }

    // Apply type arguments to payload types if present
    if (typeArgs.length > 0) {
      const subst = new Map<string, Type>();
      for (let i = 0; i < Math.min(context.length, typeArgs.length); i++) {
        subst.set(context[i]!, typeArgs[i]!);
      }
      for (let i = 0; i < payloadTypes.length; i++) {
        payloadTypes[i] = this.substituteInType(payloadTypes[i]!, subst);
      }
    }

    if (payloadTypes.length === 0) {
      // Unit constructor: EnumName
      return enumType;
    } else {
      // Function constructor: payload -> EnumName
      // For multiple payloads: (A, B, C) -> EnumName becomes A -> B -> C -> EnumName
      let result = enumType.ok;
      for (let i = payloadTypes.length - 1; i >= 0; i--) {
        result = arrow_type(payloadTypes[i]!, result);
      }
      return { ok: result };
    }
  }

  // Check if a type contains a variable (for detecting recursion)
  private typeContainsVar(type: Type, varName: string): boolean {
    if ("var" in type) {
      return type.var === varName;
    }

    if ("arrow" in type) {
      return (
        this.typeContainsVar(type.arrow.from, varName) ||
        this.typeContainsVar(type.arrow.to, varName)
      );
    }

    if ("forall" in type) {
      // Don't look inside if the variable is shadowed
      if (type.forall.var === varName) return false;
      return this.typeContainsVar(type.forall.body, varName);
    }

    if ("app" in type) {
      return (
        this.typeContainsVar(type.app.func, varName) ||
        this.typeContainsVar(type.app.arg, varName)
      );
    }

    if ("lam" in type) {
      if (type.lam.var === varName) return false;
      return this.typeContainsVar(type.lam.body, varName);
    }

    if ("record" in type) {
      return type.record.some(([_, fieldType]) =>
        this.typeContainsVar(fieldType, varName),
      );
    }

    if ("variant" in type) {
      return type.variant.some(([_, caseType]) =>
        this.typeContainsVar(caseType, varName),
      );
    }

    if ("tuple" in type) {
      return type.tuple.some((t) => this.typeContainsVar(t, varName));
    }

    if ("mu" in type) {
      if (type.mu.var === varName) return false;
      return this.typeContainsVar(type.mu.body, varName);
    }

    return false;
  }

  // Convert AST expression to System F-omega term
  convertTerm(node: Expression): Result<string, Term> {
    // Variable reference
    if ("name" in node) {
      return { ok: var_term(node.name) };
    }

    // Constructor
    if ("constr" in node) {
      const constructorName = node.constr;

      // Find which enum this constructor belongs to
      for (const [enumName, enumInfo] of this.enumDefinitions) {
        if (enumInfo.variants.has(constructorName)) {
          const variantInfo = enumInfo.variants.get(constructorName)!;

          if (variantInfo.payloadTypes.length === 0) {
            // Unit constructor - it's the value itself
            const enumType = this.computeEnumType(enumInfo);
            if ("err" in enumType) return enumType;
            return {
              ok: inject_term(
                constructorName,
                con_term("()", con_type("Unit")),
                enumType.ok,
              ),
            };
          } else {
            // Constructor function - return as a term with its type
            // Get constructor type WITHOUT type arguments (polymorphic)
            const constructorType = this.getConstructorType(
              enumName,
              constructorName,
            );
            if ("err" in constructorType) return constructorType;
            return { ok: con_term(constructorName, constructorType.ok) };
          }
        }
      }

      return { err: `Unknown constructor: ${constructorName}` };
    }

    // Function call
    if ("call" in node) {
      const callee = node.call[0];
      const args = node.call[1];

      if ("constr" in callee) {
        const constructorName = callee.constr;
        const constructorInfo = this.lookupConstructor(constructorName);
        if ("err" in constructorInfo) return constructorInfo;

        const enumType = this.computeEnumType(
          this.enumDefinitions.get(constructorInfo.ok.enumName)!,
        );
        if ("err" in enumType) return enumType;

        // Convert arguments
        const convertedArgs = [] as Term[];
        for (const a of args) {
          const result = this.convertTerm(a);
          if ("err" in result) return result;
          convertedArgs.push(result.ok);
        }

        // Build payload
        let payload: Term;
        if (convertedArgs.length === 1) {
          payload = convertedArgs[0]!;
        } else {
          payload = tuple_term(convertedArgs);
        }

        return { ok: inject_term(constructorName, payload, enumType.ok) };
      }

      // Regular call
      const calleeTerm = this.convertTerm(callee);
      if ("err" in calleeTerm) return calleeTerm;
      let result = calleeTerm.ok;

      for (const arg of args) {
        const argResult = this.convertTerm(arg);
        if ("err" in argResult) return argResult;
        result = app_term(result, argResult.ok);
      }

      return { ok: result };
    }

    // Block expression
    if ("block" in node) {
      // Convert block to nested let bindings
      let result: Term | null = null;

      for (const stmt of node.block) {
        if ("let_bind" in stmt) {
          const value = this.convertTerm(stmt.let_bind.expression);
          if ("err" in value) return value;
          // This is simplified - you'd need to handle patterns properly
          if (result === null) {
            result = value.ok;
          } else {
            // Create a let binding (you might need to extend your Term type)
            // For now, we'll sequence expressions
            result = value.ok;
          }
        } else if ("expression" in stmt) {
          const term = this.convertTerm(stmt.expression);
          if ("err" in term) return term;
          result = term.ok;
        }
      }

      return { ok: result ?? var_term("unit") };
    }

    // If expression
    if ("if_expr" in node) {
      const cond = this.convertTerm(node.if_expr.cond);
      if ("err" in cond) return cond;
      const ifBody = this.convertTerm(node.if_expr.if_body);
      if ("err" in ifBody) return ifBody;
      const elseBody = node.if_expr.else_body
        ? this.convertTerm(node.if_expr.else_body)
        : { ok: con_term("()", con_type("Unit")) };
      if ("err" in elseBody) return elseBody;

      // Encode as match on boolean
      return {
        ok: match_term(cond.ok, [
          [con_pattern("true", con_type("Bool")), ifBody.ok],
          [con_pattern("false", con_type("Bool")), elseBody.ok],
        ]),
      };
    }

    // Field selection (e.g., obj.field)
    if ("select" in node) {
      const record = this.convertTerm(node.select[0]);
      if ("err" in record) return record;
      const label = node.select[1].name;
      return { ok: project_term(record.ok, label) };
    }

    // Match expression
    if ("match" in node) {
      const scrutinee = this.convertTerm(node.match[0]);
      if ("err" in scrutinee) return scrutinee;
      const cases = [] as [Pattern, Term][];
      for (const arm of node.match[1]) {
        const pattern = this.convertPattern(arm.pattern);
        if ("err" in pattern) return pattern;
        const body = this.convertTerm(arm.body);
        if ("err" in body) return body;
        cases.push([pattern.ok, body.ok]);
      }
      return { ok: match_term(scrutinee.ok, cases) };
    }

    // Literals
    if ("int" in node) {
      return {
        ok: con_term(
          node.int.value.toString(),
          con_type(`Int${node.int.size.toString()}`),
        ),
      };
    }

    if ("float" in node) {
      return {
        ok: con_term(
          node.float.value.toString(),
          con_type(`Float${node.float.size.toString()}`),
        ),
      };
    }

    if ("bool" in node) {
      return { ok: con_term(node.bool.toString(), con_type("Bool")) };
    }

    if ("string" in node) {
      return { ok: con_term(node.string, con_type("String")) };
    }

    // Lambda expression
    if ("fn" in node) {
      const params = node.fn.params;
      const body = this.convertTerm(node.fn.body);
      if ("err" in body) return body;

      // Build nested lambdas: fn(x, y) => body becomes λx.λy.body
      let result = body.ok;
      for (let i = params.length - 1; i >= 0; i--) {
        const param = params[i]!;
        const paramType = param.guard
          ? this.convertType(param.guard)
          : { ok: var_type("$infer") }; // Will be inferred
        if ("err" in paramType) return paramType;

        result = lam_term(param.name.name, paramType.ok, result);
      }

      return { ok: result };
    }

    // Record literal
    if ("record" in node) {
      const fields = [] as [string, Term][];

      for (const [name, expr] of node.record) {
        const term = this.convertTerm(expr);
        if ("err" in term) return term;
        fields.push([name.name, term.ok]);
      }

      return { ok: record_term(fields) };
    }

    // Tuple literal
    if ("tuple" in node) {
      const terms = [] as Term[];
      for (const e of node.tuple) {
        const term = this.convertTerm(e);
        if ("err" in term) return term;
        terms.push(term.ok);
      }
      return { ok: tuple_term(terms) };
    }

    // Prefix/postfix/infix operators
    if ("prefix" in node) {
      const op = node.prefix.op;
      const operand = this.convertTerm(node.prefix.operand);
      if ("err" in operand) return operand;

      return { ok: app_term(var_term(op), operand.ok) };
    }

    if ("postfix" in node) {
      const op = node.postfix.op;
      const operand = this.convertTerm(node.postfix.operand);
      if ("err" in operand) return operand;

      return { ok: app_term(var_term(op), operand.ok) };
    }

    if ("infix" in node) {
      const op = node.infix.op;
      const left = this.convertTerm(node.infix.left);
      if ("err" in left) return left;
      const right = this.convertTerm(node.infix.right);
      if ("err" in right) return right;
      return { ok: app_term(app_term(var_term(op), left.ok), right.ok) };
    }

    // Self expression
    if ("self" in node) {
      return { ok: var_term("self") };
    }

    return { err: `Unknown expression: ${JSON.stringify(node)}` };
  }

  // Convert AST pattern to System F-omega pattern
  convertPattern(node: PatternExpression): Result<string, Pattern> {
    // Variable pattern
    if ("name" in node) {
      if (node.name === "_") {
        return { ok: wildcard_pattern() };
      }

      return { ok: var_pattern(node.name) };
    }

    // Constructor pattern
    if ("constr" in node) {
      const name = node.constr[0].type;
      const args = [] as Pattern[];

      for (const a of node.constr[1]) {
        const arg = this.convertPattern(a);
        if ("err" in arg) return arg;
        args.push(arg.ok);
      }

      // For now, create a variant pattern
      // In a complete implementation, you'd resolve the constructor
      if (args.length === 0) {
        return { ok: variant_pattern(name, wildcard_pattern()) };
      } else if (args.length === 1) {
        return { ok: variant_pattern(name, args[0]!) };
      } else {
        return { ok: variant_pattern(name, tuple_pattern(args)) };
      }
    }

    // Literal patterns
    if ("int" in node) {
      return {
        ok: con_pattern(
          node.int.value.toString(),
          con_type(`Int${node.int.size.toString()}`),
        ),
      };
    }

    if ("float" in node) {
      return {
        ok: con_pattern(node.float.value.toString(), con_type("Float")),
      };
    }

    if ("string" in node) {
      return { ok: con_pattern(node.string, con_type("String")) };
    }

    // Record pattern
    if ("record" in node) {
      const fields = [] as [string, Pattern][];
      for (const [name, pat] of node.record) {
        const patResult = this.convertPattern(pat);
        if ("err" in patResult) return patResult;
        fields.push([name.name, patResult.ok]);
      }

      return { ok: record_pattern(fields) };
    }

    // Tuple pattern
    if ("tuple" in node) {
      const tuple = [] as Pattern[];
      for (const p of node.tuple) {
        const result = this.convertPattern(p);
        if ("err" in result) return result;
        tuple.push(result.ok);
      }
      return { ok: tuple_pattern(tuple) };
    }

    return { err: `Unknown pattern: ${JSON.stringify(node)}` };
  }

  lookupConstructor(
    name: string,
  ): Result<string, { enumName: string; type: Type }> {
    for (const [enumName, enumInfo] of this.enumDefinitions) {
      if (enumInfo.variants.has(name)) {
        const type = this.getConstructorType(enumName, name);
        if ("err" in type) return type;
        return { ok: { enumName, type: type.ok } };
      }
    }
    return { err: `Constructor '${name}' not found.` };
  }
}
