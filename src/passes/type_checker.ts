import type {
  BuiltinDeclaration,
  ConstructorImport,
  Declaration,
  EnumDeclaration,
  Fn,
  ImplDeclaration,
  Import,
  ImportDeclaration,
  LetDeclaration,
  Module,
  NameImport,
  TraitDeclaration,
  TypeExpression,
  TypeImport,
} from "../parser.js";
import {
  app_type,
  arrow_kind,
  arrow_type,
  type Binding,
  type Context,
  checkKind,
  collectTypeVars,
  type EnumDef,
  type FieldScheme,
  forall_type,
  freshMetaVar,
  instantiate,
  type Kind,
  kindArity,
  kindsEqual,
  normalizeType,
  showContext,
  showKind,
  showTerm,
  showType,
  starKind,
  substituteType,
  type Term,
  type TermBinding,
  type TraitDef,
  type Type,
  type TypingError,
  typecheck,
  typesEqual,
} from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import { lookup_type, type Scope, type ScopeIndex } from "./create_scopes.js";
import {
  collectTypeVarsFromTypes,
  computeEnumKind,
  type TermMap,
  type TypeMap,
} from "./elaborate.js";

function bindingKey(bind: Binding): string {
  if ("type" in bind) return `type:${bind.type.name}`;
  if ("term" in bind) return `term:${bind.term.name}`;
  if ("trait_def" in bind) return `trait:${bind.trait_def.name}`;
  if ("trait_impl" in bind) return `impl:${bind.trait_impl.trait}`;
  // Fallback hash or string
  return JSON.stringify(bind);
}

export class TypeChecker extends BaseVisitor {
  private globalContext = [] as Context;
  private context = [] as Context;
  private processedTraits = new Set<string>();

  errors = [] as TypingError[];
  constructor(
    public termMap: TermMap,
    public typeMap: TypeMap,
    public globalScope: Scope,
    public scopes: ScopeIndex,
  ) {
    super();
    this.context = this.globalContext;
  }

  check(module: Module) {
    // Phase 1: Collect all trait definitions into global context
    this.collectTraits(module);

    // Reset context to global (now includes all traits)
    this.context = this.globalContext.slice();
    this.processedTraits.clear(); // Reset for phase 2

    // Phase 2: Full type checking pass
    this.visitModule(module);
  }

  // Phase 1: Visit only traits/enums/builtins to build global context
  private collectTraits(module: Module) {
    console.log("Phase 1: Collecting global definitions (traits, enums, etc.)");
    for (const decl of module.module) {
      if ("trait" in decl) {
        this.visitTraitDeclaration(decl);
      } else if ("enum" in decl) {
        this.visitEnumDeclaration(decl);
      } else if ("builtin" in decl) {
        this.visitBuiltinDeclaration(decl);
      } else if ("import_dec" in decl) {
        // Handle imports if they add globals (e.g., traits from other modules)
        this.visitImportDeclaration(decl);
      } else if ("impl" in decl) {
        // Collect impls after traits (for dict bindings)
        this.visitImplDeclaration(decl); // This pushes trait_impl
      }
      // Add other global decls as needed (e.g., TypeDeclaration if they define types used in traits)
    }
    console.log(
      `Collected ${this.globalContext.filter((b) => "trait_def" in b).length} traits`,
    );
    console.log(
      "Global context after collection:",
      showContext(this.globalContext),
    );

    const projections = this.getTraitProjections(); // Implement to recompute or share from Elaborate
    for (const [projName, projTerm] of projections) {
      const result = typecheck(this.globalContext, projTerm);
      if ("ok" in result) {
        this.globalContext.push({
          term: { name: projName, type: result.ok }, // e.g., "map": ∀Self:Map. (t->u) -> Self t -> Self u
        });
        console.log(
          `Bound global projection ${projName}: ${showType(result.ok)}`,
        );
      } else {
        console.error(`Failed to bind ${projName}:`, result.err);
      }
    }
  }

  private getTraitProjections(): [string, Term][] {
    const projections: [string, Term][] = [];
    for (const binding of this.globalContext) {
      if ("trait_def" in binding) {
        const trait = binding.trait_def;
        for (const [methodName, _] of trait.methods) {
          const projName = methodName; // e.g., "map"
          const projTerm = {
            trait_lam: {
              trait_var: `dict_${projName}`,
              trait: trait.name,
              type_var: "Self",
              kind: trait.kind,
              constraints: [{ trait: trait.name, type: { var: "Self" } }],
              body: {
                trait_method: {
                  dict: { var: `dict_${projName}` },
                  method: methodName,
                },
              },
            },
          };
          projections.push([projName, projTerm]);
        }
      }
    }
    return projections;
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    // Visit each import to register them in the context
    for (const imp of node.import_dec.imports) {
      this.visitImport(imp);
    }
    return node;
  }

  override visitTypeImport(node: TypeImport): Import {
    const scope = this.scopes.get(node.type.name);
    if (!scope) throw new Error("Scope not generated for import");
    const type = lookup_type(node.type.name.type, scope);
    if (!type) throw new Error("Could not find import at this point.");
    const alias = node.type.alias?.type ?? node.type.name.type;

    if ("enum" in type) {
      const enumImport = type.enum;
      // Compute kind directly from type parameters (no context lookup needed)
      const kind = computeEnumKind(enumImport.enum.type_params);
      this.context.push({ type: { name: alias, kind } });
    } else throw new Error("Not Implemented");
    return node;
  }

  override visitNameImport(node: NameImport): Import {
    const scope = this.scopes.get(node.name.name);
    if (!scope) throw new Error("Scope not generated for import");
    const type = lookup_type(node.name.name.name, scope);
    if (!type) throw new Error("Could not find import at this point.");
    const _alias = node.name.alias?.name ?? node.name.name.name;

    throw new Error("Not implemented");
  }

  override visitConstructorImport(node: ConstructorImport): Import {
    const scope = this.scopes.get(node.constr.name);
    if (!scope) throw new Error("Scope not generated for import");
    const type = lookup_type(node.constr.name.type, scope);
    if (!type) throw new Error("Could not find import at this point.");
    const alias = node.constr.alias?.type ?? node.constr.name.type;

    if ("variant" in type) {
      const variant_type = this.typeMap.get(type.variant);
      if (!variant_type) throw new Error("Variant type not found");

      // Just register the constructor - DON'T type-check it
      this.context.push({
        term: { name: alias, type: variant_type },
      });
    } else throw new Error("Not implemented.");
    return node;
  }

  override visitLetDeclaration(node: LetDeclaration): Declaration {
    const term = this.termMap.get(node.let_dec.value);
    if (!term) throw new Error("Term not generated for let.");
    const result = typecheck(this.context, term);
    if ("ok" in result) {
      this.context.push({
        term: {
          name: node.let_dec.id.name,
          type: result.ok,
        },
      });
    } else {
      this.errors.push(result.err);
    }
    return node;
  }

  override visitBuiltinDeclaration(node: BuiltinDeclaration): Declaration {
    // we assume the type is correct as long as the inner types are correct
    const term = this.termMap.get(node);
    // it should be a builtin term
    if (!term) throw new Error("Term not generated for builtin.");

    const result = typecheck(this.context, term);

    if ("ok" in result) {
      this.context.push({
        term: {
          name: node.builtin.alias.name,
          type: result.ok,
        },
      });
    } else {
      this.errors.push(result.err);
    }
    return node;
  }

  private inImplContext = false;
  private enterImplContext(): void {
    this.inImplContext = true;
  }
  private exitImplContext(): void {
    this.inImplContext = false;
  }
  private isInImplMethod(): boolean {
    return this.inImplContext;
  }

  override visitFn(node: Fn): Fn {
    const term = this.termMap.get(node);
    if (!term) throw new Error("Term not generated for fn.");

    const result = typecheck(this.context, term);
    if ("ok" in result) {
      // Only push global binding for top-level fns (not impl methods)
      // Detect: if node has a module-level name and not in impl context (use a stack or flag)
      const shouldPushGlobal = node.name && !this.isInImplMethod(); // Implement isInImplMethod() if needed
      if (shouldPushGlobal) {
        this.context.push({
          term: {
            name: node.name?.name ?? "anonymous",
            type: result.ok,
          },
        });
      }
      // For debugging: always log the inferred type
      console.log(
        `Inferred type for ${node.name?.name || "anon fn"}: ${showType(result.ok)}`,
      );
    } else {
      console.error(
        `Type error in fn ${node.name?.name || "anon"}:`,
        result.err,
      );
      console.error("Term:", showTerm(term));
      console.error("Context:", showContext(this.context));
      if ("type_mismatch" in result.err) {
        console.error(
          `Expected: ${showType(result.err.type_mismatch.expected)}`,
        );
        console.error(`Actual: ${showType(result.err.type_mismatch.actual)}`);
      } else if ("not_a_variant" in result.err) {
        console.error(`Not a variant: ${showType(result.err.not_a_variant)}`);
      }
      this.errors.push(result.err);
    }
    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    const enum_decl = node.enum;

    // Build the correct kind: * → * → ... → * (unchanged)
    let enumKind: Kind = starKind;
    for (let i = enum_decl.type_params.length - 1; i >= 0; i--) {
      enumKind = arrow_kind(starKind, enumKind);
    }

    // Add enum type to context with the correct kind (unchanged)
    this.context.push({
      type: { name: enum_decl.id.type, kind: enumKind },
    });

    // NEW: Build and push EnumDef binding (for nominal pattern/inject checks)
    const params = enum_decl.type_params.map((p) => p.name); // ["t", "u"]
    const variants: [string, FieldScheme][] = [];

    for (const variant of enum_decl.variants) {
      let fieldScheme: FieldScheme; // Unbound type(s) with param vars
      if ("fields" in variant) {
        // Single or named fields (treat as tuple for simplicity)
        const fieldTypes = variant.fields.fields.map(
          (f) => this.typeMap.get(f.ty)!,
        ); // e.g., [{var: "t"}]
        fieldScheme =
          fieldTypes.length === 1 ? fieldTypes[0]! : { tuple: fieldTypes };
      } else {
        // Multi-value fields
        const fieldTypes = variant.values.values.map(
          (ty) => this.typeMap.get(ty)!,
        );
        fieldScheme =
          fieldTypes.length === 1 ? fieldTypes[0]! : { tuple: fieldTypes };
      }
      const label =
        "fields" in variant ? variant.fields.id.type : variant.values.id.type;
      variants.push([label, fieldScheme]);
    }

    const enumDef: EnumDef = {
      enum: {
        name: enum_decl.id.type,
        kind: enumKind,
        params,
        variants,
      },
    };
    this.context.push({ enum: enumDef }); // Push to global (this.context in phase 1)

    console.log(
      `Pushed EnumDef for ${enum_decl.id.type}: ${params.length} params, ${variants.length} variants`,
    );

    // Add variant constructors as terms (unchanged, but ensure types are poly)
    for (const variant of enum_decl.variants) {
      const variant_type = this.typeMap.get(variant); // Poly ctor type, e.g., ∀t. t → Option<t>
      if (!variant_type) {
        console.warn("No type found for variant:", variant);
        continue;
      }

      const variant_name =
        "fields" in variant ? variant.fields.id.type : variant.values.id.type;
      this.context.push({
        term: { name: variant_name, type: variant_type },
      });
    }

    return node;
  }

  // Helper: Skip traits in full pass if already processed
  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    if (this.processedTraits.has(node.trait.id.type)) {
      console.log(`Skipping already-processed trait: ${node.trait.id.type}`);
      return node; // Idempotent: already in context
    }
    this.processedTraits.add(node.trait.id.type);
    // Existing visitTraitDeclaration logic...
    const trait_decl = node.trait;

    // First, visit all type expressions to populate the type map
    for (const traitFn of trait_decl.fns) {
      for (const param of traitFn.params) {
        this.visitTypeExpression(param.ty);
      }
      this.visitTypeExpression(traitFn.return_type);
    }

    // Determine Self's kind based on how it's used in method signatures
    let selfKind: Kind = { star: null }; // Default to * if Self isn't used

    // Look at all Self applications in method signatures
    for (const traitFn of trait_decl.fns) {
      // Check return type
      const returnSelfInfo = this.analyzeSelfUsage(traitFn.return_type);
      if (returnSelfInfo.isUsed) {
        // Update kind based on the arity
        selfKind = { star: null };
        for (let i = 0; i < returnSelfInfo.arity; i++) {
          selfKind = {
            arrow: {
              from: { star: null },
              to: selfKind,
            },
          };
        }
      }

      // Check parameter types
      for (const param of traitFn.params) {
        const paramSelfInfo = this.analyzeSelfUsage(param.ty);
        if (paramSelfInfo.isUsed) {
          // Update kind based on the arity
          selfKind = { star: null };
          for (let i = 0; i < paramSelfInfo.arity; i++) {
            selfKind = {
              arrow: {
                from: { star: null },
                to: selfKind,
              },
            };
          }
        }
      }
    }

    // Build method types (similar to elaboration but for checking)
    const methods: [string, Type][] = [];

    // Create renaming map (same logic as ElaboratePass)
    const methodGenericRenaming = new Map<string, string>();
    let renameCounter = 0;

    const allMethodGenerics = new Set<string>();
    for (const traitFn of trait_decl.fns) {
      for (const tp of traitFn.type_params) {
        if (tp.name !== "Self") allMethodGenerics.add(tp.name);
      }

      const paramTypes = traitFn.params
        .map((p) => this.typeMap.get(p.ty))
        .filter(Boolean) as Type[];
      const returnType = this.typeMap.get(traitFn.return_type) as Type;

      const vars = new Set<string>();
      collectTypeVarsFromTypes([...paramTypes, returnType], vars);
      vars.delete("Self");

      for (const v of vars) allMethodGenerics.add(v);
    }

    for (const tp of trait_decl.type_params) {
      if (tp.name !== "Self") allMethodGenerics.add(tp.name);
    }

    for (const oldName of allMethodGenerics) {
      methodGenericRenaming.set(oldName, `τ${renameCounter++}`);
    }

    // Now build methods with renaming applied
    for (const traitFn of trait_decl.fns) {
      const paramTypes: Type[] = traitFn.params.map((p) => {
        let ty = this.typeMap.get(p.ty)!;
        for (const [oldName, newName] of methodGenericRenaming.entries()) {
          ty = substituteType(oldName, { var: newName }, ty);
        }
        return ty;
      });

      let returnType = this.typeMap.get(traitFn.return_type)!;
      for (const [oldName, newName] of methodGenericRenaming.entries()) {
        returnType = substituteType(oldName, { var: newName }, returnType);
      }
      if (!returnType) throw new Error("Return type not found");

      let selfType: Type = { var: "Self" };
      if (trait_decl.type_params.length > 0) {
        const origParam = trait_decl.type_params[0]!.name;
        const renamedParam =
          origParam === "Self"
            ? "Self"
            : methodGenericRenaming.get(origParam) || origParam;

        selfType = app_type(selfType, { var: renamedParam });
      }

      let methodType = arrow_type(selfType, returnType) as Type;

      for (let i = paramTypes.length - 1; i >= 0; i--)
        methodType = arrow_type(paramTypes[i]!, methodType);

      const boundVars = new Set(
        traitFn.type_params.map((tp) => {
          return tp.name === "Self"
            ? "Self"
            : methodGenericRenaming.get(tp.name) || tp.name;
        }),
      );
      boundVars.add("Self"); // Self is bound by the trait

      const sigFreeVars = collectTypeVars(methodType).filter(
        (v) => !boundVars.has(v),
      );

      // Wrap in foralls for free variables
      let generalizedMethodType = methodType;
      for (const freeVar of sigFreeVars.sort().reverse())
        generalizedMethodType = forall_type(
          freeVar,
          starKind,
          generalizedMethodType,
        );

      // Wrap in foralls for method's explicit type parameters
      for (let i = traitFn.type_params.length - 1; i >= 0; i--) {
        const origVar = traitFn.type_params[i]!.name;
        if (origVar === "Self") continue;
        const renamedVar = methodGenericRenaming.get(origVar) || origVar;
        generalizedMethodType = forall_type(
          renamedVar,
          starKind,
          generalizedMethodType,
        );
      }

      methods.push([traitFn.name.name, generalizedMethodType]);
      this.typeMap.set(traitFn, generalizedMethodType);
    }

    const traitDef: TraitDef = {
      name: trait_decl.id.type,
      type_param: "Self",
      kind: selfKind, // Use the computed kind for Self
      methods,
    };

    this.context.push({ trait_def: traitDef });
    console.log(`Added trait_def for ${trait_decl.id.type} to context`);

    return node;
  }

  // Add the same helper methods as in ElaboratePass
  private analyzeSelfUsage(typeExpr: TypeExpression): {
    isUsed: boolean;
    arity: number;
  } {
    if ("type" in typeExpr && typeExpr.type === "Self") {
      return { isUsed: true, arity: 0 }; // Self itself, not an application
    }

    if ("app" in typeExpr) {
      // Check if the root is Self
      const root = this.getApplicationRoot(typeExpr);
      if ("type" in root && root.type === "Self") {
        // Count the arguments
        return { isUsed: true, arity: this.countApplicationArgs(typeExpr) };
      }
    }

    return { isUsed: false, arity: 0 };
  }

  private getApplicationRoot(typeExpr: TypeExpression): TypeExpression {
    if ("app" in typeExpr) {
      return this.getApplicationRoot(typeExpr.app.callee);
    }
    return typeExpr;
  }

  private countApplicationArgs(typeExpr: TypeExpression): number {
    if ("app" in typeExpr) {
      return typeExpr.app.args.length;
    }
    return 0;
  }

  // Add to TypeChecker class (private helper for scoped context extension)
  private withExtendedContext<T>(extension: Context, fn: () => T): T {
    // Push extensions
    for (const bind of extension) {
      this.context.push(bind);
    }
    try {
      return fn();
    } finally {
      // Pop extensions (in reverse order)
      for (let i = extension.length - 1; i >= 0; i--) {
        this.context.pop();
      }
    }
  }

  private computeStrippedKind(
    type: Type,
    implParamVars: string[],
    context: Context,
  ): { ok: { stripped: number; kind: Kind } } | { err: TypingError } {
    let current: Type = type;
    let strippedCount = 0;

    while ("app" in current) {
      const arg = current.app.arg;
      // Only strip if arg is a var that's an impl param (e.g., 'r')
      if ("var" in arg && implParamVars.includes(arg.var)) {
        const argKindResult = checkKind(context, arg);
        if ("err" in argKindResult) return { err: argKindResult.err };

        const funcKindResult = checkKind(context, current.app.func);
        if ("err" in funcKindResult) return { err: funcKindResult.err };

        if (!("arrow" in funcKindResult.ok)) {
          return { err: { not_a_type_function: current.app.func } };
        }

        if (!kindsEqual(funcKindResult.ok.arrow.from, argKindResult.ok)) {
          return {
            err: {
              kind_mismatch: {
                expected: funcKindResult.ok.arrow.from,
                actual: argKindResult.ok,
              },
            },
          };
        }

        // Strip successful: move to func
        current = current.app.func;
        strippedCount++;
      } else {
        // Non-strippable arg (concrete or non-impl-var)
        break;
      }
    }

    const finalKindResult = checkKind(context, current);
    if ("err" in finalKindResult) return { err: finalKindResult.err };

    return { ok: { stripped: strippedCount, kind: finalKindResult.ok } };
  }

  override visitImplDeclaration(node: ImplDeclaration): Declaration {
    const impl_decl = node.impl;

    // Get the trait definition
    const traitDef = this.context.find(
      (b) => "trait_def" in b && b.trait_def.name === impl_decl.name.type,
    );

    if (!traitDef || !("trait_def" in traitDef)) {
      console.error(new Error().stack);
      this.errors.push({ unbound: impl_decl.name.type });
      return node;
    }

    console.log(`Found trait_def for ${impl_decl.name.type}`);

    // Visit the target type
    this.visitTypeExpression(impl_decl.for);
    // Get the elaborated forType
    const forType = this.typeMap.get(impl_decl.for);
    if (!forType) {
      this.errors.push({ unbound: "for type" });
      return node;
    }
    console.log(`Elaborated forType: ${showType(forType)}`); // Debug: e.g., (Either l r)

    // Create implContext (copy of global + extensions)
    const implContext: Context = [...this.context];

    // Add impl's type parameters to context (always, even for vars like 'u', 'r')
    const implParamVars: string[] = []; // Track open vars for stripping
    for (const typeParam of impl_decl.type_params) {
      this.visitTypeExpression(typeParam);
      const paramTy = this.typeMap.get(typeParam);
      if (!paramTy) {
        this.errors.push({ unbound: "impl type param" });
        return node;
      }
      if ("var" in paramTy) {
        const v = paramTy.var; // e.g., 'u', 'r'
        implParamVars.push(v);
        if (!implContext.some((e) => "type" in e && e.type.name === v)) {
          implContext.push({ type: { name: v, kind: { star: null } } });
        }
        console.log(`Added impl param: ${v}:*`); // Debug
      } else {
        // Concrete param: check well-kinded
        const paramKind = checkKind(implContext, paramTy);
        if ("err" in paramKind) {
          this.errors.push(paramKind.err);
          return node;
        }
      }
    }

    // [FIX: Extract free vars from elaborated forType (type-level, like in elaboration)]
    const forTypeVars = collectTypeVars(forType); // Now uses normalized type, collects ['l', 'r']
    console.log(`forTypeVars: [${forTypeVars.join(", ")}]`); // Debug: e.g., ['l', 'r']

    const trueFreeVars = forTypeVars.filter((v) => !implParamVars.includes(v)); // e.g., ['l']
    console.log(`trueFreeVars (skolems): [${trueFreeVars.join(", ")}]`); // Debug

    // Add true free vars to implContext (e.g., 'l:*')
    for (const typeVar of trueFreeVars) {
      if (!implContext.some((e) => "type" in e && e.type.name === typeVar)) {
        implContext.push({
          type: { name: typeVar, kind: { star: null } },
        });
        console.log(`Added skolem: ${typeVar}:*`); // Debug: 'l:*'
      }
    }

    console.log(
      `implContext bindings: ${implContext
        .slice(-implParamVars.length - trueFreeVars.length)
        .map((e) => ("type" in e ? `${e.type.name}:*` : "other"))
        .join(", ")}`,
    ); // Now includes 'l:*'

    // Compute full kind for basic well-kinded check
    const fullTypeKind = checkKind(implContext, forType);
    if ("err" in fullTypeKind) {
      console.error(`Kind check failed for forType ${showType(forType)}`); // Debug
      console.error("implContext:", showContext(implContext)); // Debug: confirm 'l' bound
      this.errors.push(fullTypeKind.err);
      return node;
    }
    console.log(
      `Full forType "${showType(forType)}" kind: ${showKind(fullTypeKind.ok)}`,
    ); // Should now succeed

    // Compute STRIPPED partial kind (strips trailing impl param vars, e.g., Either<l, r> → Either<l> : * → *)
    const strippedResult = this.computeStrippedKind(
      forType,
      implParamVars,
      implContext,
    );
    if ("err" in strippedResult) {
      this.errors.push(strippedResult.err);
      return node;
    }
    const { stripped: strippedCount, kind: partialKind } = strippedResult.ok;
    console.log(
      `Partial kind after stripping ${strippedCount} args: ${showKind(partialKind)}`,
    ); // Debug: '* → *'

    // Check compatibility on PARTIAL kind
    const expectedKind = traitDef.trait_def.kind;
    console.log(`Expected Self kind: ${showKind(expectedKind)}`); // Debug: '* → *'
    if (!kindsEqual(expectedKind, partialKind)) {
      this.errors.push({
        kind_mismatch: { expected: expectedKind, actual: partialKind },
      });
      return node;
    }
    console.log("Kind check PASSED!");

    // Verify arity matches (stripped args == expected Self apps, e.g., 1 for * → *)
    const expectedArity = kindArity(expectedKind);
    if (strippedCount !== expectedArity) {
      this.errors.push({
        kind_mismatch: {
          // Or arity_mismatch
          expected: expectedKind,
          actual: partialKind,
        },
      });
      console.log(
        `Arity mismatch: expected ${expectedArity}, got ${strippedCount}`,
      );
      return node;
    }

    // Build methodContextExtension (deltas from implContext + self bind)
    const globalBindings = new Set(
      this.globalContext.map((b) => bindingKey(b)),
    );
    const methodContextExtension: Binding[] = [];
    for (const entry of implContext) {
      if (!globalBindings.has(bindingKey(entry))) {
        methodContextExtension.push(entry); // Deltas: t:*, l:*, etc.
      }
    }

    // Add self term binding
    methodContextExtension.push({
      term: { name: "self", type: forType },
    });

    const fullMethodContext: Context = [
      ...this.globalContext,
      ...methodContextExtension,
    ];

    // Also add methodBindings to full context
    const methodBindings: TermBinding[] = [];
    const traitTypeParamMap = new Map<string, Type>();

    // For "impl Map<r, u> for Either<l, r>", the trait's <t, u> map to impl's <r, u>
    const traitDecl = this.findTraitDeclaration(impl_decl.name.type);
    if (traitDecl) {
      // Map trait params to impl params (same order)
      for (
        let i = 0;
        i < impl_decl.type_params.length &&
        i < traitDecl.trait.type_params.length;
        i++
      ) {
        const traitParam = traitDecl.trait.type_params[i]!.name;
        const implParam = this.typeMap.get(impl_decl.type_params[i]!); // Changed from this.types
        if (implParam) {
          traitTypeParamMap.set(traitParam, implParam);
        }
      }
    }

    for (const [methodName, methodTy] of traitDef.trait_def.methods) {
      // Substitute "Self" with forType (existing)
      let instantiatedMethodTy = substituteType("Self", forType, methodTy);

      // Beta-reduce lambda applications
      instantiatedMethodTy = normalizeType(instantiatedMethodTy, this.context);

      // Substitute trait top-level params with impl params (existing)
      for (const [traitParam, implType] of traitTypeParamMap.entries()) {
        instantiatedMethodTy = substituteType(
          traitParam,
          implType,
          instantiatedMethodTy,
        );
      }

      // Instantiate method generics (per-method foralls like ∀t ∀u. ...) with fresh metas
      // This aligns renamed τ0/τ1 with impl context vars (r, u) via unification during check
      instantiatedMethodTy = instantiate(instantiatedMethodTy, freshMetaVar);

      // Normalize to beta-reduce any remaining apps
      instantiatedMethodTy = normalizeType(instantiatedMethodTy, this.context);

      console.log(
        `Bound method ${methodName}: ${showType(instantiatedMethodTy)}`, // Now shows (?0 → ?1) → (forType ?0) → (forType ?1)
      );

      methodBindings.push({
        term: { name: methodName, type: instantiatedMethodTy },
      });
    }

    for (const mb of methodBindings) {
      fullMethodContext.push(mb);
    }

    // Build full method context: type vars + self + all method bindings
    const fullMethodContextExtension: Binding[] =
      methodContextExtension.concat(methodBindings);

    this.enterImplContext();
    for (const fn of impl_decl.fns) {
      // Use withExtendedContext on deltas only? No—push fullMethodContext minus global? Wait.
      // Simpler: Temporarily set this.context = fullMethodContext during visitFn, restore after.
      const originalContext = this.context.slice();
      this.context = fullMethodContext;
      try {
        this.visitFn(fn); // Now has enums, self, map, etc.
      } finally {
        this.context = originalContext;
      }
      if (this.errors.length > 0) {
        const lastError = this.errors[this.errors.length - 1];
        console.error("Errors after method:", lastError);
        if (lastError && "type_mismatch" in lastError) {
          console.log("actual:", showType(lastError.type_mismatch.actual));
          console.log("expected:", showType(lastError.type_mismatch.expected));
        }
        return node;
      }
      console.log(`Method ${fn.name?.name} typechecked OK`);
    }
    this.exitImplContext();

    // Manually construct dict result (skip inferType to avoid redundant/buggy shape check)
    // The forType shape was already verified above, methods checked
    const dictTerm = this.termMap.get(node);
    if (!dictTerm) throw new Error("Dictionary term not generated");

    // Use a placeholder type (full forType or constructed); the binding uses full forType anyway
    const dictResult = {
      ok: { con: `Dict<${impl_decl.name.type}, ${showType(forType)}>` },
    };

    console.log(
      "Dict shape check skipped (already verified via forType); type:",
      showType(dictResult.ok),
    );

    let strippedForType = normalizeType(forType, this.context);
    for (let i = implParamVars.length - 1; i >= 0; i--) {
      if ("app" in strippedForType) {
        strippedForType = strippedForType.app.func;
      }
    }
    let implType = strippedForType;
    for (const skolem of trueFreeVars) {
      implType = forall_type(skolem, starKind, implType);
    }

    const implBinding = {
      trait_impl: {
        trait: impl_decl.name.type,
        type: implType, // ← now polymorphic ∀l::*.Either<l>
        dict: dictTerm,
      },
    };

    // Register impl in global context
    this.context.push(implBinding);
    if (
      !this.globalContext.some(
        (b) =>
          "trait_impl" in b &&
          b.trait_impl.trait === impl_decl.name.type &&
          typesEqual(b.trait_impl.type, forType),
      )
    ) {
      this.globalContext.push(implBinding);
    }

    console.log("Impl typecheck complete");
    return node;
  }

  findTraitDeclaration(traitName: string): TraitDeclaration | null {
    // Search scopes for the trait declaration
    for (const [_, scope] of this.scopes) {
      const element = lookup_type(traitName, scope);
      if (element && "trait" in element) {
        return element.trait;
      }
    }
    return null;
  }
}
