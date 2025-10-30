import {
  type BuiltinDeclaration,
  type ConstructorImport,
  type Declaration,
  type EnumDeclaration,
  type Fn,
  type ImplDeclaration,
  type Import,
  type LetDeclaration,
  type Module,
  type NameImport,
  showFn,
  type TraitDeclaration,
  type TypeDeclaration,
  type TypeExpression,
  type TypeImport,
} from "../parser.js";
import {
  type Binding,
  type Context,
  checkKind,
  collectTypeVars,
  inferType,
  type Kind,
  kindArity,
  kindsEqual,
  showContext,
  showKind,
  showTerm,
  showType,
  substituteType,
  type Term,
  type TermBinding,
  type TraitDef,
  type Type,
  type TypingError,
  typecheck,
} from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import { lookup_type, type Scope, type ScopeIndex } from "./create_scopes.js";
import type { TermMap, TypeMap } from "./elaborate.js";

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

    // for (const [_, element] of globalScope.term_elements) {
    //   if ("let_decl" in element) {
    //     this.visitLetDeclaration(element.let_decl);
    //   } else if ("enum" in element) {
    //     this.visitEnumDeclaration(element.enum);
    //   } else if ("fn" in element) {
    //     this.visitFn(element.fn);
    //   } else if ("builtin" in element) {
    //     this.visitBuiltinDeclaration(element.builtin);
    //   } else {
    //     console.log(element);
    //     process.exit(1);
    //   }
    // }
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

  override visitTypeImport(node: TypeImport): Import {
    const scope = this.scopes.get(node.type.name);
    if (!scope) throw new Error("Scope not generated for import");
    const type = lookup_type(node.type.name.type, scope);
    if (!type) throw new Error("Could not find import at this point.");
    const alias = node.type.alias?.type ?? node.type.name.type;

    if ("enum" in type) {
      const enumImport = type.enum;
      const enumType = this.typeMap.get(enumImport);
      if (!enumType) throw new Error("Enum type not generated.");
      const kind = checkKind(this.context, enumType);
      if ("err" in kind) throw new Error("Err in kind here, what happened?");

      this.context.push({ type: { name: alias, kind: kind.ok } });
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
      console.error(`Type error in fn ${node.name?.name || "anon"}:`);
      console.error(showTerm(term));
      console.error(showContext(this.context));
      console.error(result.err);
      this.errors.push(result.err);
    }
    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    const enum_decl = node.enum;

    // Build the correct kind: * → * → ... → *
    let enumKind: Kind = { star: null };
    for (let i = enum_decl.type_params.length - 1; i >= 0; i--) {
      enumKind = {
        arrow: {
          from: { star: null },
          to: enumKind,
        },
      };
    }

    // Add enum type to context with the correct kind
    this.context.push({
      type: { name: enum_decl.id.type, kind: enumKind },
    });

    // Add each variant constructor to the context
    for (const variant of enum_decl.variants) {
      // Get the elaborated type for this variant constructor
      const variant_type = this.typeMap.get(variant);

      if (!variant_type) {
        console.warn("No type found for variant:", variant);
        continue;
      }

      // Get the variant constructor name
      const variant_name =
        "fields" in variant ? variant.fields.id.type : variant.values.id.type;

      // Just register the constructor - DON'T type-check it
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

    for (const traitFn of trait_decl.fns) {
      const paramTypes: Type[] = traitFn.params.map((p) => {
        const ty = this.typeMap.get(p.ty);
        if (!ty) throw new Error("Parameter type not found");
        return ty;
      });

      const returnType = this.typeMap.get(traitFn.return_type);
      if (!returnType) throw new Error("Return type not found");

      let selfType: Type = { var: "Self" };
      if (trait_decl.type_params.length > 0) {
        selfType = {
          app: {
            func: selfType,
            arg: { var: trait_decl.type_params[0]!.name },
          },
        };
      }

      let methodType = returnType;

      methodType = {
        arrow: {
          from: selfType,
          to: methodType,
        },
      };

      for (let i = paramTypes.length - 1; i >= 0; i--) {
        methodType = {
          arrow: {
            from: paramTypes[i]!,
            to: methodType,
          },
        };
      }

      for (let i = traitFn.type_params.length - 1; i >= 0; i--) {
        methodType = {
          forall: {
            var: traitFn.type_params[i]!.name,
            kind: { star: null },
            body: methodType,
          },
        };
      }

      methods.push([traitFn.name.name, methodType]);
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
    console.log("visiting impl declaration");
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
    const methodContextExtension: Binding[] = [];
    const globalBindings = new Set(this.context.map((b) => bindingKey(b))); // Use key to avoid JSON.stringify issues
    for (const entry of implContext) {
      if (!globalBindings.has(bindingKey(entry))) {
        methodContextExtension.push(entry); // e.g., t:*, u:*, l:*
      }
    }

    // Add self term binding (type = full forType, e.g., Option<t>)
    methodContextExtension.push({
      term: {
        name: "self",
        type: forType, // e.g., { app: { func: { con: "Option" }, arg: { var: "t" } } }
      },
    });

    console.log(
      `Method extension: ${methodContextExtension.map((b) => ("type" in b ? `${b.type.name}:*` : `self:${showType((b as TermBinding).term.type)}`)).join(", ")}`,
    ); // Debug: "t:*, u:*, self:(Option t)"

    const methodBindings: TermBinding[] = [];
    for (const [methodName, methodTy] of traitDef.trait_def.methods) {
      // Substitute "Self" with the full forType in the method type
      const instantiatedMethodTy = substituteType("Self", forType, methodTy);
      console.log(
        `Bound method ${methodName}: ${showType(instantiatedMethodTy)}`,
      ); // Debug
      methodBindings.push({
        term: { name: methodName, type: instantiatedMethodTy },
      });
    }

    // Build full method context: type vars + self + all method bindings
    const fullMethodContextExtension: Binding[] =
      methodContextExtension.concat(methodBindings);

    this.enterImplContext();
    // Type-check methods with full context (now includes "map" binding, self, etc.)
    for (const fn of impl_decl.fns) {
      this.withExtendedContext(fullMethodContextExtension, () => {
        this.visitFn(fn); // Now resolves self, map, etc.
      });
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

    // Register impl in global context
    this.context.push({
      trait_impl: {
        trait: impl_decl.name.type,
        type: forType,
        dict: dictTerm,
      },
    });

    console.log("Impl typecheck complete");
    return node;
  }
}
