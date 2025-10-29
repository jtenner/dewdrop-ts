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
  type TypeImport,
} from "../parser.js";
import {
  type Context,
  checkKind,
  type Kind,
  kindsEqual,
  showType,
  type TraitDef,
  type Type,
  type TypingError,
  typecheck,
} from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import { lookup_type, type Scope, type ScopeIndex } from "./create_scopes.js";
import type { TermMap, TypeMap } from "./elaborate.js";

export class TypeChecker extends BaseVisitor {
  private globalContext = [] as Context;
  private context = [] as Context;
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
    this.context = this.globalContext.slice();
    this.visitModule(module);
  }

  override visitDeclaration(node: Declaration): Declaration {
    if ("trait" in node) console.log("visiting trait");
    else console.log("not found");
    return super.visitDeclaration(node);
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

  override visitFn(node: Fn): Fn {
    const term = this.termMap.get(node);
    if (!term) throw new Error("Term not generated for fn.");

    const result = typecheck(this.context, term);
    if ("ok" in result) {
      this.context.push({
        term: {
          name: node.name!.name,
          type: result.ok,
        },
      });
    } else {
      console.log(showFn(node));
      if ("not_a_function" in result.err) {
        console.log(showType(result.err.not_a_function));
      }
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

  override visitTypeDeclaration(node: TypeDeclaration): Declaration {
    const type_decl = node.type_dec;
    const type = this.typeMap.get(type_decl.value);

    if (!type) throw new Error("Type not elaborated for type declaration.");

    // Determine expected kind based on type parameters
    let expected_kind: Kind = { star: null };

    // Build kind from right to left: T1 → T2 → ... → *
    for (let i = type_decl.params.length - 1; i >= 0; i--) {
      expected_kind = {
        arrow: {
          from: { star: null }, // assume all params have kind *
          to: expected_kind,
        },
      };
    }

    // Check that the type is well-kinded
    const kind_result = checkKind(this.context, type);

    if ("ok" in kind_result) {
      if (kindsEqual(expected_kind, kind_result.ok)) {
        this.context.push({
          type: {
            name: type_decl.id.type,
            kind: kind_result.ok,
          },
        });
      } else {
        this.errors.push({
          unexpected_kind: { name: type_decl.id.type, kind: kind_result.ok },
        });
      }
    } else {
      this.errors.push(kind_result.err);
    }
    return node;
  }
  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    const trait_decl = node.trait;

    // Build trait kind
    let traitKind: Kind = { star: null };
    for (let i = trait_decl.type_params.length - 1; i >= 0; i--) {
      traitKind = {
        arrow: {
          from: { star: null },
          to: traitKind,
        },
      };
    }

    // Build method types (similar to elaboration but for checking)
    const methods: [string, Type][] = [];

    for (const traitFn of trait_decl.fns) {
      for (const param of traitFn.params) {
        this.visitTypeExpression(param.ty);
      }
      this.visitTypeExpression(traitFn.return_type);

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
      kind: traitKind,
      methods,
    };

    this.context.push({ trait_def: traitDef });

    return node;
  }

  override visitImplDeclaration(node: ImplDeclaration): Declaration {
    console.log("visiting impl declaration");
    const impl_decl = node.impl;

    // Get the trait definition
    const traitDef = this.context.find(
      (b) => "trait_def" in b && b.trait_def.name === impl_decl.name.type,
    );

    if (!traitDef || !("trait_def" in traitDef)) {
      this.errors.push({ unbound: impl_decl.name.type });
      return node;
    }

    const forType = this.typeMap.get(impl_decl.for);
    if (!forType) throw new Error("Impl 'for' type not found");

    // Type-check the dictionary term
    const dictTerm = this.termMap.get(node);
    if (!dictTerm) throw new Error("Dictionary term not generated");

    const result = typecheck(this.context, dictTerm);

    if ("ok" in result) {
      console.log("Impl success");
      this.context.push({
        trait_impl: {
          trait: impl_decl.name.type,
          type: forType,
          dict: dictTerm,
        },
      });
    } else {
      this.errors.push(result.err);
    }

    return node;
  }
}
