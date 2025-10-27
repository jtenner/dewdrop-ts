import type {
  Declaration,
  EnumDeclaration,
  Fn,
  FnDeclaration,
  LetDeclaration,
  Module,
  TypeDeclaration,
} from "../parser.js";
import {
  type Context,
  checkKind,
  isStarKind,
  type Kind,
  kindsEqual,
  showKind,
  type TypingError,
  typecheck,
} from "../types_system_f_omega.js";
import { BaseVisitor } from "../visitor.js";
import type { Scope } from "./create_scopes.js";
import type { TermMap, TypeMap } from "./elaborate.js";

export class TypeChecker extends BaseVisitor {
  private globalContext = [] as Context;
  private context = [] as Context;
  errors = [] as TypingError[];
  constructor(
    public termMap: TermMap,
    public typeMap: TypeMap,
    public globalScope: Scope,
  ) {
    super();
    this.context = this.globalContext;

    for (const [type, element] of globalScope.term_elements) {
      if ("let_decl" in element) {
        this.visitLetDeclaration(element.let_decl);
      } else if ("enum" in element) {
        this.visitEnumDeclaration(element.enum);
      } else if ("fn" in element) {
        this.visitFn(element.fn);
      }
    }
  }

  check(module: Module) {
    this.context = this.globalContext.slice();
    this.visitModule(module);
  }

  override visitLetDeclaration(node: LetDeclaration): Declaration {
    const term = this.termMap.get(node.let_dec.value);
    if (!term) {
      console.log(node.let_dec.value);
      throw new Error("Term not generated for let.");
    }
    const result = typecheck(this.context, term);
    if ("ok" in result) {
      this.globalContext.push({
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

  override visitFn(node: Fn): Fn {
    const term = this.termMap.get(node);
    if (!term) throw new Error("Term not generated for fn.");

    const result = typecheck(this.context, term);
    if ("ok" in result) {
      this.globalContext.push({
        term: {
          name: node.name!.name,
          type: result.ok,
        },
      });
    } else {
      this.errors.push(result.err);
    }
    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    const enum_decl = node.enum;

    // Add enum type to context
    this.context.push({
      type: { name: enum_decl.id.type, kind: { star: null } },
    });

    // Type check and add each variant constructor
    for (const variant of enum_decl.variants) {
      const variant_term = this.termMap.get(variant);
      const variant_type = this.typeMap.get(variant);

      if (!variant_term || !variant_type) continue;

      // Type check the variant constructor
      const result = typecheck(this.context, variant_term);

      if ("ok" in result) {
        // Add variant to context
        const variant_name =
          "fields" in variant ? variant.fields.id.type : variant.values.id.type;

        this.context.push({
          term: { name: variant_name, type: result.ok },
        });
      } else {
        this.errors.push(result.err);
      }
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
}
