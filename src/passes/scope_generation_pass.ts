import type { NameToken } from "../lexer.js";
import type {
  ArrowKindBodyExpression,
  BlockExpression,
  TypeConstructorExpression,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnDeclaration,
  FnExpression,
  FnParam,
  LetDeclaration,
  MatchArm,
  MemoryImport,
  NamedTypeExpression,
  NameIdentifier,
  PatternExpression,
  TraitDeclaration,
  TraitFn,
  TypeDeclaration,
  TypeIdentifier,
  ConstructorPatternExpression,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export type FnParamScopeSymbol = { fn_param: FnParam };
export type NamePatternScopeSymbol = { name_pattern: NameIdentifier };
export type FnDeclarationScopeSymbol = { fn_decl: FnDeclaration };
export type TypeDeclarationScopeSymbol = { type_alias: TypeDeclaration };
export type EnumDeclarationScopeSymbol = { enum_decl: EnumDeclaration };
export type EnumVariantScopeSymbol = { enum_variant: EnumVariant };
export type MemoryScopeSymbol = { memory: MemoryImport };
export type TraitScopeSymbol = { trait: TraitDeclaration };
export type TraitFnScopeSymbol = { trait_fn: TraitFn };
export type LetDeclarationScopeSymbol = { let_decl: LetDeclaration };
export type TypeParamScopeSymbol = { type_param: NameIdentifier };
export type ArrowKindSymbol = { arrow: ArrowKindBodyExpression }; // a <- b
export type ScopeSymbol =
  | FnParamScopeSymbol
  | TypeParamScopeSymbol
  | ArrowKindSymbol
  | NamePatternScopeSymbol
  | FnDeclarationScopeSymbol
  | TypeDeclarationScopeSymbol
  | EnumDeclarationScopeSymbol
  | EnumVariantScopeSymbol
  | MemoryScopeSymbol
  | TraitScopeSymbol
  | TraitFnScopeSymbol
  | LetDeclarationScopeSymbol;

export type NameResolutionError =
  | { unbound_name: string }
  | { unbound_type: string }
  | { unbound_constr: string };

type Scope = {
  parent: Scope | null;
  symbols: Map<string, ScopeSymbol>;
  children: Scope[];
  id: number;
  kind: "module" | "function" | "block" | "match_arm";
};

export class ScopeGenerationPass extends BaseVisitor {
  id = 0;
  current = {
    parent: null,
    symbols: new Map(),
    children: [],
    id: this.id++,
    kind: "module",
  } as Scope;
  scopes = [this.current] as Scope[];
  errors = [] as NameResolutionError[];
  lookup = new WeakMap<
    NameIdentifier | TypeIdentifier | ConstructorPatternExpression,
    ScopeSymbol
  >();

  enter(kind: Scope["kind"] = "function") {
    const next = {
      children: [],
      id: this.id++,
      kind,
      parent: this.current,
      symbols: new Map(),
    } as Scope;
    this.current.children.push(next);
    this.scopes.push(next);
    this.current = next;
  }

  exit() {
    const current = this.current;
    this.scopes.pop();
    if (!current.parent) throw new Error("Invalid scope stack state.");
    this.current = current.parent;
  }

  override visitFnParam(node: FnParam): FnParam {
    this.current.symbols.set(node.name.name, { fn_param: node });
    return node;
  }

  override visitNamePatternExpression(node: NameIdentifier): PatternExpression {
    this.current.symbols.set(node.name, { name_pattern: node });
    return node;
  }

  override visitFnDeclaration(node: FnDeclaration): Declaration {
    this.current.symbols.set(node.fn.fn.name!.name, { fn_decl: node });
    return node;
  }

  override visitTypeDeclaration(node: TypeDeclaration): Declaration {
    this.current.symbols.set(node.type_dec.id.type, { type_alias: node });
    return node;
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    this.current.symbols.set(node.enum.id.type, { enum_decl: node });
    return node;
  }

  override visitEnumVariant(node: EnumVariant): EnumVariant {
    const id = "fields" in node ? node.fields.id.type : node.values.id.type;
    this.current.symbols.set(id, { enum_variant: node });
    return node;
  }

  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    this.current.symbols.set(node.trait.id.type, { trait: node });
    return node;
  }

  override visitTraitFn(node: TraitFn): TraitFn {
    this.current.symbols.set(node.name.name, { trait_fn: node });
    return node;
  }

  override visitLetDeclaration(node: LetDeclaration): Declaration {
    this.current.symbols.set(node.let_dec.id.name, { let_decl: node });
    return node;
  }

  override visitFn(node: Fn): Fn {
    this.enter("function");
    // Add type parameters to scope
    for (const type_param of node.type_params) {
      this.current.symbols.set(type_param.name, { type_param });
    }
    super.visitFn(node);
    this.exit();
    return node;
  }

  override visitFnExpression(node: FnExpression): Expression {
    this.enter("function");
    super.visitFnExpression(node);
    this.exit();
    return node;
  }

  override visitBlockExpression(node: BlockExpression): Expression {
    this.enter("block");
    const next = super.visitBlockExpression(node);
    this.exit();
    return next;
  }

  override visitMatchArm(node: MatchArm): MatchArm {
    this.enter("match_arm");
    const next = super.visitMatchArm(node);
    this.exit();
    return next;
  }

  override visitNameIdentifier(node: NameIdentifier): NameIdentifier {
    const symbol = this.lookupSymbol(node.name);
    if (!symbol) {
      this.errors.push({ unbound_name: node.name });
    } else {
      // Each identifier must be a "unique" reference to store
      // a scope related to that identifier.
      // Replacement at this point means the identifier was
      // re-used at some point.
      if (this.lookup.has(node)) {
        node = { name: node.name };
      }
      this.lookup.set(node, symbol);
    }
    return node;
  }

  override visitConstructorPatternExpression(
    node: ConstructorPatternExpression,
  ): PatternExpression {
    const symbol = this.lookupSymbol(node.constr.type.type);
    if (!symbol) {
      this.errors.push({ unbound_constr: node.constr.type.type });
    } else {
      this.lookup.set(node, symbol);
    }
    return node;
  }

  override visitTypeIdentifier(node: TypeIdentifier): TypeIdentifier {
    const symbol = this.lookupSymbol(node.type);
    if (!symbol) {
      this.errors.push({ unbound_type: node.type });
    } else {
      // Each identifier must be a "unique" reference to store
      // a scope related to that identifier.
      // Replacement at this point means the identifier was
      // re-used at some point.
      if (this.lookup.has(node)) {
        node = { type: node.type };
      }
      this.lookup.set(node, symbol);
    }
    return node;
  }

  private lookupSymbol(name: string): ScopeSymbol | null {
    let scope: Scope | null = this.current;
    while (scope) {
      if (scope.symbols.has(name)) return scope.symbols.get(name)!;
      scope = scope.parent;
    }
    return null;
  }
}
