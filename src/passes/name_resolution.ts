import type {
  ArrowKindBodyExpression,
  BlockExpression,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnDeclaration,
  FnExpression,
  FnParam,
  LetDeclaration,
  MemoryImport,
  NameIdentifier,
  PatternExpression,
  TraitDeclaration,
  TraitFn,
  TypeDeclaration,
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
export type ArrowKindSymbol = { arrow: ArrowKindBodyExpression }; // a <- b
export type ScopeSymbol =
  | FnParamScopeSymbol
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

type Scope = {
  parent: Scope | null;
  symbols: Map<string, ScopeSymbol>;
  children: Scope[];
};

export class ScopeGenerationPass extends BaseVisitor {
  current: Scope = {
    parent: null,
    symbols: new Map(),
    children: [],
  };
  scopes: Scope[] = [this.current];

  enter() {
    const next = {
      parent: this.current,
      symbols: new Map(),
      children: [],
    } satisfies Scope;
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
    this.enter();
    super.visitFn(node);
    this.exit();
    return node;
  }

  override visitFnExpression(node: FnExpression): Expression {
    this.enter();
    super.visitFnExpression(node);
    this.exit();
    return node;
  }

  override visitBlockExpression(node: BlockExpression): Expression {
    this.enter();
    super.visitBlockExpression(node);
    this.exit();
    return node;
  }
}
