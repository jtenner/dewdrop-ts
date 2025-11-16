import * as path from "node:path";
import type {
  BodyExpression,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnParam,
  Import,
  Module,
  NameIdentifier,
  PatternExpression,
  TraitDeclaration,
  TraitFn,
  TypeDeclaration,
  TypeExpression,
} from "./parser.js";
import type {
  Binding,
  Pattern,
  Term,
  TraitDefBinding,
  TraitImplBinding,
  Type,
} from "system-f-omega";
import type { ModuleEntry, ModuleGraph } from "./graph.js";

export type FileEntry = {
  absolute: string;
  relative: string;
};

export const to_file_entry = (
  loc: string,
  basedir: string,
  ext: string | null = null,
) => {
  loc = ext && path.extname(loc) !== ext ? `${loc}.ext` : loc;
  return path.isAbsolute(loc)
    ? {
        absolute: loc,
        relative: `./${path.relative(basedir, loc)}`,
      }
    : {
        absolute: path.join(basedir, loc),
        relative: loc,
      };
};

export type Result<TErr, TOk> = { ok: TOk } | { err: TErr };

export type Builtin = {};

export const ok = <T>(val: T) => ({ ok: val });
export const err = <T>(val: T) => ({ err: val });

export type ASTNode =
  | Module
  | Declaration
  | Expression
  | EnumVariant
  | BodyExpression
  | TypeExpression
  | PatternExpression
  | Import
  | TraitFn
  | Fn
  | FnParam;
export type BuiltinScopeElement = { builtin: Builtin };
export type EnumScopeElement = { enum: EnumDeclaration };
export type FnParamScopeElement = { fnparam: FnParam };
export type FnScopeElement = { fn: Fn };
export type SelfScopeElement = { self: null };
export type SelfTypeScopeElement = { selftype: TypeExpression };
export type TraitDeclarationScopeElement = { trait: TraitDeclaration };
export type TraitFnScopeElement = { trait_fn: TraitFn };
export type TypeScopeElement = { type: TypeDeclaration };
export type TypeVarScopeElement = { typevar: NameIdentifier };
export type UnresolvedImportScopeElement = { unresolved: Import };
export type VariantScopeElement = { variant: EnumVariant };
export type VarScopeElement = { var: NameIdentifier };
export type ScopeElement =
  | BuiltinScopeElement
  | EnumScopeElement
  | FnParamScopeElement
  | FnScopeElement
  | SelfScopeElement
  | SelfTypeScopeElement
  | TraitDeclarationScopeElement
  | TraitFnScopeElement
  | TypeScopeElement
  | TypeVarScopeElement
  | UnresolvedImportScopeElement
  | VariantScopeElement
  | VarScopeElement;

export class Scope {
  terms = new Map<string, ScopeElement>();
  types = new Map<string, ScopeElement>();
  constructor(public parent: Scope | null = null) {}

  getType(name: string) {
    let current: Scope | null = this;
    while (current) {
      if (current.types.has(name)) return current.types.get(name)!;
      current = current.parent;
    }
    return null;
  }

  getTerm(name: string) {
    let current: Scope | null = this;
    while (current) {
      if (current.terms.has(name)) return current.terms.get(name)!;
      current = current.parent;
    }
    return null;
  }
}

export type FunctionDeclarationElaboration = {
  fn_decl: {
    fn: ASTNode;
    term: Term;
    type: Type;
  };
};
export type TraitDefElaboration = {
  trait_def: TraitDefBinding;
};
export type TraitImplElaboration = {
  trait_impl: TraitImplBinding;
};
export type TypeElaboration = {
  type: Type;
};
export type LetBindElaboration = { let_bind: { pattern: Pattern; term: Term } };
export type BuiltinElaboration = { builtin: { type: Type } };
export type EnumElaboration = {
  enum: {
    binding: Binding;
    alias: Binding | null;
    variantDefs: [string, [Term, Type]][];
  };
};
export type Elaboration =
  | FunctionDeclarationElaboration
  | TraitDefElaboration
  | TraitImplElaboration
  | TypeElaboration
  | LetBindElaboration
  | BuiltinElaboration
  | EnumElaboration;

export type CompilerContext = {
  bindings: Map<ASTNode, Binding[]>;
  builtins: Map<string, Builtin>;
  elaborated: Map<ASTNode, Elaboration>;
  globalModule: ModuleEntry;
  globalScope: Scope;
  modules: ModuleGraph;
  scopes: Map<ASTNode, Scope>;
  terms: Map<ASTNode, Term>;
  types: Map<ASTNode, Type>;
  patterns: Map<ASTNode, Pattern>;
};
