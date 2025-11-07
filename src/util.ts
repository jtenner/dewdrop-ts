import * as path from "node:path";
import type {
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnDeclaration,
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
import type { Term, Type } from "system-f-omega";

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
  | TypeExpression
  | PatternExpression
  | Import
  | TraitFn
  | FnParam;
export type BuiltinScopeElement = { builtin: Builtin };
export type EnumScopeElement = { enum: EnumDeclaration };
export type FnParamScopeElement = { fnparam: FnParam };
export type FnScopeElement = { fn: Fn };
export type SelfScopeElement = { self: null };
export type SelfTypeScopeElement = { selftype: TypeExpression };
export type TraitDeclarationScopeElement = { trait: TraitDeclaration };
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
  | TypeScopeElement
  | TypeVarScopeElement
  | UnresolvedImportScopeElement
  | VariantScopeElement
  | VarScopeElement;
export type Scope = {
  parent: Scope | null;
  types: Map<string, ScopeElement>;
  terms: Map<string, ScopeElement>;
};

export const getType = (name: string, scope: Scope) => {
  let current: Scope | null = scope;
  while (current) {
    if (current.types.has(name)) return current.types.get(name)!;
    current = scope.parent;
  }
  return null;
};

export const getTerm = (name: string, scope: Scope) => {
  let current: Scope | null = scope;
  while (current) {
    if (current.terms.has(name)) return current.terms.get(name)!;
    current = scope.parent;
  }
  return null;
};

export type CompilerContext = {
  modules: Map<string, Module>;
  scopes: Map<ASTNode, Scope>;
  globalScope: Scope;
  terms: Map<ASTNode, Term>;
  types: Map<ASTNode, Type>;
};
