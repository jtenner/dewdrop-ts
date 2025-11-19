import * as path from "node:path";
import type { Kind, Pattern, Term, Type } from "system-f-omega";
import type { ModuleEntry, ModuleGraph } from "./graph.js";
import type {
  BodyExpression,
  BuiltinDeclaration,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  Fn,
  FnDeclaration,
  FnParam,
  ImplDeclaration,
  Import,
  Module,
  NameIdentifier,
  PatternExpression,
  TraitDeclaration,
  TraitFn,
  TypeDeclaration,
  TypeExpression,
} from "./parser.js";

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

export type EnumTypeWorkItem = {
  enum_type: {
    name: string;
    typeParams: string[]; // ["E", "A"]
    enumKind: Kind; // * → * → *
    variants: [string, Type][]; // [["Ok", A], ["Err", E]]
    node: EnumDeclaration; // (optional: good for error reporting)
  };
};

export type EnumConstructorWorkItem = {
  enum_constructor: {
    name: string; // e.g. "Ok"
    constructorType: Type;
    node: EnumVariant;
    parent: EnumDeclaration;
  };
};

export type BuiltinTypeWorkItem = {
  builtin: {
    name: string;
    fnType: Type;
    node: BuiltinDeclaration;
  };
};
export type FunctionWorkItem = {
  fn: {
    name: string;
    fnTerm: Term;
    fnType: Type;
    node: FnDeclaration;
  };
};
export type LetWorkItem = {
  let: {
    assert: boolean;
    pattern: Pattern;
    term: Term;
  };
};
export type TypeWorkItem = {
  type: {
    name: string;
    params: string[];
    body: Type;
    typeDecl: TypeDeclaration;
  };
};
export type TraitWorkItem = {
  trait: {
    name: string;
    typeParams: string[];
    fns: { name: string; fn: Type }[];
  };
};
export type ImplWorkItem = {
  impl: {
    traitName: string;
    forType: Type;
    traitType: Type;
    dict: Term;
    methods: { name: string; fnTerm: Term; fnType: Type }[];
    typeParams: string[];
    node: ImplDeclaration;
  };
};
export type WorkItem =
  | EnumTypeWorkItem
  | EnumConstructorWorkItem
  | BuiltinTypeWorkItem
  | FunctionWorkItem
  | LetWorkItem
  | TypeWorkItem
  | TraitWorkItem
  | ImplWorkItem;

export type CompilerContext = {
  builtins: Map<string, Builtin>;
  globalModule: ModuleEntry;
  globalScope: Scope;
  modules: ModuleGraph;
  scopes: Map<ASTNode, Scope>;
  imports: Map<Import, ScopeElement>;
  worklists: Map<Module, WorkItem[]>;
};
