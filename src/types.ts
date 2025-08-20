export type CoreModule = {
  type_defs: TypeDef[];
  term_defs: TermDef[];
};

export type TypeDef = {
  name: string;
  kind: Kind;
  ty: CoreType;
};

export type DataConstructor = {
  name: string;
  ty: CoreType;
};

export const data_constructor = (name: string, ty: CoreType) =>
  ({
    name,
    ty,
  }) as const;

export type TermDef = {
  name: string;
  ty: CoreType;
  body: CoreTerm;
};

export type Kind = { type: "star" } | { type: "arrow"; k1: Kind; k2: Kind };

export const star_kind = () => ({ type: "star" }) as const;
export const arrow_kind = (k1: Kind, k2: Kind) =>
  ({ type: "arrow", k1, k2 }) as const;

export type CoreType =
  | { type: "var"; name: string }
  | { type: "etvar"; name: string }
  | { type: "con"; name: string }
  | { type: "arrow"; t1: CoreType; t2: CoreType }
  | { type: "forall"; name: string; t: CoreType }
  | { type: "app"; f: CoreType; t: CoreType }
  | { type: "product"; tys: CoreType[] };

export const var_type = (name: string) =>
  ({
    type: "var",
    name,
  }) as const;

export const etvar_type = (name: string) =>
  ({
    type: "etvar",
    name,
  }) as const;

export const con_type = (name: string) =>
  ({
    type: "con",
    name,
  }) as const;

export const arrow_type = (t1: CoreType, t2: CoreType) =>
  ({
    type: "arrow",
    t1,
    t2,
  }) as const;

export const forall_type = (name: string, t: CoreType) =>
  ({
    type: "forall",
    name,
    t,
  }) as const;

export const app_type = (f: CoreType, t: CoreType) =>
  ({
    type: "app",
    f,
    t,
  }) as const;

export const product_type = (tys: CoreType[]) =>
  ({
    type: "product",
    tys,
  }) as const;

export type CoreTerm =
  | { type: "var"; name: string }
  | { type: "litint"; value: bigint }
  | { type: "lambda"; param: string; param_ty: CoreType; body: CoreTerm }
  | { type: "app"; func: CoreTerm; arg: CoreTerm }
  | { type: "typelambda"; param: string; body: CoreTerm }
  | { type: "constructor"; name: string; args: CoreTerm[] }
  | { type: "case"; scrutinee: CoreTerm; arms: CaseArm[] }
  | {
      type: "if";
      cond: CoreTerm;
      then_branch: CoreTerm;
      else_branch: CoreTerm;
    };

export type CorePattern =
  | { type: "wildcard" }
  | { type: "var"; name: string }
  | { type: "constructor"; name: string; args: CorePattern[] };

export type CaseArm = {
  pattern: CorePattern;
  body: CoreTerm;
};
