import {
  app_type,
  arrow_type,
  type CorePattern,
  type CoreTerm,
  type CoreType,
  con_type,
  etvar_type,
  forall_type,
  product_type,
  var_type,
} from "./types.js";

export type WorklistEntry =
  | { type: "tvar"; tvar: string; kind: TyVarKind }
  | { type: "var"; var: string; ty: CoreType }
  | { type: "judgement"; judgement: Judgement };

export const tvar_worklist_entry = (tvar: string, kind: TyVarKind) =>
  ({
    type: "tvar",
    tvar,
    kind,
  }) satisfies WorklistEntry;
export const var_worklist_entry = (_var: string, ty: CoreType) =>
  ({
    type: "var",
    var: _var,
    ty,
  }) satisfies WorklistEntry;
export const judgement_worklist_entry = (judgement: Judgement) =>
  ({
    type: "judgement",
    judgement,
  }) satisfies WorklistEntry;

export type TyVarKind =
  | { type: "universal" }
  | { type: "existential" }
  | { type: "solved"; ty: CoreType }
  | { type: "marker" };

export const universal_ty_var_kind = () =>
  ({ type: "universal" }) satisfies TyVarKind;
export const existential_ty_var_kind = () =>
  ({ type: "existential" }) satisfies TyVarKind;
export const solved_ty_var_kind = (ty: CoreType) =>
  ({
    type: "solved",
    ty,
  }) satisfies TyVarKind;
export const marker_ty_var_kind = () =>
  ({ type: "marker" }) satisfies TyVarKind;

export type Judgement =
  | { type: "sub"; left: CoreType; right: CoreType }
  | { type: "inf"; term: CoreTerm; ty: CoreType }
  | { type: "chk"; term: CoreTerm; ty: CoreType }
  | { type: "infapp"; func_ty: CoreType; arg: CoreTerm; result_ty: CoreType };

export const sub_judgement = (left: CoreType, right: CoreType) =>
  ({ type: "sub", left, right }) satisfies Judgement;

export const inf_judgement = (term: CoreTerm, ty: CoreType) =>
  ({ type: "inf", term, ty }) satisfies Judgement;

export const chk_judgement = (term: CoreTerm, ty: CoreType) =>
  ({ type: "chk", term, ty }) satisfies Judgement;

export const infapp_judgement = (
  func_ty: CoreType,
  arg: CoreTerm,
  result_ty: CoreType,
) => ({ type: "infapp", func_ty, arg, result_ty }) satisfies Judgement;

export type Worklist = {
  entries: WorklistEntry[];
  next_var: bigint;
};

export const worklist_new = () =>
  ({
    entries: [],
    next_var: 0n,
  }) satisfies Worklist;

export const fresh_var = (self: Worklist) => (self.next_var++).toString();

export const push = (self: Worklist, entry: WorklistEntry) => {
  self.entries.push(entry);
};

export const pop = (self: Worklist) => {
  return self.entries.pop() ?? null;
};

export const find_var = (self: Worklist, name: string) => {
  for (let i = self.entries.length; i >= 0; i--) {
    const entry = self.entries[i];
    if (entry && entry.type === "var" && entry.var === name) {
      return entry.ty;
    }
  }
  return null;
};

export const solve_evar = (self: Worklist, name: string, ty: CoreType) => {
  for (const entry of self.entries) {
    if (entry.type === "tvar" && entry.tvar === name) {
      if (entry.kind.type === "existential") {
        entry.kind = {
          type: "solved",
          ty,
        };
        return;
      }

      if (entry.kind.type === "solved") return;
    }
  }

  throw new Error(`UnboundVariable: Expected ${name}`);
};

export const before = (self: Worklist, a: string, b: string) => {
  let pos_a = -1;
  let pos_b = -1;

  let i = 0;
  for (const entry of self.entries) {
    if (entry.type === "tvar") {
      if (a === entry.tvar) pos_a = i;
      if (b === entry.tvar) pos_b = i;
    }

    i++;
  }

  return pos_a >= 0 && pos_b >= 0 && pos_a < pos_b;
};

export type DKInference = {
  worklist: Worklist;
  trace: string[];
  data_constructors: Map<string, CoreType>;
  var_context: Map<string, CoreType>;
};

export const dk_inference_with_context = (
  data_constructors: Map<string, CoreType>,
  var_context: Map<string, CoreType>,
) =>
  ({
    worklist: worklist_new(),
    trace: [],
    data_constructors,
    var_context,
  }) satisfies DKInference;

export const check_type = (
  self: DKInference,
  term: CoreTerm,
  expected_ty: CoreType,
) => {
  push(
    self.worklist,
    judgement_worklist_entry(chk_judgement(term, expected_ty)),
  );
  solve(self);
};

const solve = (self: DKInference) => {
  while (true) {
    const entry = pop(self.worklist);

    if (!entry) return;

    if (entry.type === "judgement") solve_judgement(self, entry.judgement);
  }
};

const solve_judgement = (self: DKInference, judgement: Judgement) => {
  switch (judgement.type) {
    case "sub":
      return solve_subtype(self, judgement.left, judgement.right);
    case "inf":
      return solve_inference(self, judgement.term, judgement.ty);
    case "chk":
      return solve_checking(self, judgement.term, judgement.ty);
    case "infapp":
      return solve_inf_app(
        self,
        judgement.func_ty,
        judgement.arg,
        judgement.result_ty,
      );
  }
};

const eq = (left: unknown, right: unknown) => {
  if (left === right) return true;
  if (typeof left === "object" && typeof right === "object" && left && right) {
    const left_keys = new Set(Object.keys(left));
    const right_keys = new Set(Object.keys(right));
    if (left_keys.difference(right_keys).size === 0) {
      for (const key of left_keys as Set<keyof typeof left>) {
        if (!eq(left[key], right[key])) return false;
      }
    } else return false;
    return true;
  }
  return false;
};

const solve_subtype = (self: DKInference, left: CoreType, right: CoreType) => {
  if (eq(left, right)) return;

  if (left.type === "arrow" && right.type === "arrow") {
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(right.t1, left.t1)),
    );
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(left.t2, right.t2)),
    );
    return;
  }

  if (left.type === "app" && right.type === "app") {
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(left.f, right.f)),
    );
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(left.t, right.t)),
    );
    return;
  }

  if (right.type === "forall") {
    const name = right.name;
    const ty = right.t;
    const fv = fresh_var(self.worklist);
    push(self.worklist, tvar_worklist_entry(fv, universal_ty_var_kind()));
    const substituted_ty = substitute_type(self, name, etvar_type(fv), ty);
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(left, substituted_ty)),
    );
    return;
  }

  if (left.type === "forall") {
    const name = left.name;
    const ty = left.t;
    const fv = fresh_var(self.worklist);
    push(self.worklist, tvar_worklist_entry(fv, marker_ty_var_kind()));
    push(self.worklist, tvar_worklist_entry(fv, existential_ty_var_kind()));
    const substituted_ty = substitute_type(self, name, etvar_type(fv), ty);
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(substituted_ty, right)),
    );
    return;
  }

  if (left.type === "etvar" && !occurs_check(self, left.name, right)) {
    return instantiate_left(self, left.name, right);
  }

  if (right.type === "etvar" && !occurs_check(self, right.name, left)) {
    return instantiate_right(self, left, right.name);
  }

  throw new Error("Subtyping Error", {
    cause: { left, right },
  });
};

export const solve_inference = (
  self: DKInference,
  term: CoreTerm,
  ty: CoreType,
) => {
  self.trace.push(`Inf {${JSON.stringify(term)}} |- {${JSON.stringify(ty)}}`);

  if (term.type === "var") {
    const name = term.name;
    const var_ty =
      self.var_context.get(name) ??
      find_var(self.worklist, name) ??
      self.data_constructors.get(name) ??
      null;

    if (var_ty) {
      push(self.worklist, judgement_worklist_entry(sub_judgement(var_ty, ty)));
      return;
    }
    throw new Error(`UnboundVariable: ${name}`);
  }

  if (term.type === "litint") {
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(con_type("Int"), ty)),
    );
    return;
  }

  if (term.type === "lambda") {
    const { param, param_ty, body } = term;
    const result_ty = etvar_type(fresh_var(self.worklist));
    push(
      self.worklist,
      tvar_worklist_entry(result_ty.name, existential_ty_var_kind()),
    );

    const arrow_ty = arrow_type(param_ty, result_ty);

    push(self.worklist, judgement_worklist_entry(sub_judgement(arrow_ty, ty)));
    push(self.worklist, var_worklist_entry(param, param_ty));
    push(
      self.worklist,
      judgement_worklist_entry(inf_judgement(body, result_ty)),
    );
    return;
  }

  if (term.type === "app") {
    const { func, arg } = term;
    const func_ty = etvar_type(fresh_var(self.worklist));
    const var_name = func_ty.name;
    push(
      self.worklist,
      tvar_worklist_entry(var_name, existential_ty_var_kind()),
    );
    push(
      self.worklist,
      judgement_worklist_entry(infapp_judgement(func_ty, arg, ty)),
    );
    push(self.worklist, judgement_worklist_entry(inf_judgement(func, func_ty)));
    return;
  }

  if (term.type === "typelambda") {
    const { param, body } = term;
    const body_ty = etvar_type(fresh_var(self.worklist));
    const var_name = body_ty.name;

    push(
      self.worklist,
      tvar_worklist_entry(var_name, existential_ty_var_kind()),
    );

    const forall_ty = forall_type(param, body_ty);
    push(self.worklist, judgement_worklist_entry(sub_judgement(forall_ty, ty)));

    push(self.worklist, tvar_worklist_entry(param, universal_ty_var_kind()));
    push(self.worklist, judgement_worklist_entry(inf_judgement(body, body_ty)));
    return;
  }

  if (term.type === "constructor") {
    const { name } = term;
    const constructor_ty = self.data_constructors.get(name);

    if (constructor_ty) {
      const instantiated_ty = instantiate_constructor_type(
        self,
        constructor_ty,
      );
      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(instantiated_ty, ty)),
      );
      return;
    }
    throw new Error(`UnboundDataConstructor: ${name}`);
  }

  if (term.type === "if") {
    const { cond, then_branch, else_branch } = term;
    push(
      self.worklist,
      judgement_worklist_entry(chk_judgement(cond, con_type("Bool"))),
    );

    push(
      self.worklist,
      judgement_worklist_entry(chk_judgement(then_branch, ty)),
    );

    push(
      self.worklist,
      judgement_worklist_entry(chk_judgement(else_branch, ty)),
    );
    return;
  }

  if (term.type === "case") {
    const { scrutinee, arms } = term;
    const scrutinee_ty = etvar_type(fresh_var(self.worklist));
    add_etvars_to_worklist(self, scrutinee_ty);

    push(
      self.worklist,
      judgement_worklist_entry(chk_judgement(scrutinee, scrutinee_ty)),
    );

    for (const arm of arms) {
      const pattern_bindings = check_pattern(self, arm.pattern, scrutinee_ty);

      for (const [var_name, var_type] of pattern_bindings) {
        push(self.worklist, var_worklist_entry(var_name, var_type));
      }

      push(
        self.worklist,
        judgement_worklist_entry(chk_judgement(arm.body, ty)),
      );
    }
    return;
  }
};

const solve_checking = (self: DKInference, term: CoreTerm, ty: CoreType) => {
  self.trace.push(`Chk {${JSON.stringify(term)}} <- {${JSON.stringify(ty)}}`);

  if (term.type === "lambda" && ty.type === "arrow") {
    const { param, param_ty, body } = term;
    const { t1: expected_param, t2: result_ty } = ty;

    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(param_ty, expected_param)),
    );
    push(self.worklist, var_worklist_entry(param, param_ty));
    push(
      self.worklist,
      judgement_worklist_entry(chk_judgement(body, result_ty)),
    );
  } else if (ty.type === "forall") {
    const { name, t: body_ty } = ty;
    const fv = fresh_var(self.worklist);
    push(self.worklist, tvar_worklist_entry(fv, universal_ty_var_kind()));

    const substituted_ty = substitute_type(self, name, var_type(fv), body_ty);
    push(
      self.worklist,
      judgement_worklist_entry(chk_judgement(term, substituted_ty)),
    );
  } else {
    const inferred_ty = etvar_type(fresh_var(self.worklist));
    const var_name = inferred_ty.name;
    push(
      self.worklist,
      tvar_worklist_entry(var_name, existential_ty_var_kind()),
    );
    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(inferred_ty, ty)),
    );
    push(
      self.worklist,
      judgement_worklist_entry(inf_judgement(term, inferred_ty)),
    );
  }
};

const solve_inf_app = (
  self: DKInference,
  func_ty: CoreType,
  arg: CoreTerm,
  result_ty: CoreType,
) => {
  if (func_ty.type === "arrow") {
    const { t1: param_ty, t2: ret_ty } = func_ty;

    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(ret_ty, result_ty)),
    );
    push(self.worklist, judgement_worklist_entry(chk_judgement(arg, param_ty)));
  } else if (func_ty.type === "forall") {
    const { name, t: body } = func_ty;
    const fe = fresh_var(self.worklist);
    push(self.worklist, tvar_worklist_entry(fe, existential_ty_var_kind()));
    const substituted_ty = substitute_type(self, name, etvar_type(fe), body);
    push(
      self.worklist,
      judgement_worklist_entry(
        infapp_judgement(substituted_ty, arg, result_ty),
      ),
    );
  } else if (func_ty.type === "etvar") {
    const { name } = func_ty;
    const param_ty_name = fresh_var(self.worklist);
    const ret_ty_name = fresh_var(self.worklist);
    const param_ty = etvar_type(param_ty_name);
    const ret_ty = etvar_type(ret_ty_name);

    push(
      self.worklist,
      tvar_worklist_entry(param_ty_name, existential_ty_var_kind()),
    );
    push(
      self.worklist,
      tvar_worklist_entry(ret_ty_name, existential_ty_var_kind()),
    );

    const arrow_ty = arrow_type(param_ty, ret_ty);
    solve_evar(self.worklist, name, arrow_ty);

    push(
      self.worklist,
      judgement_worklist_entry(sub_judgement(ret_ty, result_ty)),
    );
    push(self.worklist, judgement_worklist_entry(chk_judgement(arg, param_ty)));
  } else throw new Error("NotAFunction", { cause: func_ty });
};

const instantiate_left = (self: DKInference, name: string, ty: CoreType) => {
  switch (ty.type) {
    case "etvar": {
      const { name: b } = ty;
      solve_evar(self.worklist, b, etvar_type(name));
      break;
    }
    case "arrow": {
      const { t1, t2 } = ty;
      const a1 = fresh_var(self.worklist);
      const a2 = fresh_var(self.worklist);
      const arrow_ty = arrow_type(etvar_type(a1), etvar_type(a2));

      solve_evar(self.worklist, name, arrow_ty);

      push(self.worklist, tvar_worklist_entry(a1, existential_ty_var_kind()));
      push(self.worklist, tvar_worklist_entry(a2, existential_ty_var_kind()));

      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(t1, etvar_type(a1))),
      );
      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(etvar_type(a2), t2)),
      );
      break;
    }
    case "app": {
      const { f: t1, t: t2 } = ty;
      const a1 = fresh_var(self.worklist);
      const a2 = fresh_var(self.worklist);
      const app_ty = app_type(etvar_type(a1), etvar_type(a2));

      solve_evar(self.worklist, name, app_ty);

      push(self.worklist, tvar_worklist_entry(a1, existential_ty_var_kind()));
      push(self.worklist, tvar_worklist_entry(a2, existential_ty_var_kind()));

      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(etvar_type(a1), t1)),
      );
      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(etvar_type(a2), t2)),
      );
      break;
    }
    case "forall": {
      const { name: b, t } = ty;
      const fv = fresh_var(self.worklist);
      push(self.worklist, tvar_worklist_entry(fv, universal_ty_var_kind()));

      const substituted_ty = substitute_type(self, b, var_type(fv), t);
      instantiate_left(self, name, substituted_ty);
      break;
    }
    default: {
      if (is_monotype(self, ty)) {
        solve_evar(self.worklist, name, ty);
      } else {
        throw new Error("Instantiation Error", {
          cause: { name, ty },
        });
      }
    }
  }
};

const instantiate_right = (self: DKInference, ty: CoreType, name: string) => {
  switch (ty.type) {
    case "etvar": {
      const { name: b } = ty;
      solve_evar(self.worklist, b, etvar_type(name));
      break;
    }
    case "arrow": {
      const { t1, t2 } = ty;
      const a1 = fresh_var(self.worklist);
      const a2 = fresh_var(self.worklist);
      const arrow_ty = arrow_type(etvar_type(a1), etvar_type(a2));

      solve_evar(self.worklist, name, arrow_ty);

      push(self.worklist, tvar_worklist_entry(a1, existential_ty_var_kind()));
      push(self.worklist, tvar_worklist_entry(a2, existential_ty_var_kind()));

      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(etvar_type(a1), t1)),
      );
      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(t2, etvar_type(a2))),
      );
      break;
    }
    case "app": {
      const { f: t1, t: t2 } = ty;
      const a1 = fresh_var(self.worklist);
      const a2 = fresh_var(self.worklist);
      const app_ty = app_type(etvar_type(a1), etvar_type(a2));

      solve_evar(self.worklist, name, app_ty);

      push(self.worklist, tvar_worklist_entry(a1, existential_ty_var_kind()));
      push(self.worklist, tvar_worklist_entry(a2, existential_ty_var_kind()));

      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(t1, etvar_type(a1))),
      );
      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(t2, etvar_type(a2))),
      );
      break;
    }
    case "forall": {
      const { name: a, t } = ty;
      const fv = fresh_var(self.worklist);
      push(self.worklist, tvar_worklist_entry(fv, marker_ty_var_kind()));
      push(self.worklist, tvar_worklist_entry(fv, existential_ty_var_kind()));
      const substituted_ty = substitute_type(self, a, var_type(fv), t);
      instantiate_right(self, substituted_ty, name);
      break;
    }
    default: {
      if (is_monotype(self, ty)) {
        solve_evar(self.worklist, name, ty);
      } else {
        throw new Error("InstantiationError", { cause: { name, ty } });
      }
    }
  }
};

const occurs_check = (
  self: DKInference,
  name: string,
  ty: CoreType,
): boolean => {
  if (ty.type === "etvar" || ty.type === "var") return ty.name === name;
  if (ty.type === "arrow")
    return occurs_check(self, name, ty.t1) || occurs_check(self, name, ty.t2);
  if (ty.type === "app")
    return occurs_check(self, name, ty.f) || occurs_check(self, name, ty.t);
  if (ty.type === "forall") return occurs_check(self, name, ty.t);
  if (ty.type === "product")
    return ty.tys.some((t) => occurs_check(self, name, t));
  return false;
};

const is_monotype = (self: DKInference, ty: CoreType): boolean => {
  if (ty.type === "con" || ty.type === "var" || ty.type === "etvar")
    return true;
  if (ty.type === "arrow")
    return is_monotype(self, ty.t1) && is_monotype(self, ty.t2);
  if (ty.type === "app")
    return is_monotype(self, ty.f) && is_monotype(self, ty.t);
  if (ty.type === "product") return ty.tys.every((t) => is_monotype(self, t));
  return false;
};

const substitute_type = (
  self: DKInference,
  name: string,
  replacement: CoreType,
  ty: CoreType,
): CoreType => {
  switch (ty.type) {
    case "var": {
      if (ty.name === name) return replacement;
      return ty;
    }
    case "arrow": {
      const { t1, t2 } = ty;
      return arrow_type(
        substitute_type(self, name, replacement, t1),
        substitute_type(self, name, replacement, t2),
      );
    }
    case "forall": {
      const { name: bound_var, t } = ty;
      if (bound_var === name) return ty;
      return forall_type(
        bound_var,
        substitute_type(self, name, replacement, t),
      );
    }
    case "app": {
      const { f, t } = ty;
      return app_type(
        substitute_type(self, name, replacement, f),
        substitute_type(self, name, replacement, t),
      );
    }
    case "product": {
      const { tys } = ty;
      return product_type(
        tys.map((t) => substitute_type(self, name, replacement, t)),
      );
    }
    default: {
      return ty;
    }
  }
};

const add_etvars_to_worklist = (self: DKInference, ty: CoreType) => {
  switch (ty.type) {
    case "etvar": {
      push(
        self.worklist,
        tvar_worklist_entry(ty.name, existential_ty_var_kind()),
      );
      break;
    }
    case "arrow": {
      add_etvars_to_worklist(self, ty.t1);
      add_etvars_to_worklist(self, ty.t2);
      break;
    }
    case "app": {
      add_etvars_to_worklist(self, ty.f);
      add_etvars_to_worklist(self, ty.t);
      break;
    }
    case "forall": {
      add_etvars_to_worklist(self, ty.t);
      break;
    }
    case "product": {
      for (const t of ty.tys) {
        add_etvars_to_worklist(self, t);
      }
      break;
    }
  }
};

const check_pattern = (
  self: DKInference,
  pattern: CorePattern,
  expected_ty: CoreType,
): Map<string, CoreType> => {
  let bindings = new Map<string, CoreType>();
  switch (pattern.type) {
    case "wildcard":
      return bindings;
    case "var": {
      const var_name = pattern.name;
      bindings.set(var_name, expected_ty);
      return bindings;
    }
    case "constructor": {
      const { name, args } = pattern;
      const constructor_ty = self.data_constructors.get(name) ?? null;

      if (!constructor_ty) throw new Error(`UnboundDataConstructor: ${name}`);

      const [param_tys, result_ty] = extract_constructor_signature(
        self,
        constructor_ty,
      );

      push(
        self.worklist,
        judgement_worklist_entry(sub_judgement(result_ty, expected_ty)),
      );

      if (args.length !== param_tys.length)
        throw new Error(
          `ArityMismatch: ${args.length} !== ${param_tys.length}`,
        );

      for (let i = 0; i < args.length; i++) {
        const arg_pattern = args[i]!;
        const param_ty = param_tys[i]!;
        const arg_bindings = check_pattern(self, arg_pattern, param_ty);
        const current_bindings = new Set(bindings.keys());
        const incoming_bindings = new Set(arg_bindings.keys());
        const conflicts = current_bindings.intersection(incoming_bindings);
        if (conflicts.size > 0)
          throw new Error(`Cannot Bind: ${Array.from(conflicts)}`);
        bindings = new Map([...bindings, ...arg_bindings]);
      }

      return bindings;
    }
  }
};

const extract_constructor_signature = (
  self: DKInference,
  constructor_ty: CoreType,
) => {
  const param_types = [] as CoreType[];
  let current_ty = constructor_ty;
  const substitutions = new Map<string, CoreType>();

  while (current_ty.type === "forall") {
    const { name, t: body } = current_ty;
    const fresh_var_name = fresh_var(self.worklist);
    const fresh_evar = etvar_type(fresh_var_name);

    push(
      self.worklist,
      tvar_worklist_entry(fresh_var_name, existential_ty_var_kind()),
    );

    substitutions.set(name, fresh_evar);
    current_ty = body;
  }

  current_ty = apply_type_substitutions(self, current_ty, substitutions);

  while (current_ty.type === "arrow") {
    const { t1: param_ty, t2: result_ty } = current_ty;
    param_types.push(param_ty);
    current_ty = result_ty;
  }

  return [param_types, current_ty] as const;
};

const instantiate_constructor_type = (
  self: DKInference,
  constructor_ty: CoreType,
) => {
  let current_ty = constructor_ty;
  const substitutions = new Map<string, CoreType>();

  while (current_ty.type === "forall") {
    const { name, t: body } = current_ty;
    const fresh_var_name = fresh_var(self.worklist);
    const fresh_evar = etvar_type(fresh_var_name);

    push(
      self.worklist,
      tvar_worklist_entry(fresh_var_name, existential_ty_var_kind()),
    );

    substitutions.set(name, fresh_evar);
    current_ty = body;
  }

  return apply_type_substitutions(self, current_ty, substitutions);
};

const apply_type_substitutions = (
  self: DKInference,
  ty: CoreType,
  substitutions: Map<string, CoreType>,
): CoreType => {
  switch (ty.type) {
    case "var": {
      const { name } = ty;
      return substitutions.get(name) ?? ty;
    }
    case "arrow": {
      const { t1, t2 } = ty;
      return arrow_type(
        apply_type_substitutions(self, t1, substitutions),
        apply_type_substitutions(self, t2, substitutions),
      );
    }
    case "app": {
      const { f, t } = ty;
      return app_type(
        apply_type_substitutions(self, f, substitutions),
        apply_type_substitutions(self, t, substitutions),
      );
    }
    case "forall": {
      const { name, t: body } = ty;
      return forall_type(
        name,
        apply_type_substitutions(self, body, substitutions),
      );
    }
    case "product": {
      const { tys } = ty;
      return product_type(
        tys.map((t) => apply_type_substitutions(self, t, substitutions)),
      );
    }
    case "etvar":
    case "con":
      return ty;
  }
};
