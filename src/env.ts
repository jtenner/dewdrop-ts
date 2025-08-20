import type { CoreType, DataConstructor, Kind } from "./types.ts";

export type Env = {
  type_constructors: Map<string, [Kind, DataConstructor[]]>;
  data_constructors: Map<string, CoreType>;
  term_vars: Map<string, CoreType>;
};

export const env_new = () => ({
  type_constructors: new Map(),
  data_constructors: new Map(),
  term_vars: new Map(),
});

export const add_type_constructor = (
  env: Env,
  name: string,
  kind: Kind,
  constructors: DataConstructor[],
) => {
  env.type_constructors.set(name, [kind, constructors]);
};

export const add_data_constructor = (env: Env, name: string, ty: CoreType) => {
  env.data_constructors.set(name, ty);
};

export const lookup_data_constructor = (env: Env, name: string) => {
  return env.data_constructors.get(name) ?? null;
};

export const get_data_constructors = (env: Env) => env.data_constructors;
