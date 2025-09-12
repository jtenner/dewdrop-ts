type Tagged<T, TKind extends string> = Readonly<T & { __kind: TKind }>;

type IndexRecord<TValue, TKind extends string> = {
  id: Tagged<number, TKind>;
  uid: string;
  module: string;
  name: string;
  value: TValue;
};

export class Index<TValue, TKind extends string> {
  private values = [] as TValue[];
  private by_name = new Map<string, IndexRecord<TValue, TKind>>();
  constructor(private kind: TKind) {}

  create_uid(module: string, name: string) {
    return `${this.kind}~${module}~${name}`;
  }

  allocate(module: string, name: string, value: TValue) {
    const id = (this.values.push(value) - 1) as unknown as Tagged<
      number,
      TKind
    >;
    const uid = this.create_uid(module, name);
    const record = {
      id,
      uid,
      module,
      name,
      value,
    };
    this.by_name.set(uid, record);
    return id;
  }

  get_by_name(module: string, name: string): TValue | null {
    const uid = this.create_uid(module, name);
    if (this.by_name.has(uid)) return this.by_name.get(name)!.value;
    return null;
  }

  get_by_id(id: Tagged<number, TKind>) {
    return this.values[id as unknown as number] ?? null;
  }
}

type EnumID = Tagged<number, "enum">;
type FnID = Tagged<number, "fn">;
type LetID = Tagged<number, "let">;
type TraitID = Tagged<number, "trait">;
type TypeID = Tagged<number, "type">;
type BuiltinID = Tagged<number, "builtin">;
type TableID = Tagged<number, "table">;
type MemoryID = Tagged<number, "memory">;

export const SCOPE_KIND_ENUM = 1;
export const SCOPE_KIND_FN = 2;
export const SCOPE_KIND_LET = 3;
export const SCOPE_KIND_TRAIT = 4;
export const SCOPE_KIND_TYPE = 5;
export const SCOPE_KIND_BUILTIN = 6;
export const SCOPE_KIND_TABLE = 7;
export const SCOPE_KIND_MEMORY = 8;
export const SCOPE_KIND_NAMESPACE = 9;

type ScopeKind =
  | typeof SCOPE_KIND_ENUM
  | typeof SCOPE_KIND_FN
  | typeof SCOPE_KIND_LET
  | typeof SCOPE_KIND_TRAIT
  | typeof SCOPE_KIND_TYPE
  | typeof SCOPE_KIND_BUILTIN
  | typeof SCOPE_KIND_TABLE
  | typeof SCOPE_KIND_MEMORY
  | typeof SCOPE_KIND_NAMESPACE;

export class Scope {
  constructor(private parent: Scope | null = null) {}
  kinds = new Map<string, ScopeKind>();
  enums = new Map<string, EnumID>();
  fns = new Map<string, FnID>();
  lets = new Map<string, LetID>();
  traits = new Map<string, TraitID>();
  types = new Map<string, TypeID>();
  builtins = new Map<string, BuiltinID>();
  tables = new Map<string, TableID>();
  memories = new Map<string, MemoryID>();
  namespaces = new Map<string, Scope>();

  define_enum(name: string, id: EnumID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_ENUM);
    this.enums.set(name, id);
    return true;
  }
  define_fn(name: string, id: FnID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_FN);
    this.fns.set(name, id);
    return true;
  }
  define_let(name: string, id: LetID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_LET);
    this.lets.set(name, id);
    return true;
  }
  define_trait(name: string, id: TraitID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_TRAIT);
    this.traits.set(name, id);
    return true;
  }
  define_type(name: string, id: TypeID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_TYPE);
    this.types.set(name, id);
    return true;
  }
  define_builtin(name: string, id: BuiltinID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_BUILTIN);
    this.builtins.set(name, id);
    return true;
  }
  define_table(name: string, id: TableID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_TABLE);
    this.tables.set(name, id);
    return true;
  }
  define_memory(name: string, id: MemoryID) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_MEMORY);
    this.memories.set(name, id);
    return true;
  }
  define_namespace(name: string, id: Scope) {
    if (this.kinds.has(name)) return false;
    this.kinds.set(name, SCOPE_KIND_NAMESPACE);
    this.namespaces.set(name, id);
    return true;
  }

  get_kind(name: string): ScopeKind | null {
    return this.kinds.get(name) ?? this.parent?.get_kind(name) ?? null;
  }

  get_enum_id(name: string): EnumID | null {
    return this.enums.get(name) ?? this.parent?.get_enum_id(name) ?? null;
  }
  get_fn_id(name: string): FnID | null {
    return this.fns.get(name) ?? this.parent?.get_fn_id(name) ?? null;
  }
  get_let_id(name: string): LetID | null {
    return this.lets.get(name) ?? this.parent?.get_let_id(name) ?? null;
  }
  get_trait_id(name: string): TraitID | null {
    return this.traits.get(name) ?? this.parent?.get_trait_id(name) ?? null;
  }
  get_type_id(name: string): TypeID | null {
    return this.types.get(name) ?? this.parent?.get_type_id(name) ?? null;
  }
  get_builtin_id(name: string): BuiltinID | null {
    return this.builtins.get(name) ?? this.parent?.get_builtin_id(name) ?? null;
  }
  get_table_id(name: string): TableID | null {
    return this.tables.get(name) ?? this.parent?.get_table_id(name) ?? null;
  }
  get_memory_id(name: string): MemoryID | null {
    return this.memories.get(name) ?? this.parent?.get_memory_id(name) ?? null;
  }
}
