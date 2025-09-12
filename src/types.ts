// import type {
//   EnumDeclaration,
//   Expression,
//   TraitFn,
//   TypeExpression,
// } from "./parser.js";

// type SubtypeJudgement = { left: TypeExpression; right: TypeExpression };

// type Judgement =
//   | { subtype: SubtypeJudgement }
//   | { infer: { term: Expression; ty: TypeExpression } }
//   | { check: { term: Expression; ty: TypeExpression } }
//   | {
//       infcall: {
//         func_ty: TypeExpression;
//         args: Expression[];
//         return_ty: TypeExpression;
//       };
//     };

// type TypeVar =
//   | { existential: string }
//   | { solved: TypeExpression }
//   | { marker: string };

// type Work = { judgement: Judgement };

// type AliasEntry = {
//   params: string[];
//   body: TypeExpression;
//   base_env: Environment;
// };
// type ConstructorEntry = {
//   arity: number;
//   decl: EnumDeclaration;
//   base_env: Environment;
// };
// type TraitEntry = { params: string[]; fns: TraitFn[]; base_env: Environment };

// export class Environment {
//   id: bigint = 0n;
//   aliases = new Map<string, AliasEntry>();
//   constructors = new Map<string, ConstructorEntry>();
//   traits = new Map<string, TraitEntry>();
//   etvars = new Map<string, TypeVar>();
//   utvars = new Set<string>();

//   clone(): Environment {
//     const env = new Environment();
//     env.id = this.id;
//     env.aliases = new Map([...this.aliases]);
//     env.constructors = new Map([...this.constructors]);
//     env.traits = new Map([...this.traits]);
//     env.etvars = new Map([...this.etvars]);
//     env.utvars = new Set([...this.utvars]);
//     return env;
//   }

//   // lookup methods
//   get_alias(name: string) {
//     return this.aliases.get(name) ?? null;
//   }
//   get_constructor(name: string) {
//     return this.constructors.get(name) ?? null;
//   }
//   get_trait(name: string) {
//     return this.traits.get(name) ?? null;
//   }
//   get_etvar(name: string) {
//     return this.etvars.get(name) ?? null;
//   }

//   // extend methods
//   set_alias(name: string, entry: AliasEntry) {
//     this.aliases.set(name, entry);
//     return this;
//   }
//   set_constructor(name: string, entry: ConstructorEntry) {
//     this.constructors.set(name, entry);
//     return this;
//   }
//   set_trait(name: string, entry: TraitEntry) {
//     this.traits.set(name, entry);
//     return this;
//   }
//   set_etvar(name: string, entry: TypeVar) {
//     this.etvars.set(name, entry);
//     return this;
//   }
//   set_utvar(name: string) {
//     this.utvars.add(name);
//     return this;
//   }

//   // has methods
//   has_alias(name: string): boolean {
//     return this.aliases.has(name);
//   }
//   has_constructor(name: string): boolean {
//     return this.constructors.has(name);
//   }
//   has_trait(name: string): boolean {
//     return this.traits.has(name);
//   }
//   has_etvar(name: string): boolean {
//     return this.etvars.has(name);
//   }
//   has_utvar(name: string): boolean {
//     return this.utvars.has(name);
//   }

//   fresh_var() {
//     return `${this.id++}`;
//   }

//   substitute(
//     t: TypeExpression,
//     subst: Map<string, TypeExpression>,
//   ): TypeExpression {
//     throw new Error(`Unexpected TypeExpression kind`, { cause: t });
//   }

//   normalize(t: TypeExpression, fuel = 256): TypeExpression {
//     if (fuel <= 0) return t;

//     throw new Error(`Unexpected TypeExpression kind`, { cause: t });
//   }
// }

// export class Worklist {
//   entries = [] as Work[];
//   log = [] as string[];

//   constructor(public env: Environment) {}

//   trace(message: string) {
//     this.log.push(message);
//     return this;
//   }

//   solve(name: string, solved: TypeExpression) {
//     const kind = this.env.get_etvar(name)!;
//     if (kind) {
//       if ("solved" in kind) return this;
//       this.env.set_etvar(name, { solved });
//       return this;
//     }

//     throw new Error(`UnboundVariable: Expected ${name}`);
//   }

//   check(term: Expression, ty: TypeExpression) {
//     this.push({ judgement: { check: { term, ty } } });
//   }

//   push(work: Work) {
//     return this.entries.push(work);
//   }

//   pop() {
//     return this.entries.pop() ?? null;
//   }

//   solve_judgement(judgement: Judgement) {
//     if ("subtype" in judgement) return this.solve_subtype(judgement.subtype);
//     if ("infer" in judgement) return this.solve_infer(judgement.infer);
//     if ("check" in judgement) return this.solve_check(judgement.check);
//     if ("infcall" in judgement) return this.solve_infcall(judgement.infcall);
//     throw new Error("Invalid Judgement Kind", { cause: judgement });
//   }

//   solve_subtype({ left, right }: SubtypeJudgement) {
//     left = this.resolve_alias(left);
//     right = this.resolve_alias(right);
//     if ("fn" in left && "fn" in right)
//       return this.solve_subtype_fn(left.fn, right.fn);
//   }

//   solve_subtype_fn(
//     [left_args, left_ret]: [TypeExpression[], TypeExpression],
//     [right_args, right_ret]: [TypeExpression[], TypeExpression],
//   ) {
//     if (left_args.length !== right_args.length)
//       throw new Error(`Argument lengths do not match`);
//     for (let i = 0; i < left_args.length; i++) {
//       const left = left_args[i]!;
//       const right = right_args[i]!;
//       this.push({ judgement: { subtype: { left, right } } });
//     }

//     this.push({ judgement: { subtype: { left: left_ret, right: right_ret } } });
//   }
// }
