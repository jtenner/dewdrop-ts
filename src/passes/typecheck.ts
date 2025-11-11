import {
  arrowKind,
  type Binding,
  checkKind,
  checkPattern,
  enumDefBinding,
  inferType,
  showContext,
  showTerm,
  showType,
  starKind,
  termBinding,
  traitDefBinding,
  typeAliasBinding,
  typecheck,
} from "system-f-omega";
import {
  type BuiltinDeclaration,
  type Declaration,
  type EnumDeclaration,
  type EnumVariant,
  type FnDeclaration,
  type LetDeclaration,
  type Module,
  showDeclaration,
  showEnumVariant,
  showExpression,
  showPatternExpression,
  type TraitDeclaration,
  type TypeDeclaration,
} from "../parser.js";
import { BaseWalker } from "../visitor.js";

export class TypeCheckPass extends BaseWalker {
  private env: Binding[] = [];

  override walkModule(node: Module): void {
    this.env = [];
    super.walkModule(node);
  }

  override walkTraitDeclaration(node: TraitDeclaration): void {}

  override walkFnDeclaration(node: FnDeclaration): void {
    const term = this.context.terms.get(node.fn.fn);
    if (!term)
      throw new Error(`Term not elaborated for ${showDeclaration(node)}`);

    const typeRes = inferType(this.env, term);
    if ("err" in typeRes) {
      console.log("Term is", showTerm(term));
      console.log("Context is", showContext(this.env));
      console.log(typeRes);
      this.errors.push({ type_error: typeRes.err });
      return;
    }
    const binding = termBinding(node.fn.fn.name!.name, typeRes.ok);
    this.env.push(binding);
    this.context.bindings.set(node, [binding]);
  }

  override walkEnumDeclaration(node: EnumDeclaration): void {
    const type = this.context.types.get(node);
    if (!type)
      throw new Error(`Type not elaborated for enum: ${showDeclaration(node)}`);

    let kind = starKind;
    for (const _ of node.enum.type_params) kind = arrowKind(starKind, kind);

    const enumBinding = enumDefBinding(
      node.enum.id.type,
      kind,
      node.enum.type_params.map((t) => t.name),
      node.enum.variants.map((t) => {
        const ty = this.context.types.get(t);
        if (!ty)
          throw new Error(
            `Type not elaborated for variant: ${showEnumVariant(t)}`,
          );
        const name = "fields" in t ? t.fields.id.type : t.values.id.type;
        return [name, ty];
      }),
    );
    this.env.push(enumBinding);
    this.context.bindings.set(node, [enumBinding]);
    super.walkEnumDeclaration(node);
  }

  override walkEnumVariant(node: EnumVariant): void {
    const term = this.context.terms.get(node);
    if (!term)
      throw new Error(
        `Term not elaborated for EnumVariant: ${showEnumVariant(node)}`,
      );
    const type = this.context.types.get(node);
    if (!type)
      throw new Error(
        `Type not elaborated for EnumVariant: ${showEnumVariant(node)}`,
      );
    const name = "fields" in node ? node.fields.id.type : node.values.id.type;

    // The constructor function is defined at this point
    console.log("type checking term", showTerm(term));
    const constructorTypeRes = typecheck(this.env, term);
    if ("err" in constructorTypeRes) {
      this.errors.push({ type_error: constructorTypeRes.err });
      return;
    }
    const binding = termBinding(name, constructorTypeRes.ok);
    this.env.push(binding);
    this.context.bindings.set(node, [binding]);
  }

  override walkBuiltinDeclaration(node: BuiltinDeclaration): void {
    const builtinType = this.context.types.get(node);
    if (!builtinType)
      throw new Error(
        `Type not elaborated for builtin: ${showDeclaration(node)}`,
      );

    const binding = termBinding(node.builtin.alias.name, builtinType);
    this.env.push(binding);
    this.context.bindings.set(node, [binding]);
  }

  override walkLetDeclaration(node: LetDeclaration): void {
    const term = this.context.terms.get(node.let_dec.value);
    if (!term)
      throw new Error(
        `Term not elaborated for expression: ${showExpression(node.let_dec.value)}`,
      );
    const pattern = this.context.patterns.get(node.let_dec.pattern);
    if (!pattern)
      throw new Error(
        `Pattern not elaborated: ${showPatternExpression(node.let_dec.pattern)}`,
      );

    const typeRes = inferType(this.env, term);
    if ("err" in typeRes) {
      this.errors.push({ type_error: typeRes.err });
      return;
    }

    const patternBindings = checkPattern(pattern, typeRes.ok, this.env);
    if ("err" in patternBindings) {
      this.errors.push({ type_error: patternBindings.err });
      return;
    }

    for (const bind of patternBindings.ok) {
      this.env.push(bind);
    }
    this.context.bindings.set(node, patternBindings.ok);
  }

  override walkTypeDeclaration(node: TypeDeclaration): void {
    const name = node.type_dec.id.type;
    const body = this.context.types.get(node);
    const kinds = node.type_dec.params.map(() => starKind);
    const params = node.type_dec.params.map((t) => t.name);
    if (!body)
      throw new Error(
        `No type elaborated for type declaration: ${showDeclaration(node)}`,
      );

    const binding = typeAliasBinding(name, params, kinds, body);
    this.env.push(binding);
    this.context.bindings.set(node, [binding]);
  }

  override walkDeclaration(node: Declaration): void {
    super.walkDeclaration(node);
    if (!this.context.bindings.has(node))
      throw new Error(
        `No bindings generated for declaration: ${showDeclaration(node)}`,
      );
  }
}
