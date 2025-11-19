import {
  showImport,
  type ConstructorImport,
  type Declaration,
  type EnumImport,
  type FnImport,
  type Import,
  type ImportDeclaration,
  type Module,
  type TypeImport,
} from "../parser.js";
import type { Scope } from "../util.js";
import { BaseWalker } from "../visitor.js";

export class ExportsPass extends BaseWalker {
  currentScope!: Scope;
  importScope!: Scope;

  override walkModule(node: Module): void {
    const currentScope = this.context.scopes.get(node);
    if (!currentScope)
      throw new Error(`Scope not created for ${this.modulePath}`);
    this.currentScope = currentScope;
    super.walkModule(node);
  }

  override walkDeclaration(node: Declaration): void {
    if ("import_dec" in node) this.walkImportDeclaration(node);
    else return;
  }

  override walkImportDeclaration(node: ImportDeclaration): void {
    if (node.import_dec.import_from.startsWith("@std/")) return;

    const importSource = node.import_dec.import_from;
    const importModule = this.context.modules.findModule(
      this.modulePath,
      importSource,
    );

    if (!importModule) {
      this.errors.push({
        invalid_name: node,
      });
      return;
    }

    const importedModuleScope = this.getScope(importModule.module!);
    if (!importedModuleScope)
      throw new Error(`Module has no scope ${importModule.relativePath}`);

    this.importScope = importedModuleScope;
    super.walkImportDeclaration(node);
  }

  override walkImport(node: Import): void {
    super.walkImport(node);
    if (!this.context.imports.has(node))
      throw new Error(`Import not handled: ${showImport(node)}`);
  }

  override walkFnImport(node: FnImport): void {
    const name =
      "string" in node.fn.name ? node.fn.name.string : node.fn.name.name;
    const alias = node.fn.alias?.name ?? name;
    const importedFn = this.importScope.getTerm(name);

    if (!importedFn) {
      this.errors.push({ unbound: { name, scope: this.importScope! } });
      return;
    }
    this.context.imports.set(node, importedFn);
    if (
      "fn" in importedFn ||
      "trait_fn" in importedFn ||
      "builtin" in importedFn
    ) {
      this.currentScope.terms.set(alias, importedFn);
    } else {
      this.errors.push({ not_a_function: node });
    }
  }

  override walkConstructorImport(node: ConstructorImport): void {
    const name = node.constr.name.type;
    const alias = node.constr.alias?.type ?? name;

    const importedConstructor = this.importScope.getTerm(name);
    if (!importedConstructor) {
      this.errors.push({ unbound: { name, scope: this.importScope } });
      return;
    }

    this.context.imports.set(node, importedConstructor);

    if ("variant" in importedConstructor) {
      this.currentScope.terms.set(alias, importedConstructor);
    } else {
      this.errors.push({ not_a_constructor: node });
    }
  }

  override walkTypeImport(node: TypeImport): void {
    const name = node.type.name.type;
    const alias = node.type.alias?.type ?? name;

    const importedType = this.importScope.getType(name);
    if (!importedType) {
      this.errors.push({ unbound: { name, scope: this.importScope } });
      return;
    }

    this.context.imports.set(node, importedType);
    if (
      "enum" in importedType ||
      "trait" in importedType ||
      "type" in importedType
    ) {
      this.currentScope.types.set(alias, importedType);
    } else {
      this.errors.push({ not_a_type: node });
    }
  }

  override walkEnumImport(node: EnumImport): void {
    const name = node.enum.name.type;
    const alias = node.enum.alias?.type ?? name;

    const importedType = this.importScope.getType(name);
    if (!importedType) {
      this.errors.push({ unbound: { name, scope: this.importScope } });
      return;
    }

    this.context.imports.set(node, importedType);

    if ("enum" in importedType) {
      this.currentScope.types.set(alias, importedType);
    } else {
      this.errors.push({ not_a_type: node });
    }
  }
}
