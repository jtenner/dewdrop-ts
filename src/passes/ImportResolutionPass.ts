import * as path from "node:path";
import type { Declaration, ImportDeclaration, Module } from "../parser.js";
import { BaseVisitor } from "../visitor.js";
import type { ModuleSymbol, SymbolTable } from "./SymbolResolutionPass.js";

const stddir = path.resolve(path.join(import.meta.dir, "../../std"));

export type ImportedSymbol = {
  localName: string;
  originalName: string;
  sourceModule: string;
  symbol: ModuleSymbol;
};

export class ImportResolutionPass extends BaseVisitor {
  private currentModule: string = "";
  private imports = new Map<string, ImportedSymbol[]>(); // module -> imported symbols

  constructor(
    private symbolTables: Map<string, SymbolTable>,
    private moduleDir: string,
  ) {
    super();
  }

  resolve(modulePath: string, module: Module) {
    this.currentModule = modulePath;
    this.moduleDir = path.dirname(modulePath);
    this.imports.set(modulePath, []);
    this.visitModule(module);
  }

  getImports(modulePath: string): ImportedSymbol[] {
    return this.imports.get(modulePath) ?? [];
  }

  override visitImportDeclaration(node: ImportDeclaration): Declaration {
    let { import_from, imports } = node.import_dec;

    // Resolve import path
    if (!import_from.toLowerCase().endsWith(".dew")) {
      import_from += ".dew";
    }

    let resolvedPath: string;
    if (import_from.startsWith("@std/")) {
      resolvedPath = path.join(stddir, import_from.slice("@std/".length));
    } else if (path.isAbsolute(import_from)) {
      resolvedPath = import_from;
    } else {
      resolvedPath = path.join(this.moduleDir, import_from);
    }

    const sourceTable = this.symbolTables.get(resolvedPath);
    if (!sourceTable) {
      throw new Error(`Cannot resolve module: ${import_from}`);
    }

    const currentImports = this.imports.get(this.currentModule)!;

    for (const imp of imports) {
      if ("name" in imp) {
        const { name, alias } = imp.name;
        const localName = alias?.name ?? name.name;
        const symbol = sourceTable.lookup(name.name);

        if (!symbol) {
          throw new Error(
            `Symbol '${name.name}' not found in module '${import_from}'`,
          );
        }

        if (!symbol.public) {
          throw new Error(
            `Symbol '${name.name}' is not exported from '${import_from}'`,
          );
        }

        currentImports.push({
          localName,
          originalName: name.name,
          sourceModule: resolvedPath,
          symbol,
        });
      } else if ("type" in imp) {
        const { name, alias } = imp.type;
        const localName = alias?.type ?? name.type;
        const symbol = sourceTable.lookup(name.type);

        if (!symbol) {
          throw new Error(
            `Type '${name.type}' not found in module '${import_from}'`,
          );
        }

        if (!symbol.public) {
          throw new Error(
            `Type '${name.type}' is not exported from '${import_from}'`,
          );
        }

        currentImports.push({
          localName,
          originalName: name.type,
          sourceModule: resolvedPath,
          symbol,
        });
      }
      // Handle other import types (fn, trait, etc.)
    }

    return node;
  }
}
