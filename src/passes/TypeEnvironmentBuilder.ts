// passes/TypeEnvironmentBuilder.ts
import type { Context, Binding, Type, Kind } from "../types_system_f_omega.js";
import type { SymbolTable, ModuleSymbol } from "./SymbolResolutionPass.js";
import type { ImportedSymbol } from "./ImportResolutionPass.js";
import { con_type } from "../types_system_f_omega.js";

export class TypeEnvironmentBuilder {
  buildContext(
    symbolTable: SymbolTable,
    imports: ImportedSymbol[],
    typeCache: Map<string, Type>,
  ): Context {
    const context: Context = [];

    // Add built-in types
    context.push({ type: { name: "Int", kind: { star: null } } });
    context.push({ type: { name: "Bool", kind: { star: null } } });
    context.push({ type: { name: "String", kind: { star: null } } });
    context.push({ type: { name: "Float", kind: { star: null } } });
    context.push({ type: { name: "Unit", kind: { star: null } } });

    // Add imported symbols
    for (const imp of imports) {
      if (imp.symbol.type) {
        if (imp.symbol.kind === "fn" || imp.symbol.kind === "let") {
          context.push({
            term: { name: imp.localName, type: imp.symbol.type },
          });
        } else if (
          imp.symbol.kind === "type" ||
          imp.symbol.kind === "enum" ||
          imp.symbol.kind === "trait"
        ) {
          context.push({
            type: { name: imp.localName, kind: { star: null } },
          });
        }
      }
    }

    // Add local symbols (that have been typed)
    for (const [name, symbol] of symbolTable.getAll()) {
      if (symbol.type && !imports.some((imp) => imp.localName === name)) {
        if (symbol.kind === "fn" || symbol.kind === "let") {
          context.push({ term: { name, type: symbol.type } });
        } else if (
          symbol.kind === "type" ||
          symbol.kind === "enum" ||
          symbol.kind === "trait"
        ) {
          context.push({ type: { name, kind: { star: null } } });
        }
      }
    }

    return context;
  }
}
