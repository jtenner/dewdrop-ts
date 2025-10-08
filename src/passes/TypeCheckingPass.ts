// passes/TypeCheckingPass.ts
import type { Module, Declaration, FnDeclaration } from "../parser.js";
import { BaseVisitor } from "../visitor.js";
import type { Context, Type, TypingError } from "../types_system_f_omega.js";
import { typecheck, showType } from "../types_system_f_omega.js";
import { ASTToSystemFOmega } from "./ASTToSystemFOmega.js";
import type { SymbolTable } from "./SymbolResolutionPass.js";
import type { ImportedSymbol } from "./ImportResolutionPass.js";
import { TypeEnvironmentBuilder } from "./TypeEnvironmentBuilder.js";

export class TypeCheckingPass extends BaseVisitor {
  private converter = new ASTToSystemFOmega();
  private envBuilder = new TypeEnvironmentBuilder();
  private context: Context = [];
  private currentModule: string = "";
  private errors: Array<{ module: string; error: TypingError | Error }> = [];

  constructor(
    private symbolTables: Map<string, SymbolTable>,
    private imports: Map<string, ImportedSymbol[]>,
    private typeCache: Map<string, Type>,
  ) {
    super();
  }

  check(modulePath: string, module: Module): boolean {
    this.currentModule = modulePath;
    const symbolTable = this.symbolTables.get(modulePath)!;
    const moduleImports = this.imports.get(modulePath) ?? [];

    // Build type environment
    this.context = this.envBuilder.buildContext(
      symbolTable,
      moduleImports,
      this.typeCache,
    );

    console.log(`  Type checking module: ${modulePath}`);

    try {
      this.visitModule(module);
      return this.errors.length === 0;
    } catch (error) {
      this.errors.push({
        module: modulePath,
        error: error instanceof Error ? error : new Error(String(error)),
      });
      return false;
    }
  }

  getErrors() {
    return this.errors;
  }

  override visitFnDeclaration(node: FnDeclaration): Declaration {
    const fn = node.fn.fn;
    const fnName = fn.name!.name;

    // Convert function body to System F-omega term
    const term = this.converter.convertTerm(fn.body);
    if ("err" in term) {
      this.errors.push({
        error: new Error(term.err),
        module: this.currentModule,
      });
      return node;
    }
    // Build context with function parameters
    const fnContext: Context = [...this.context];

    for (const param of fn.params) {
      if (param.guard) {
        const paramType = this.converter.convertType(param.guard);
        if ("err" in paramType)
          this.errors.push({
            error: new Error(paramType.err),
            module: this.currentModule,
          });
        else
          fnContext.push({
            term: { name: param.name.name, type: paramType.ok },
          });
      }
    }

    // Type check the function body
    const result = typecheck(fnContext, term.ok);

    if ("err" in result) {
      console.error(`      Error in ${fnName}:`, result.err);
      this.errors.push({
        module: this.currentModule,
        error: result.err,
      });
    } else {
      console.log(`      ${fnName} : ${showType(result.ok)}`);

      // Cache the type in the symbol table
      const symbolTable = this.symbolTables.get(this.currentModule)!;
      const symbol = symbolTable.lookup(fnName);
      if (symbol) {
        symbol.type = result.ok;
        this.typeCache.set(`${this.currentModule}::${fnName}`, result.ok);
      }

      // Add to context for subsequent declarations
      this.context.push({
        term: { name: fnName, type: result.ok },
      });
    }
    return node;
  }

  override visitLetDeclaration(node: any): Declaration {
    const letName = node.let_dec.id.name;
    console.log(`    Checking let binding: ${letName}`);

    try {
      const term = this.converter.convertTerm(node.let_dec.value);
      if ("err" in term) {
        this.errors.push({
          error: new Error(term.err),
          module: this.currentModule,
        });
        return node;
      }
      const result = typecheck(this.context, term.ok);

      if ("err" in result) {
        console.error(`      Error in ${letName}:`, result.err);
        this.errors.push({
          module: this.currentModule,
          error: result.err,
        });
      } else {
        console.log(`      ${letName} : ${showType(result.ok)}`);

        // Cache the type
        const symbolTable = this.symbolTables.get(this.currentModule)!;
        const symbol = symbolTable.lookup(letName);
        if (symbol) {
          symbol.type = result.ok;
          this.typeCache.set(`${this.currentModule}::${letName}`, result.ok);
        }

        // Add to context
        this.context.push({
          term: { name: letName, type: result.ok },
        });
      }
    } catch (error) {
      console.error(`      Exception in ${letName}:`, error);
      this.errors.push({
        module: this.currentModule,
        error: error instanceof Error ? error : new Error(String(error)),
      });
    }

    return node;
  }

  override visitEnumDeclaration(node: any): Declaration {
    // Register enum type in context
    const enumName = node.enum.id.type;
    console.log(`    Registering enum: ${enumName}`);

    this.context.push({
      type: { name: enumName, kind: { star: null } },
    });

    return node;
  }

  override visitTypeDeclaration(node: any): Declaration {
    const typeName = node.type_dec.id.type;
    console.log(`    Registering type alias: ${typeName}`);

    this.context.push({
      type: { name: typeName, kind: { star: null } },
    });

    return node;
  }
}
