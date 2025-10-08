import type {
  Declaration,
  EnumDeclaration,
  FnDeclaration,
  LetDeclaration,
  Module,
  TraitDeclaration,
  TypeDeclaration,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export type ModuleSymbol = {
  name: string;
  kind: "fn" | "type" | "enum" | "trait" | "let" | "param";
  declared_in: string; // module path
  public: boolean;
  type?: any; // Will be filled during type checking
};

export class SymbolTable {
  private symbols = new Map<string, ModuleSymbol>();
  private scopes: Map<string, ModuleSymbol>[] = [new Map()]; // Stack of scopes

  enterScope() {
    this.scopes.push(new Map());
  }

  exitScope() {
    this.scopes.pop();
  }

  define(name: string, symbol: ModuleSymbol) {
    const currentScope = this.scopes[this.scopes.length - 1]!;
    if (currentScope.has(name)) {
      throw new Error(`Symbol '${name}' already defined in current scope`);
    }
    currentScope.set(name, symbol);
    this.symbols.set(name, symbol);
  }

  lookup(name: string): ModuleSymbol | undefined {
    // Search from innermost to outermost scope
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      const symbol = this.scopes[i]!.get(name);
      if (symbol) return symbol;
    }
    return undefined;
  }

  getAll(): Map<string, ModuleSymbol> {
    return this.symbols;
  }
}

export class SymbolResolutionPass extends BaseVisitor {
  private currentModule: string = "";

  constructor(
    public symbolTables: Map<string, SymbolTable>,
    public modules: Map<string, Module>,
  ) {
    super();
  }

  resolve(modulePath: string, module: Module) {
    this.currentModule = modulePath;
    const table = new SymbolTable();
    this.symbolTables.set(modulePath, table);
    this.visitModule(module);
  }

  private getCurrentTable(): SymbolTable {
    return this.symbolTables.get(this.currentModule)!;
  }

  override visitFnDeclaration(node: FnDeclaration): Declaration {
    const table = this.getCurrentTable();
    if (!node.fn.fn.name) return super.visitFnDeclaration(node);

    table.define(node.fn.fn.name.name, {
      name: node.fn.fn.name.name,
      kind: "fn",
      declared_in: this.currentModule,
      public: node.fn.pub,
    });
    return super.visitFnDeclaration(node);
  }

  override visitEnumDeclaration(node: EnumDeclaration): Declaration {
    const table = this.getCurrentTable();
    table.define(node.enum.id.type, {
      name: node.enum.id.type,
      kind: "enum",
      declared_in: this.currentModule,
      public: node.enum.pub,
    });
    return super.visitEnumDeclaration(node);
  }

  override visitTypeDeclaration(node: TypeDeclaration): Declaration {
    const table = this.getCurrentTable();
    table.define(node.type_dec.id.type, {
      name: node.type_dec.id.type,
      kind: "type",
      declared_in: this.currentModule,
      public: node.type_dec.pub,
    });
    return super.visitTypeDeclaration(node);
  }

  override visitLetDeclaration(node: LetDeclaration): Declaration {
    const table = this.getCurrentTable();
    table.define(node.let_dec.id.name, {
      name: node.let_dec.id.name,
      kind: "let",
      declared_in: this.currentModule,
      public: node.let_dec.pub,
    });
    return super.visitLetDeclaration(node);
  }

  override visitTraitDeclaration(node: TraitDeclaration): Declaration {
    const table = this.getCurrentTable();
    table.define(node.trait.id.type, {
      name: node.trait.id.type,
      kind: "trait",
      declared_in: this.currentModule,
      public: node.trait.pub,
    });
    return super.visitTraitDeclaration(node);
  }
}
