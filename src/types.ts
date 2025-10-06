import * as path from "node:path";
import {
  parse_file_streaming,
  type Declaration,
  type Import,
  type Module,
  type TypeExpression,
} from "./parser.js";
import type { Type } from "./types_system_f_omega.js";
export type CompilerOptions = {
  basedir: string;
  entries: string[];
};

export type FSInterface = {
  join: (...args: string[]) => string;
  relative: (from: string, to: string) => string;
  isAbsolute: (path: string) => boolean;
  resolve: (path: string) => string;
  readFile: (path: string) => Promise<string>;
  readDir: (path: string) => Promise<string[]>;
};

export const nodefs = () =>
  ({
    isAbsolute: path.isAbsolute,
    join: path.join,
    async readDir(dir) {
      const fs = await import("node:fs/promises");
      const contents = await fs.readdir(dir);
      return contents;
    },
    async readFile(path) {
      const fs = await import("node:fs/promises");
      return await fs.readFile(path, "utf-8");
    },
    relative: path.relative,
    resolve: path.resolve,
  }) as FSInterface;

export type CompilerOutput = {};

const declarationPath = (relativeModule: string, name: string) =>
  `${relativeModule}~${name}`;

export class Compiler {
  basedir: string;
  fs: FSInterface;
  declarations = new Map<string, Declaration>();

  exported = new Set<string>();
  entries = new Set<string>();

  constructor(options: Partial<CompilerOptions>, fs: FSInterface) {
    this.basedir = options.basedir ?? ".";
    this.fs = fs;

    for (const entry of options.entries ?? ["./src/index.dew"]) {
      const relativeEntry = fs.isAbsolute(entry)
        ? fs.relative(this.basedir, entry)
        : entry;

      this.entries.add(relativeEntry);
    }
  }

  async compile(): Promise<CompilerOutput> {
    await this.parsePass();
    return {};
  }

  async parsePass() {
    const parsed = new Set<string>();
    const queue: string[] = Array.from(this.entries);

    while (queue.length > 0) {
      const currentFile = queue.shift()!;

      if (parsed.has(currentFile)) {
        continue;
      }

      parsed.add(currentFile);

      // Read and parse the file
      const filePath = this.fs.join(this.basedir, currentFile);
      const content = await this.fs.readFile(filePath);

      // Get the module path (without .dew extension)
      const modulePath = currentFile.replace(/\.dew$/, "");

      // Process declarations
      for await (const declaration of parse_file_streaming(content)) {
        const declarationName = this.getDeclarationName(declaration);

        if (declarationName) {
          const key = declarationPath(modulePath, declarationName);
          this.declarations.set(key, declaration);

          // Track exports
          if (this.isExported(declaration)) {
            this.exported.add(key);
          }
        }

        // Extract and queue imports
        if ("import_dec" in declaration) {
          const importPath = this.resolveImport(
            declaration.import_dec.import_from,
            currentFile,
          );

          if (importPath && !parsed.has(importPath)) {
            queue.push(importPath);
          }
        }
      }
    }
  }

  private getDeclarationName(declaration: Declaration): string | null {
    if ("fn" in declaration) {
      return declaration.fn.fn.name?.name ?? null;
    }
    if ("enum" in declaration) {
      return declaration.enum.id.type;
    }
    if ("type_dec" in declaration) {
      return declaration.type_dec.id.type;
    }
    if ("let_dec" in declaration) {
      return declaration.let_dec.id.name;
    }
    if ("trait" in declaration) {
      return declaration.trait.id.type;
    }
    if ("impl" in declaration) {
      return declaration.impl.name.type;
    }
    if ("import_dec" in declaration) {
      return null; // Imports don't have a single name
    }
    return null;
  }

  private isExported(declaration: Declaration): boolean {
    if ("fn" in declaration) {
      return declaration.fn.pub;
    }
    if ("enum" in declaration) {
      return declaration.enum.pub;
    }
    if ("type_dec" in declaration) {
      return declaration.type_dec.pub;
    }
    if ("let_dec" in declaration) {
      return declaration.let_dec.pub;
    }
    if ("trait" in declaration) {
      return declaration.trait.pub;
    }
    if ("impl" in declaration) {
      return false; // impl blocks aren't directly exported
    }
    return false;
  }

  private resolveImport(
    importFrom: string,
    currentFile: string,
  ): string | null {
    // Handle relative imports
    if (importFrom.startsWith("./") || importFrom.startsWith("../")) {
      const currentDir = currentFile.split("/").slice(0, -1).join("/");
      const resolved = this.fs.join(currentDir, importFrom);

      // Add .dew extension if not present
      return resolved.endsWith(".dew") ? resolved : `${resolved}.dew`;
    }

    // Handle standard library imports (e.g., "std/module")
    // Skip these for now as they're not in the local filesystem
    if (importFrom.startsWith("std/")) {
      return null;
    }

    // Treat as relative to basedir
    const resolved = importFrom;
    return resolved.endsWith(".dew") ? resolved : `${resolved}.dew`;
  }
}
