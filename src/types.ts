// compiler.ts
import type {
  Declaration,
  Module,
  FnDeclaration,
  TypeDeclaration,
  LetDeclaration,
  TraitDeclaration,
  ImplDeclaration,
  EnumDeclaration,
  ImportDeclaration,
} from "./parser.js"; // Adjust the import path
import { parse_file, parse_file_streaming } from "./parser.js";
import {
  type Type,
  type Result,
  type Context,
  typecheck,
} from "./types_system_f_omega.js";

export class Compiler {}
