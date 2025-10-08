import type { NameToken, StringToken, TypeToken } from "./lexer.js";
import type {
  ApplicationTypeExpression,
  ArrowKindBodyExpression,
  AssignBodyExpression,
  BlockExpression,
  BodyExpression,
  BoolExpression,
  BuiltinImport,
  CallExpression,
  ConstructorExpression,
  ConstructorPatternExpression,
  Declaration,
  EnumDeclaration,
  EnumVariant,
  Expression,
  ExpressionBodyExpression,
  FloatExpression,
  FloatPatternExpression,
  Fn,
  FnDeclaration,
  FnExpression,
  FnImport,
  FnParam,
  FnSignature,
  FnTypeExpression,
  GlobalImport,
  IfExpression,
  ImplDeclaration,
  Import,
  ImportDeclaration,
  InfixExpression,
  Int,
  IntExpression,
  IntPatternExpression,
  LetBindBodyExpression,
  LetDeclaration,
  MatchArm,
  MatchExpression,
  MemoryImport,
  Module,
  NamedTypeExpression,
  NameIdentifier,
  NameImport,
  PatternExpression,
  PostfixExpression,
  PrefixExpression,
  RecordExpression,
  RecordPatternExpression,
  RecordTypeExpression,
  SelectExpression,
  SelectTypeExpression,
  SelfExpression,
  StarImport,
  StringPatternExpression,
  TableImport,
  TraitDeclaration,
  TraitFn,
  TraitImport,
  TupleExpression,
  TuplePatternExpression,
  TupleTypeExpression,
  TypeDeclaration,
  TypeExpression,
  TypeImport,
} from "./parser.js";

// Visitor interface with transform capability
export interface ASTVisitor {
  // Module
  visitModule?(node: Module): Module;

  // Declarations
  visitFnDeclaration?(node: FnDeclaration): Declaration;
  visitEnumDeclaration?(node: EnumDeclaration): Declaration;
  visitImportDeclaration?(node: ImportDeclaration): Declaration;
  visitTypeDeclaration?(node: TypeDeclaration): Declaration;
  visitLetDeclaration?(node: LetDeclaration): Declaration;
  visitTraitDeclaration?(node: TraitDeclaration): Declaration;
  visitImplDeclaration?(node: ImplDeclaration): Declaration;

  // Type Expressions
  visitNameTypeExpression?(node: NameIdentifier): TypeExpression;
  visitSelectTypeExpression?(node: SelectTypeExpression): TypeExpression;
  visitApplicationTypeExpression?(
    node: ApplicationTypeExpression,
  ): TypeExpression;
  visitFnTypeExpression?(node: FnTypeExpression): TypeExpression;
  visitRecordTypeExpression?(node: RecordTypeExpression): TypeExpression;
  visitTupleTypeExpression?(node: TupleTypeExpression): TypeExpression;

  // Body Expressions
  visitArrowKindBodyExpression?(node: ArrowKindBodyExpression): BodyExpression;
  visitLetBindBodyExpression?(node: LetBindBodyExpression): BodyExpression;
  visitAssignBodyExpression?(node: AssignBodyExpression): BodyExpression;
  visitExpressionBodyExpression?(
    node: ExpressionBodyExpression,
  ): BodyExpression;

  // Expressions
  visitNameExpression?(node: NameIdentifier): Expression;
  visitConstructorExpression?(node: ConstructorExpression): Expression;
  visitCallExpression?(node: CallExpression): Expression;
  visitBlockExpression?(node: BlockExpression): Expression;
  visitIfExpression?(node: IfExpression): Expression;
  visitSelectExpression?(node: SelectExpression): Expression;
  visitMatchExpression?(node: MatchExpression): Expression;
  visitFloatExpression?(node: FloatExpression): Expression;
  visitIntExpression?(node: IntExpression): Expression;
  visitBoolExpression?(node: BoolExpression): Expression;
  visitPrefixExpression?(node: PrefixExpression): Expression;
  visitPostfixExpression?(node: PostfixExpression): Expression;
  visitInfixExpression?(node: InfixExpression): Expression;
  visitStringExpression?(node: StringToken): Expression;
  visitFnExpression?(node: FnExpression): Expression;
  visitRecordExpression?(node: RecordExpression): Expression;
  visitTupleExpression?(node: TupleExpression): Expression;
  visitSelfExpression?(node: SelfExpression): Expression;

  // Pattern Expressions
  visitNamePatternExpression?(node: NameIdentifier): PatternExpression;
  visitConstructorPatternExpression?(
    node: ConstructorPatternExpression,
  ): PatternExpression;
  visitIntPatternExpression?(node: IntPatternExpression): PatternExpression;
  visitFloatPatternExpression?(node: FloatPatternExpression): PatternExpression;
  visitStringPatternExpression?(
    node: StringPatternExpression,
  ): PatternExpression;
  visitRecordPatternExpression?(
    node: RecordPatternExpression,
  ): PatternExpression;
  visitTuplePatternExpression?(node: TuplePatternExpression): PatternExpression;

  // Other nodes
  visitMatchArm?(node: MatchArm): MatchArm;
  visitEnumVariant?(node: EnumVariant): EnumVariant;
  visitImport?(node: Import): Import;
  visitFn?(node: Fn): Fn;
  visitTraitFn?(node: TraitFn): TraitFn;
  visitFnParam?(node: FnParam): FnParam;
  visitNamedTypeExpression?(node: NamedTypeExpression): NamedTypeExpression;

  visitTypeImport?(node: TypeImport): Import;
  visitFnImport?(node: FnImport): Import;
  visitGlobalImport?(node: GlobalImport): Import;
  visitTableImport?(node: TableImport): Import;
  visitStarImport?(node: StarImport): Import;
  visitMemoryImport?(node: MemoryImport): Import;
  visitNameImport?(node: NameImport): Import;
  visitTraitImport?(node: TraitImport): Import;
  visitBuiltinImport?(node: BuiltinImport): Import;

  visitFnSignature?(node: FnSignature): FnSignature;

  visitTypeIdentifier?(node: TypeToken): TypeToken;
  visitNameIdentifier?(node: NameToken): NameToken;
  visitString?(node: StringToken): StringToken;
  visitInt?(node: Int): Int;
}

// Default visitor that recursively visits and reconstructs the tree
export class BaseVisitor implements ASTVisitor {
  visitModule(node: Module): Module {
    return {
      module: node.module.map((decl) => this.visitDeclaration(decl)),
    };
  }

  visitDeclaration(node: Declaration): Declaration {
    if ("fn" in node) return this.visitFnDeclaration(node);
    if ("enum" in node) return this.visitEnumDeclaration(node);
    if ("import_dec" in node) return this.visitImportDeclaration(node);
    if ("type_dec" in node) return this.visitTypeDeclaration(node);
    if ("let_dec" in node) return this.visitLetDeclaration(node);
    if ("trait" in node) return this.visitTraitDeclaration(node);
    if ("impl" in node) return this.visitImplDeclaration(node);
    throw new Error("Unknown declaration type");
  }

  visitFnDeclaration(node: FnDeclaration): Declaration {
    return {
      fn: {
        pub: node.fn.pub,
        fn: this.visitFn(node.fn.fn),
      },
    };
  }

  visitEnumDeclaration(node: EnumDeclaration): Declaration {
    return {
      enum: {
        pub: node.enum.pub,
        id: node.enum.id,
        type_params: node.enum.type_params,
        variants: node.enum.variants.map((v) => this.visitEnumVariant(v)),
      },
    };
  }

  visitImportDeclaration(node: ImportDeclaration): Declaration {
    return {
      import_dec: {
        import_from: node.import_dec.import_from,
        imports: node.import_dec.imports.map((i) => this.visitImport(i)),
      },
    };
  }

  visitTypeDeclaration(node: TypeDeclaration): Declaration {
    return {
      type_dec: {
        pub: node.type_dec.pub,
        id: node.type_dec.id,
        params: node.type_dec.params,
        value: this.visitTypeExpression(node.type_dec.value),
      },
    };
  }

  visitLetDeclaration(node: LetDeclaration): Declaration {
    return {
      let_dec: {
        pub: node.let_dec.pub,
        id: node.let_dec.id,
        guard: node.let_dec.guard
          ? this.visitTypeExpression(node.let_dec.guard)
          : null,
        value: this.visitExpression(node.let_dec.value),
      },
    };
  }

  visitTraitDeclaration(node: TraitDeclaration): Declaration {
    return {
      trait: {
        pub: node.trait.pub,
        id: node.trait.id,
        type_params: node.trait.type_params,
        fns: node.trait.fns.map((f) => this.visitTraitFn(f)),
      },
    };
  }

  visitImplDeclaration(node: ImplDeclaration): Declaration {
    return {
      impl: {
        name: node.impl.name,
        type_params: node.impl.type_params.map((t) =>
          this.visitTypeExpression(t),
        ),
        for: this.visitTypeExpression(node.impl.for),
        fns: node.impl.fns.map((f) => this.visitFn(f)),
      },
    };
  }

  visitTypeExpression(node: TypeExpression): TypeExpression {
    if ("select" in node) return this.visitSelectTypeExpression(node);
    if ("app" in node) return this.visitApplicationTypeExpression(node);
    if ("fn" in node) return this.visitFnTypeExpression(node);
    if ("record" in node) return this.visitRecordTypeExpression(node);
    if ("tuple" in node) return this.visitTupleTypeExpression(node);
    return node; // NameIdentifier or TypeIdentifier
  }

  visitSelectTypeExpression(node: SelectTypeExpression): TypeExpression {
    return {
      select: {
        root: this.visitTypeExpression(node.select.root),
        name: node.select.name,
      },
    };
  }

  visitApplicationTypeExpression(
    node: ApplicationTypeExpression,
  ): TypeExpression {
    return {
      app: {
        callee: this.visitTypeExpression(node.app.callee),
        args: node.app.args.map((a) => this.visitTypeExpression(a)),
      },
    };
  }

  visitFnTypeExpression(node: FnTypeExpression): TypeExpression {
    return {
      fn: {
        args: node.fn.args.map((a) => this.visitTypeExpression(a)),
        ret: this.visitTypeExpression(node.fn.ret),
      },
    };
  }

  visitRecordTypeExpression(node: RecordTypeExpression): TypeExpression {
    return {
      record: node.record.map((f) => this.visitNamedTypeExpression(f)),
    };
  }

  visitTupleTypeExpression(node: TupleTypeExpression): TypeExpression {
    return {
      tuple: node.tuple.map((t) => this.visitTypeExpression(t)),
    };
  }

  visitNamedTypeExpression(node: NamedTypeExpression): NamedTypeExpression {
    return {
      name: node.name,
      ty: this.visitTypeExpression(node.ty),
    };
  }

  visitBodyExpression(node: BodyExpression): BodyExpression {
    if ("arrow_bind" in node) return this.visitArrowKindBodyExpression(node);
    if ("let_bind" in node) return this.visitLetBindBodyExpression(node);
    if ("assign" in node) return this.visitAssignBodyExpression(node);
    if ("expression" in node) return this.visitExpressionBodyExpression(node);
    throw new Error("Unknown body expression type");
  }

  visitArrowKindBodyExpression(node: ArrowKindBodyExpression): BodyExpression {
    return {
      arrow_bind: {
        name: node.arrow_bind.name,
        expression: this.visitExpression(node.arrow_bind.expression),
      },
    };
  }

  visitLetBindBodyExpression(node: LetBindBodyExpression): BodyExpression {
    return {
      let_bind: {
        pattern: this.visitPatternExpression(node.let_bind.pattern),
        assert: node.let_bind.assert,
        expression: this.visitExpression(node.let_bind.expression),
      },
    };
  }

  visitAssignBodyExpression(node: AssignBodyExpression): BodyExpression {
    return {
      assign: {
        name: node.assign.name,
        expression: this.visitExpression(node.assign.expression),
      },
    };
  }

  visitExpressionBodyExpression(
    node: ExpressionBodyExpression,
  ): BodyExpression {
    return {
      expression: this.visitExpression(node.expression),
    };
  }

  visitExpression(node: Expression): Expression {
    if ("constr" in node) return this.visitConstructorExpression(node); // Constructor
    if ("call" in node) return this.visitCallExpression(node);
    if ("block" in node) return this.visitBlockExpression(node);
    if ("if_expr" in node) return this.visitIfExpression(node);
    if ("select" in node) return this.visitSelectExpression(node);
    if ("match" in node) return this.visitMatchExpression(node);
    if ("float" in node) return this.visitFloatExpression(node);
    if ("int" in node) return this.visitIntExpression(node);
    if ("bool" in node) return this.visitBoolExpression(node);
    if ("prefix" in node) return this.visitPrefixExpression(node);
    if ("postfix" in node) return this.visitPostfixExpression(node);
    if ("infix" in node) return this.visitInfixExpression(node);
    if ("string" in node) return this.visitStringExpression(node);
    if ("fn" in node) return this.visitFnExpression(node);
    if ("record" in node) return this.visitRecordExpression(node);
    if ("tuple" in node) return this.visitTupleExpression(node);
    if ("self" in node) return node;
    return node; // NameIdentifier
  }

  visitConstructorExpression(node: ConstructorExpression): Expression {
    return node;
  }

  visitBoolExpression(node: BoolExpression): Expression {
    return node;
  }

  visitIntExpression(node: IntExpression): Expression {
    return node;
  }

  visitFloatExpression(node: FloatExpression): Expression {
    return node;
  }

  visitStringExpression(node: StringToken): Expression {
    return this.visitString(node);
  }

  visitCallExpression(node: CallExpression): Expression {
    return {
      call: [
        this.visitExpression(node.call[0]),
        node.call[1].map((e) => this.visitExpression(e)),
      ],
    };
  }

  visitBlockExpression(node: BlockExpression): Expression {
    return {
      block: node.block.map((b) => this.visitBodyExpression(b)),
    };
  }

  visitIfExpression(node: IfExpression): Expression {
    return {
      if_expr: {
        cond: this.visitExpression(node.if_expr.cond),
        if_body: this.visitExpression(node.if_expr.if_body),
        else_body:
          node.if_expr.else_body &&
          this.visitExpression(node.if_expr.else_body),
      },
    };
  }

  visitSelectExpression(node: SelectExpression): Expression {
    return {
      select: [this.visitExpression(node.select[0]), node.select[1]],
    };
  }

  visitMatchExpression(node: MatchExpression): Expression {
    return {
      match: [
        this.visitExpression(node.match[0]),
        node.match[1].map((a) => this.visitMatchArm(a)),
      ],
    };
  }

  visitPrefixExpression(node: PrefixExpression): Expression {
    return {
      prefix: {
        op: node.prefix.op,
        operand: this.visitExpression(node.prefix.operand),
      },
    };
  }

  visitPostfixExpression(node: PostfixExpression): Expression {
    return {
      postfix: {
        op: node.postfix.op,
        operand: this.visitExpression(node.postfix.operand),
      },
    };
  }

  visitInfixExpression(node: InfixExpression): Expression {
    return {
      infix: {
        op: node.infix.op,
        left: this.visitExpression(node.infix.left),
        right: this.visitExpression(node.infix.right),
      },
    };
  }

  visitFnExpression(node: FnExpression): Expression {
    return {
      fn: {
        params: node.fn.params.map((p) => this.visitFnParam(p)),
        return_type:
          node.fn.return_type && this.visitTypeExpression(node.fn.return_type),
        body: this.visitExpression(node.fn.body),
      },
    };
  }

  visitRecordExpression(node: RecordExpression): Expression {
    return {
      record: node.record.map(
        ([name, expr]) =>
          [name, this.visitExpression(expr)] as [NameIdentifier, Expression],
      ),
    };
  }

  visitTupleExpression(node: TupleExpression): Expression {
    return {
      tuple: node.tuple.map((e) => this.visitExpression(e)),
    };
  }

  visitSelfExpression(node: SelfExpression): Expression {
    return node;
  }

  visitPatternExpression(node: PatternExpression): PatternExpression {
    if ("constr" in node) return this.visitConstructorPatternExpression(node);
    if ("int" in node) return node;
    if ("float" in node) return node;
    if ("string" in node) return node;
    if ("record" in node) return this.visitRecordPatternExpression(node);
    if ("tuple" in node) return this.visitTuplePatternExpression(node);
    return node; // NameIdentifier
  }

  visitConstructorPatternExpression(
    node: ConstructorPatternExpression,
  ): PatternExpression {
    return {
      constr: [
        node.constr[0],
        node.constr[1].map((p) => this.visitPatternExpression(p)),
      ],
    };
  }

  visitRecordPatternExpression(
    node: RecordPatternExpression,
  ): PatternExpression {
    return {
      record: node.record.map(
        ([name, pattern]) =>
          [name, this.visitPatternExpression(pattern)] as [
            NameIdentifier,
            PatternExpression,
          ],
      ),
    };
  }

  visitTuplePatternExpression(node: TuplePatternExpression): PatternExpression {
    return {
      tuple: node.tuple.map((p) => this.visitPatternExpression(p)),
    };
  }

  visitMatchArm(node: MatchArm): MatchArm {
    return {
      pattern: this.visitPatternExpression(node.pattern),
      guard: node.guard && this.visitExpression(node.guard),
      body: this.visitExpression(node.body),
    };
  }

  visitEnumVariant(node: EnumVariant): EnumVariant {
    if ("fields" in node) {
      return {
        fields: {
          id: this.visitTypeIdentifier(node.fields.id),
          fields: node.fields.fields.map((f) =>
            this.visitNamedTypeExpression(f),
          ),
        },
      };
    }
    return {
      values: {
        id: this.visitTypeIdentifier(node.values.id),
        values: node.values.values.map((v) => this.visitTypeExpression(v)),
      },
    };
  }

  visitImport(node: Import): Import {
    if ("type" in node) return this.visitTypeImport(node);
    if ("fn" in node) return this.visitFnImport(node);
    if ("global" in node) return this.visitGlobalImport(node);
    if ("table" in node) return this.visitTableImport(node);
    if ("star" in node) return this.visitStarImport(node);
    if ("memory" in node) return this.visitMemoryImport(node);
    if ("name" in node) return this.visitNameImport(node);
    if ("trait" in node) return this.visitTraitImport(node);
    if ("builtin" in node) return this.visitBuiltinImport(node);
    return node;
  }

  visitTypeImport(node: TypeImport): Import {
    return {
      type: {
        alias: node.type.alias && this.visitTypeIdentifier(node.type.alias),
        name: this.visitTypeIdentifier(node.type.name),
      },
    };
  }

  visitFnImport(node: FnImport): Import {
    return {
      fn: {
        alias: node.fn.alias && this.visitNameIdentifier(node.fn.alias),
        name:
          "string" in node.fn.name
            ? this.visitString(node.fn.name)
            : this.visitNameIdentifier(node.fn.name),
        signature:
          node.fn.signature && this.visitFnSignature(node.fn.signature),
      },
    };
  }
  visitGlobalImport(node: GlobalImport): Import {
    return {
      global: {
        alias: node.global.alias && this.visitNameIdentifier(node.global.alias),
        global_type: this.visitTypeExpression(node.global.global_type),
        mut: node.global.mut,
        name:
          "string" in node.global.name
            ? this.visitString(node.global.name)
            : this.visitNameIdentifier(node.global.name),
      },
    };
  }
  visitTableImport(node: TableImport): Import {
    return {
      table: {
        alias: node.table.alias && this.visitNameIdentifier(node.table.alias),
        min: node.table.min && this.visitInt(node.table.min),
        max: node.table.max && this.visitInt(node.table.max),
        name:
          "string" in node.table.name
            ? this.visitString(node.table.name)
            : this.visitNameIdentifier(node.table.name),
        table_type: this.visitTypeExpression(node.table.table_type),
      },
    };
  }
  visitStarImport(node: StarImport): Import {
    return {
      star: this.visitTypeIdentifier(node.star),
    };
  }
  visitMemoryImport(node: MemoryImport): Import {
    return {
      memory: {
        alias: node.memory.alias && this.visitNameIdentifier(node.memory.alias),
        min: node.memory.min && this.visitInt(node.memory.min),
        max: node.memory.max && this.visitInt(node.memory.max),
        name:
          "string" in node.memory.name
            ? this.visitString(node.memory.name)
            : this.visitNameIdentifier(node.memory.name),
      },
    };
  }
  visitNameImport(node: NameImport): Import {
    return {
      name: {
        name: this.visitNameIdentifier(node.name.name),
        alias: node.name.alias && this.visitNameIdentifier(node.name.alias),
      },
    };
  }
  visitTraitImport(node: TraitImport): Import {
    return {
      trait: {
        name: this.visitTypeIdentifier(node.trait.name),
        alias: node.trait.alias && this.visitTypeIdentifier(node.trait.alias),
      },
    };
  }
  visitBuiltinImport(node: BuiltinImport): Import {
    return {
      builtin: {
        name: this.visitString(node.builtin.name),
        alias: this.visitNameIdentifier(node.builtin.alias),
      },
    };
  }

  visitFn(node: Fn): Fn {
    return {
      name: node.name,
      type_params: node.type_params,
      params: node.params.map((p) => this.visitFnParam(p)),
      return_type: node.return_type
        ? this.visitTypeExpression(node.return_type)
        : null,
      body: this.visitExpression(node.body),
    };
  }

  visitTraitFn(node: TraitFn): TraitFn {
    return {
      name: node.name,
      params: node.params.map((p) => this.visitNamedTypeExpression(p)),
      return_type: this.visitTypeExpression(node.return_type),
    };
  }

  visitFnParam(node: FnParam): FnParam {
    return {
      name: node.name,
      guard: node.guard ? this.visitTypeExpression(node.guard) : null,
    };
  }

  visitFnSignature(node: FnSignature): FnSignature {
    return {
      param_types: node.param_types.map((p) => this.visitFnParam(p)),
      return_type: this.visitTypeExpression(node.return_type),
    };
  }

  visitTypeIdentifier(node: TypeToken): TypeToken {
    return node;
  }

  visitNameIdentifier(node: NameToken): NameToken {
    return node;
  }

  visitString(node: StringToken): StringToken {
    return node;
  }
  visitInt(node: Int): Int {
    return node;
  }
}

import type {
  Kind,
  Type,
  Term,
  Pattern,
  Context,
  Binding,
  TypingError,
  Constraint,
} from "./types_system_f_omega.js";

// Visitor interface with transform capability
export interface TypeSystemVisitor {
  // Kinds
  visitStarKind?(node: { star: null }): Kind;
  visitArrowKind?(node: { arrow: { from: Kind; to: Kind } }): Kind;

  // Types
  visitVarType?(node: { var: string }): Type;
  visitArrowType?(node: { arrow: { from: Type; to: Type } }): Type;
  visitForallType?(node: {
    forall: { var: string; kind: Kind; body: Type };
  }): Type;
  visitAppType?(node: { app: { func: Type; arg: Type } }): Type;
  visitLamType?(node: { lam: { var: string; kind: Kind; body: Type } }): Type;
  visitConType?(node: { con: string }): Type;
  visitRecordType?(node: { record: [string, Type][] }): Type;
  visitVariantType?(node: { variant: [string, Type][] }): Type;
  visitMuType?(node: { mu: { var: string; body: Type } }): Type;
  visitTupleType?(node: { tuple: Type[] }): Type;

  // Terms
  visitVarTerm?(node: { var: string }): Term;
  visitLamTerm?(node: { lam: { arg: string; type: Type; body: Term } }): Term;
  visitAppTerm?(node: { app: { callee: Term; arg: Term } }): Term;
  visitTyLamTerm?(node: {
    tylam: { var: string; kind: Kind; body: Term };
  }): Term;
  visitTyAppTerm?(node: { tyapp: { term: Term; type: Type } }): Term;
  visitConTerm?(node: { con: { name: string; type: Type } }): Term;
  visitRecordTerm?(node: { record: [string, Term][] }): Term;
  visitProjectTerm?(node: { project: { record: Term; label: string } }): Term;
  visitInjectTerm?(node: {
    inject: { label: string; value: Term; variantType: Type };
  }): Term;
  visitMatchTerm?(node: {
    match: { scrutinee: Term; cases: [Pattern, Term][] };
  }): Term;
  visitFoldTerm?(node: { fold: { type: Type; term: Term } }): Term;
  visitUnfoldTerm?(node: { unfold: Term }): Term;
  visitTupleTerm?(node: { tuple: Term[] }): Term;
  visitTupleProjectTerm?(node: {
    tupleProject: { tuple: Term; index: number };
  }): Term;

  // Patterns
  visitVarPattern?(node: { var: string }): Pattern;
  visitWildcardPattern?(node: { wildcard: null }): Pattern;
  visitConPattern?(node: { con: { name: string; type: Type } }): Pattern;
  visitRecordPattern?(node: { record: [string, Pattern][] }): Pattern;
  visitVariantPattern?(node: {
    variant: { label: string; pattern: Pattern };
  }): Pattern;
  visitTuplePattern?(node: { tuple: Pattern[] }): Pattern;

  // Context and Bindings
  visitTermBinding?(node: { term: { name: string; type: Type } }): Binding;
  visitTypeBinding?(node: { type: { name: string; kind: Kind } }): Binding;
  visitContext?(node: Context): Context;

  // Errors (optional - for error transformations)
  visitTypeError?(node: TypingError): TypingError;

  // Constraints (optional - for constraint transformations)
  visitConstraint?(node: Constraint): Constraint;
}

// Default visitor that recursively visits and reconstructs the tree
export class BaseTypeSystemVisitor implements TypeSystemVisitor {
  // Kinds
  visitKind(node: Kind): Kind {
    if ("star" in node) return this.visitStarKind(node);
    if ("arrow" in node) return this.visitArrowKind(node);
    throw new Error("Unknown kind type");
  }

  visitStarKind(node: { star: null }): Kind {
    return node;
  }

  visitArrowKind(node: { arrow: { from: Kind; to: Kind } }): Kind {
    return {
      arrow: {
        from: this.visitKind(node.arrow.from),
        to: this.visitKind(node.arrow.to),
      },
    };
  }

  // Types
  visitType(node: Type): Type {
    if ("var" in node) return this.visitVarType(node);
    if ("arrow" in node) return this.visitArrowType(node);
    if ("forall" in node) return this.visitForallType(node);
    if ("app" in node) return this.visitAppType(node);
    if ("lam" in node) return this.visitLamType(node);
    if ("con" in node) return this.visitConType(node);
    if ("record" in node) return this.visitRecordType(node);
    if ("variant" in node) return this.visitVariantType(node);
    if ("mu" in node) return this.visitMuType(node);
    if ("tuple" in node) return this.visitTupleType(node);
    throw new Error("Unknown type");
  }

  visitVarType(node: { var: string }): Type {
    return node;
  }

  visitArrowType(node: { arrow: { from: Type; to: Type } }): Type {
    return {
      arrow: {
        from: this.visitType(node.arrow.from),
        to: this.visitType(node.arrow.to),
      },
    };
  }

  visitForallType(node: {
    forall: { var: string; kind: Kind; body: Type };
  }): Type {
    return {
      forall: {
        var: node.forall.var,
        kind: this.visitKind(node.forall.kind),
        body: this.visitType(node.forall.body),
      },
    };
  }

  visitAppType(node: { app: { func: Type; arg: Type } }): Type {
    return {
      app: {
        func: this.visitType(node.app.func),
        arg: this.visitType(node.app.arg),
      },
    };
  }

  visitLamType(node: { lam: { var: string; kind: Kind; body: Type } }): Type {
    return {
      lam: {
        var: node.lam.var,
        kind: this.visitKind(node.lam.kind),
        body: this.visitType(node.lam.body),
      },
    };
  }

  visitConType(node: { con: string }): Type {
    return node;
  }

  visitRecordType(node: { record: [string, Type][] }): Type {
    return {
      record: node.record.map(
        ([label, type]) => [label, this.visitType(type)] as [string, Type],
      ),
    };
  }

  visitVariantType(node: { variant: [string, Type][] }): Type {
    return {
      variant: node.variant.map(
        ([label, type]) => [label, this.visitType(type)] as [string, Type],
      ),
    };
  }

  visitMuType(node: { mu: { var: string; body: Type } }): Type {
    return {
      mu: {
        var: node.mu.var,
        body: this.visitType(node.mu.body),
      },
    };
  }

  visitTupleType(node: { tuple: Type[] }): Type {
    return {
      tuple: node.tuple.map((t) => this.visitType(t)),
    };
  }

  // Terms
  visitTerm(node: Term): Term {
    if ("var" in node) return this.visitVarTerm(node);
    if ("lam" in node) return this.visitLamTerm(node);
    if ("app" in node) return this.visitAppTerm(node);
    if ("tylam" in node) return this.visitTyLamTerm(node);
    if ("tyapp" in node) return this.visitTyAppTerm(node);
    if ("con" in node) return this.visitConTerm(node);
    if ("record" in node) return this.visitRecordTerm(node);
    if ("project" in node) return this.visitProjectTerm(node);
    if ("inject" in node) return this.visitInjectTerm(node);
    if ("match" in node) return this.visitMatchTerm(node);
    if ("fold" in node) return this.visitFoldTerm(node);
    if ("unfold" in node) return this.visitUnfoldTerm(node);
    if ("tuple" in node) return this.visitTupleTerm(node);
    if ("tupleProject" in node) return this.visitTupleProjectTerm(node);
    throw new Error("Unknown term");
  }

  visitVarTerm(node: { var: string }): Term {
    return node;
  }

  visitLamTerm(node: { lam: { arg: string; type: Type; body: Term } }): Term {
    return {
      lam: {
        arg: node.lam.arg,
        type: this.visitType(node.lam.type),
        body: this.visitTerm(node.lam.body),
      },
    };
  }

  visitAppTerm(node: { app: { callee: Term; arg: Term } }): Term {
    return {
      app: {
        callee: this.visitTerm(node.app.callee),
        arg: this.visitTerm(node.app.arg),
      },
    };
  }

  visitTyLamTerm(node: {
    tylam: { var: string; kind: Kind; body: Term };
  }): Term {
    return {
      tylam: {
        var: node.tylam.var,
        kind: this.visitKind(node.tylam.kind),
        body: this.visitTerm(node.tylam.body),
      },
    };
  }

  visitTyAppTerm(node: { tyapp: { term: Term; type: Type } }): Term {
    return {
      tyapp: {
        term: this.visitTerm(node.tyapp.term),
        type: this.visitType(node.tyapp.type),
      },
    };
  }

  visitConTerm(node: { con: { name: string; type: Type } }): Term {
    return {
      con: {
        name: node.con.name,
        type: this.visitType(node.con.type),
      },
    };
  }

  visitRecordTerm(node: { record: [string, Term][] }): Term {
    return {
      record: node.record.map(
        ([label, term]) => [label, this.visitTerm(term)] as [string, Term],
      ),
    };
  }

  visitProjectTerm(node: { project: { record: Term; label: string } }): Term {
    return {
      project: {
        record: this.visitTerm(node.project.record),
        label: node.project.label,
      },
    };
  }

  visitInjectTerm(node: {
    inject: { label: string; value: Term; variantType: Type };
  }): Term {
    return {
      inject: {
        label: node.inject.label,
        value: this.visitTerm(node.inject.value),
        variantType: this.visitType(node.inject.variantType),
      },
    };
  }

  visitMatchTerm(node: {
    match: { scrutinee: Term; cases: [Pattern, Term][] };
  }): Term {
    return {
      match: {
        scrutinee: this.visitTerm(node.match.scrutinee),
        cases: node.match.cases.map(
          ([pattern, term]) =>
            [this.visitPattern(pattern), this.visitTerm(term)] as [
              Pattern,
              Term,
            ],
        ),
      },
    };
  }

  visitFoldTerm(node: { fold: { type: Type; term: Term } }): Term {
    return {
      fold: {
        type: this.visitType(node.fold.type),
        term: this.visitTerm(node.fold.term),
      },
    };
  }

  visitUnfoldTerm(node: { unfold: Term }): Term {
    return {
      unfold: this.visitTerm(node.unfold),
    };
  }

  visitTupleTerm(node: { tuple: Term[] }): Term {
    return {
      tuple: node.tuple.map((t) => this.visitTerm(t)),
    };
  }

  visitTupleProjectTerm(node: {
    tupleProject: { tuple: Term; index: number };
  }): Term {
    return {
      tupleProject: {
        tuple: this.visitTerm(node.tupleProject.tuple),
        index: node.tupleProject.index,
      },
    };
  }

  // Patterns
  visitPattern(node: Pattern): Pattern {
    if ("var" in node) return this.visitVarPattern(node);
    if ("wildcard" in node) return this.visitWildcardPattern(node);
    if ("con" in node) return this.visitConPattern(node);
    if ("record" in node) return this.visitRecordPattern(node);
    if ("variant" in node) return this.visitVariantPattern(node);
    if ("tuple" in node) return this.visitTuplePattern(node);
    throw new Error("Unknown pattern");
  }

  visitVarPattern(node: { var: string }): Pattern {
    return node;
  }

  visitWildcardPattern(node: { wildcard: null }): Pattern {
    return node;
  }

  visitConPattern(node: { con: { name: string; type: Type } }): Pattern {
    return {
      con: {
        name: node.con.name,
        type: this.visitType(node.con.type),
      },
    };
  }

  visitRecordPattern(node: { record: [string, Pattern][] }): Pattern {
    return {
      record: node.record.map(
        ([label, pattern]) =>
          [label, this.visitPattern(pattern)] as [string, Pattern],
      ),
    };
  }

  visitVariantPattern(node: {
    variant: { label: string; pattern: Pattern };
  }): Pattern {
    return {
      variant: {
        label: node.variant.label,
        pattern: this.visitPattern(node.variant.pattern),
      },
    };
  }

  visitTuplePattern(node: { tuple: Pattern[] }): Pattern {
    return {
      tuple: node.tuple.map((p) => this.visitPattern(p)),
    };
  }

  // Context and Bindings
  visitBinding(node: Binding): Binding {
    if ("term" in node) return this.visitTermBinding(node);
    if ("type" in node) return this.visitTypeBinding(node);
    throw new Error("Unknown binding type");
  }

  visitTermBinding(node: { term: { name: string; type: Type } }): Binding {
    return {
      term: {
        name: node.term.name,
        type: this.visitType(node.term.type),
      },
    };
  }

  visitTypeBinding(node: { type: { name: string; kind: Kind } }): Binding {
    return {
      type: {
        name: node.type.name,
        kind: this.visitKind(node.type.kind),
      },
    };
  }

  visitContext(node: Context): Context {
    return node.map((binding) => this.visitBinding(binding));
  }

  // Optional: Error and Constraint visitors
  visitTypeError(node: TypingError): TypingError {
    return node; // Base implementation doesn't transform errors
  }

  visitConstraint(node: Constraint): Constraint {
    if ("type_eq" in node) {
      return {
        type_eq: {
          left: this.visitType(node.type_eq.left),
          right: this.visitType(node.type_eq.right),
        },
      };
    }
    if ("kind_eq" in node) {
      return {
        kind_eq: {
          left: this.visitKind(node.kind_eq.left),
          right: this.visitKind(node.kind_eq.right),
        },
      };
    }
    if ("has_kind" in node) {
      return {
        has_kind: {
          ty: this.visitType(node.has_kind.ty),
          kind: this.visitKind(node.has_kind.kind),
          context: this.visitContext(node.has_kind.context),
        },
      };
    }
    if ("has_type" in node) {
      return {
        has_type: {
          term: this.visitTerm(node.has_type.term),
          ty: this.visitType(node.has_type.ty),
          context: this.visitContext(node.has_type.context),
        },
      };
    }
    throw new Error("Unknown constraint type");
  }
}
