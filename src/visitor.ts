import type { NameToken, TypeToken } from "./lexer.js";
import type { Declaration, EnumDeclaration, EnumVariant, Fn, FnDeclaration, FnParam, ImplDeclaration, Import, ImportDeclaration, LetDeclaration, Module, TraitDeclaration, TypeDeclaration } from "./parser.js";

function $visit_event<TNode, TScope>(
  node: TNode,
  scope: TScope,
  event?: (node: TNode, scope: TScope) => [TNode | null, TScope | null],

) {
  if (event) {
    const [replace_node, replace_scope] = event(node, scope);
    // @ts-ignore
    if (replace_node && replace_node !== node) continue;
    node = replace_node ?? node;
    scope = replace_scope ?? scope;
  }
}

function $visit_event_defer<TNode, TSpecific, TScope>(
  node: TNode,
  spec: TSpecific,
  scope: TScope,
  event?: (node: TSpecific, scope: TScope) => [TNode | null, TScope | null],
) {
  if (event) {
    const [replace_node, replace_scope] = event(spec, scope);
    // @ts-ignore
    if (replace_node === null || replace_node && replace_node !== node) return [replace_node, replace_scope ?? scope];
    node = replace_node ?? node;
    scope = replace_scope ?? scope;
  }
}

function $visit_node<TNode, TScope>(
  node: TNode,
  scope: TScope,
  body: (node: TNode, scope: TScope) => [TNode, TScope],
) {
  const [replace_node, replace_scope] = body(node, scope);
  // @ts-ignore
  if (replace_node && replace_node !== node) continue;
  node = replace_node ?? node;
  scope = replace_scope ?? scope;
}

function $visit_node_defer<TNode, TScope>(
  node: TNode,
  scope: TScope,
  body: (node: TNode, scope: TScope) => [TNode, TScope],
) {
  const [replace_node, replace_scope] = body(node, scope);
  if (replace_node && replace_node !== node) return [replace_node, replace_scope ?? scope];
  node = replace_node ?? node;
  scope = replace_scope ?? scope;
}

function $visit_child<TNode, TScope>(
  node: TNode,
  scope: TScope,
  body: (node: TNode, scope: TScope) => [TNode, TScope],
) {
  const [replace_node, replace_scope] = body(node, scope);
  node = replace_node ?? node;
  scope = replace_scope ?? scope;
}


function $visit_all<TNode, TScope>(
  nodes: TNode[],
  scope: TScope,
  body: (node: TNode, scope: TScope) => [TNode, TScope],
) {
  const result = [] as TNode[];
  let i = 0;
  while (true) {
    if (i >= nodes.length) break;

    const node = nodes[i]!;
    const [replace_node, replace_scope] = body(node, scope);
    scope = replace_scope ?? scope;
    if (replace_node !== null) result.push(replace_node);
    i++;
  }

  nodes = result;
}

export abstract class Visitor<TScope> {
  visit<TNode>(
    node: TNode,
    scope: TScope,
    on_visit: (node: TNode, scope: TScope) => [TNode, TScope],
    on_enter?: (node: TNode, scope: TScope) => [TNode | null, TScope | null],
    on_exit?: (node: TNode, scope: TScope) => [TNode | null, TScope | null],
  ): [TNode, TScope] {
    while (true) {
      $visit_event!(node, scope, on_enter);
      $visit_node!(node, scope, on_visit);
      $visit_event!(node, scope, on_exit);
      
      return [node, scope];
    }
  }

  visit_module(node: Module, scope: TScope) {
    return this.visit(
      node,
      scope,
      (node, scope) => {
        $visit_all!(node.module, scope, this.visit_declaration);
        return [node, scope];
      },
      this.on_enter_module,
      this.on_exit_module,
    )
  }

  visit_declaration(node: Declaration, scope: TScope) {
    return this.visit(
      node,
      scope,
      (node, scope) => {
        if ("fn" in node) {
          $visit_event_defer!(node, node, scope, this.on_enter_fn_declaration);
          let fn = node.fn.fn;
          $visit_child!(fn, scope, this.visit_fn);
          node.fn.fn = fn;
          $visit_event_defer!(node, node, scope, this.on_exit_fn_declaration);
        } else if ("enum" in node) {
          $visit_event_defer!(node, node, scope, this.on_enter_enum_declaration);
          
          let id = node.enum.id;
          $visit_child!(id, scope, this.visit_type_id);
          node.enum.id = id;

          let type_params = node.enum.type_params;
          $visit_all!(type_params, scope, this.visit_name_id);
          node.enum.type_params = type_params;

          let variants = node.enum.variants;
          $visit_all!(variants, scope, this.visit_enum_variant);
          node.enum.variants = variants;

          $visit_event_defer!(node, node, scope, this.on_exit_enum_declaration);
        } else if ("import_dec" in node) {
          
        } else if ("type_dec" in node) {
          
        } else if ("let_dec" in node) {
          
        } else if ("trait" in node) {
          
        } else if ("impl" in node) {
          
        }
        return [node, scope];
      },
      this.on_enter_declaration,
      this.on_exit_declaration,
    )
  }

  visit_fn(node: Fn, scope: TScope): [Fn, TScope] {
    return [node, scope];
  }

  visit_type_id(node: TypeToken, scope: TScope): [TypeToken, TScope] {
    return this.visit(node, scope, (node, scope) => [node, scope], this.on_enter_type_id, this.on_exit_type_id);
  }

  visit_name_id(node: NameToken, scope: TScope): [NameToken, TScope] {
    return this.visit(node, scope, (node, scope) => [node, scope], this.on_enter_name_id, this.on_exit_name_id);
  }

  visit_enum_variant(node: EnumVariant, scope: TScope): [EnumVariant, TScope] {
    
    return [node, scope];
  }

  protected abstract on_enter_module?(node: Module, scope: TScope): [Module | null, TScope | null];
  protected abstract on_exit_module?(node: Module, scope: TScope): [Module | null, TScope | null];

  protected abstract on_enter_declaration?(node: Declaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_declaration?(node: Declaration, scope: TScope): [Declaration | null, TScope | null];

  protected abstract on_enter_fn_declaration?(node: FnDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_fn_declaration?(node: FnDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_enum_declaration?(node: EnumDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_enum_declaration?(node: EnumDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_import_declaration?(node: ImportDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_import_declaration?(node: ImportDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_type_declaration?(node: TypeDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_type_declaration?(node: TypeDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_let_declaration?(node: LetDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_let_declaration?(node: LetDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_trait_declaration?(node: TraitDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_trait_declaration?(node: TraitDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_impl_declaration?(node: ImplDeclaration, scope: TScope): [Declaration | null, TScope | null];
  protected abstract on_exit_impl_declaration?(node: ImplDeclaration, scope: TScope): [Declaration | null, TScope | null];
  
  protected abstract on_enter_type_id(node: TypeToken, scope: TScope): [TypeToken | null, TScope | null];
  protected abstract on_exit_type_id(node: TypeToken, scope: TScope): [TypeToken | null, TScope | null];

  protected abstract on_enter_name_id(node: NameToken, scope: TScope): [NameToken | null, TScope | null];
  protected abstract on_exit_name_id(node: NameToken, scope: TScope): [NameToken | null, TScope | null];
}
