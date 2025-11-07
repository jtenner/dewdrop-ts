import type { NameToken, TypeToken } from "../lexer.js";
import {
  type BlockExpression,
  block_expr,
  call_expr,
  type Expression,
  type Fn,
  fn_expr,
  fn_param,
  let_bind_body_expr,
  name_expr,
  expression_body_expr,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export class DesugarPass extends BaseVisitor {
  // every name and type should be "Fresh" before compilation
  override visitTypeIdentifier(node: TypeToken): TypeToken {
    return { type: node.type };
  }

  override visitNameIdentifier(node: NameToken): NameToken {
    return { name: node.name };
  }
  override visitBlockExpression(node: BlockExpression): Expression {
    for (let i = 0; i < node.block.length; i++) {
      const body_expr = node.block[i]!;
      if ("arrow_bind" in body_expr) {
        // The rest of the function becomes the body of the inner callback
        const { name, expression } = body_expr.arrow_bind;
        const rest = node.block.splice(i + 1, node.block.length - i);

        const fn = fn_expr({
          body: block_expr(rest),
          name: null,
          return_type: null,
          params: [fn_param(name.name, null)],
          type_params: [],
        } satisfies Fn);
        const next = call_expr(name_expr("map"), [fn, expression]);
        node.block[i] = expression_body_expr(next);
        return super.visitBlockExpression(node);
      }

      if ("expression" in body_expr && i !== node.block.length - 1) {
        node.block[i] = let_bind_body_expr(
          false,
          name_expr("_"),
          body_expr.expression,
        );
      }
    }

    return node;
  }
}
