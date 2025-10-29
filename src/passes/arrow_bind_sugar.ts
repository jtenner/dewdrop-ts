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
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export class ArrowBindSugarPass extends BaseVisitor {
  override visitBlockExpression(node: BlockExpression): Expression {
    for (let i = 0; i < node.block.length; i++) {
      const body_expr = node.block[i]!;
      if ("arrow_bind" in body_expr) {
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
        const result = this.visitCallExpression(next);
        return result;
      }

      if ("expression" in body_expr) {
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
