import {
  type BlockExpression,
  block_expr,
  call_expr,
  type Expression,
  fn_expr,
  fn_param,
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
        const fn = fn_expr([fn_param(name.name, null)], null, block_expr(rest));
        const next = call_expr(name_expr("map"), [fn, expression]);
        this.visitCallExpression(next);
        return node;
      }
    }

    return node;
  }
}
