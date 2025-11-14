import type { NameToken, TypeToken } from "../lexer.js";
import {
  type BlockExpression,
  block_expr,
  call_expr,
  type Expression,
  expression_body_expr,
  type Fn,
  fn_expr,
  fn_param,
  let_bind_body_expr,
  name_expr,
} from "../parser.js";
import { BaseVisitor } from "../visitor.js";

export class DesugarPass extends BaseVisitor {
  // every name and type should be "Fresh" before compilation
  override visitTypeIdentifier(node: TypeToken): TypeToken {
    return {
      type: node.type,
      position: node.position.slice() as [number, number],
    };
  }

  override visitNameIdentifier(node: NameToken): NameToken {
    return {
      name: node.name,
      position: node.position.slice() as [number, number],
    };
  }

  override visitBlockExpression(node: BlockExpression): Expression {
    for (let i = 0; i < node.block.length; i++) {
      const body_expr = node.block[i]!;

      if ("arrow_bind" in body_expr) {
        // The rest of the function becomes the body of the inner callback
        const { name, expression } = body_expr.arrow_bind;
        const rest = node.block.splice(i + 1, node.block.length - i);

        const fn = fn_expr(
          {
            body: block_expr(rest, expression.position),
            name: null,
            return_type: null,
            params: [fn_param(name.name, null, expression.position)],
            type_params: [],
            position: body_expr.position,
          } satisfies Fn,
          expression.position,
        );

        let next: Expression;

        if ("call" in expression) {
          expression.call[1].push(fn);
          next = expression;
        } else {
          next = call_expr(expression, [fn], expression.position);
        }

        node.block[i] = expression_body_expr(next, expression.position);
        return super.visitBlockExpression(node);
      }

      if ("expression" in body_expr && i !== node.block.length - 1) {
        node.block[i] = let_bind_body_expr(
          false,
          name_expr("_", body_expr.position),
          body_expr.expression,
          body_expr.position,
        );
      }
    }

    return super.visitBlockExpression(node);
  }
}
