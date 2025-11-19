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
  name_expr,
  type InfixExpression,
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
    const block = node.block;

    for (let i = block.length - 1; i >= 0; i--) {
      const item = block[i]!;
      if ("arrow_bind" in item) {
        const nextIndex = i + 1;
        const count = block.length - nextIndex;
        const rest = block.splice(nextIndex, count);
        const position = block[0]?.position ?? node.position;
        const fnParamName = item.arrow_bind.name.name;
        const fnPosition = item.arrow_bind.name.position;
        const fnParam = fn_param(fnParamName, null, fnPosition);
        const fn = {
          body: block_expr(rest, position),
          name: null,
          params: [fnParam],
          position,
          return_type: null,
          type_params: [],
        } satisfies Fn;
        const fnExpr = fn_expr(fn, position);

        if ("call" in item.arrow_bind.expression) {
          // should be first parameter of the call expression
          item.arrow_bind.expression.call[1].unshift(fnExpr);
          block[i] = expression_body_expr(
            item.arrow_bind.expression,
            item.arrow_bind.expression.position,
          );
        } else {
          block[i] = expression_body_expr(
            call_expr(item.arrow_bind.expression, [fnExpr], position),
            position,
          );
        }
      }
    }
    return super.visitBlockExpression(block_expr(block, node.position));
  }

  override visitInfixExpression(node: InfixExpression): Expression {
    switch (node.infix.op) {
      case "!=":
        return call_expr(
          name_expr("not", node.position),
          [
            call_expr(
              name_expr("eq", node.position),
              [node.infix.left, node.infix.right],
              node.position,
            ),
          ],
          node.position,
        );
      case "==":
        return call_expr(
          name_expr("eq", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "<>":
        return call_expr(
          name_expr("concat", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "<":
        return call_expr(
          name_expr("lt", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "<=":
        return call_expr(
          name_expr("lte", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case ">":
        return call_expr(
          name_expr("gt", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case ">=":
        return call_expr(
          name_expr("gte", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "%":
        return call_expr(
          name_expr("mod", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "&":
        return call_expr(
          name_expr("and", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "|":
        return call_expr(
          name_expr("or", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "&&":
        return call_expr(
          name_expr("logical_and", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "||":
        return call_expr(
          name_expr("logical_or", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "+":
        return call_expr(
          name_expr("add", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "-":
        return call_expr(
          name_expr("sub", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "*":
        return call_expr(
          name_expr("mul", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "/":
        return call_expr(
          name_expr("mul", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "**":
        return call_expr(
          name_expr("exp", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "<<":
        return call_expr(
          name_expr("shl", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case ">>":
        return call_expr(
          name_expr("shr", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "^":
        return call_expr(
          name_expr("xor", node.position),
          [node.infix.left, node.infix.right],
          node.position,
        );
      case "|>":
        if ("call" in node.infix.right) {
          node.infix.right.call[1].unshift(node.infix.left);
          return node.infix.right;
        }
        return call_expr(node.infix.right, [node.infix.left], node.position);
    }
    throw new Error(`Binary operator not supported: ${node.infix.op}`);
  }
}
