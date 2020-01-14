package com.mmm.his.cer.utility.farser.ast.nodes;

import java.util.List;
import java.util.Set;

/**
 * Implementation of a non-terminal node for use in the AST. This class represents a logical OR
 * operation.
 *
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public class Or<T> extends NonTerminal<T> {

  @Override
  public boolean evaluate(List<T> operands, Set<T> accumulator) {
    boolean leftTrue = left.evaluate(operands, accumulator);
    boolean rightTrue = right.evaluate(operands, accumulator);
    if ((leftTrue) && left instanceof Terminal) {
      accumulator.add(((Terminal<T>) left).getOperandValue());
    }
    if ((rightTrue) && right instanceof Terminal) {
      accumulator.add(((Terminal<T>) right).getOperandValue());
    }
    return leftTrue || rightTrue;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Or{");
    sb.append("left=").append(left);
    sb.append(", right=").append(right);
    sb.append('}');
    return sb.toString();
  }
}
