package com.mmm.his.cer.utility.farser.ast.node.operator;

import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;

import java.util.List;
import java.util.Set;

/**
 * Implementation of a non-terminal node for use in the AST. This class represents a logical AND
 * operation.
 *
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public class And<T> extends NonTerminal<T> {

  @Override
  public boolean evaluate(List<T> operands, Set<T> accumulator) {
    return left.evaluate(operands, accumulator) && right.evaluate(operands, accumulator);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("And{");
    sb.append("left=").append(left);
    sb.append(", right=").append(right);
    sb.append('}');
    return sb.toString();
  }
}
