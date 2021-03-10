package com.mmm.his.cer.utility.farser.ast.node.operator;

import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;
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

    if (leftTrue) {
      return true;
    }

    return right.evaluate(operands, accumulator);
  }

  @Override
  public String toString() {
    return "Or{" + "left=" + left + ", right=" + right + '}';
  }
}
