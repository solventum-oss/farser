package com.mmm.his.cer.utility.farser.ast.nodes;

import java.util.List;
import java.util.Set;

/**
 * A terminal node that represents an operand from a grouper formula. This will be compared against
 * a list of operands to see if it's value is present.
 *
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public class Operand<T> extends Terminal<T> {

  public Operand(T value) {
    super(value);
  }

  @Override
  public boolean evaluate(List<T> operands, Set<T> accumulator) {
    return operands.contains(this.operandValue);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Operand{");
    sb.append("operandValue='").append(operandValue).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
