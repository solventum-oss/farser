package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;

/**
 *
 *
 * @author Thomas Naeff
 *
 * @param <C>
 */
public class ComplexTestEqualOperator<C> extends NonTerminal<C, Integer> {

  @Override
  public Boolean evaluate(C context) {
    Integer leftResult = left.evaluate(context);
    Integer rightResult = left.evaluate(context);
    // NPE save 'equals'
    return leftResult == null ? leftResult == rightResult : leftResult.equals(rightResult);
  }

  @Override
  public String print() {
    return "EQUAL";
  }

  @Override
  public String toString() {
    return "Equal{" + "left=" + left + ", right=" + right + '}';
  }
}
