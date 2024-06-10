package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BooleanNonTerminal;

/**
 *
 *
 * @author Thomas Naeff
 *
 * @param <C>
 */
public class ComplexTestEqualOperator<C> extends BooleanNonTerminal<C, Integer> {

  @Override
  public Boolean evaluate(C context) {
    Integer leftResult = left.evaluate(context);
    Integer rightResult = right.evaluate(context);
    // NPE safe 'equals'
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
