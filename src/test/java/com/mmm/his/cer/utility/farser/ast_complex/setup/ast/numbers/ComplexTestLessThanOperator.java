package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.numbers;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BooleanNonTerminal;

/**
 *
 *
 * @author Thomas Naeff
 *
 * @param <C>
 */
public class ComplexTestLessThanOperator<C> extends BooleanNonTerminal<C, Integer> {

  @Override
  public Boolean evaluate(C context) {
    return left.evaluate(context) < right.evaluate(context);
  }

  @Override
  public String print() {
    return "LESS-THAN";
  }

  @Override
  public String toString() {
    return "LessThan{" + "left=" + left + ", right=" + right + '}';
  }
}
