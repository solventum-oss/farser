package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminalExpression;

/**
 * Operator that has a left and right with different return types.
 */
public class ComplexTestHasOperator<C> extends BaseNonTerminalExpression<C, String, Integer, Boolean> {

  @Override
  public Boolean evaluate(C context) {
    String leftResult = left.evaluate(context);
    
    Integer rightResult = right.evaluate(context);
    String rightStr = String.valueOf(rightResult);
    
    return leftResult.contains(rightStr);
  }

  @Override
  public String print() {
    return "HAS";
  }

  @Override
  public String toString() {
    return "Has{" + "left=" + left + ", right=" + right + '}';
  }
}
