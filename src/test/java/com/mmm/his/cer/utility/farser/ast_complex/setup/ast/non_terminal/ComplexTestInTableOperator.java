package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.OtherReturnNonTerminal;

/**
 *
 *
 * @author Thomas Naeff
 *
 * @param <C>
 */
public class ComplexTestInTableOperator<C> extends OtherReturnNonTerminal<C, String, Boolean> {

  @Override
  public Boolean evaluate(C context) {
    // Just some dummy "in table" evaluation
    return right.evaluate(context).contains(left.evaluate(context));
  }

  @Override
  public String print() {
    return "IN-TABLE";
  }

  @Override
  public String toString() {
    return "InTable{" + "left=" + left + ", right=" + right + '}';
  }
}
