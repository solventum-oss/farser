package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;

/**
 *
 *
 * @author Thomas Naeff
 *
 * @param <C>
 */
public class ComplexTestInOperator<C> extends NonTerminal<C, String> {

  @Override
  public Boolean evaluate(C context) {
    // Just some dummy "in" evaluation
    return right.evaluate(context).contains(left.evaluate(context));
  }

  @Override
  public String print() {
    return "IN";
  }

  @Override
  public String toString() {
    return "In{" + "left=" + left + ", right=" + right + '}';
  }
}
