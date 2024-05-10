package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;

/**
 *
 *
 * @author Thomas Naeff
 *
 * @param <C>
 */
public class ComplexTestInTableOperator<C> extends NonTerminal<C, String> {

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
