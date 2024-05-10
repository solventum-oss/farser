package com.mmm.his.cer.utility.farser.ast.node.operator.bool;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BooleanNonTerminal;

/**
 * Implementation of a non-terminal node for use in the AST. This class represents a logical AND
 * operation.
 *
 * @param <C> The context type used in the terminal nodes.
 * @author Mike Funaro
 */
public class And<C> extends BooleanNonTerminal<C, Boolean> {

  @Override
  public Boolean evaluate(C context) {
    // Evaluate left-side first, then right-side
    return left.evaluate(context) && right.evaluate(context);
  }

  @Override
  public String toString() {
    return "And{" + "left=" + left + ", right=" + right + '}';
  }
}
