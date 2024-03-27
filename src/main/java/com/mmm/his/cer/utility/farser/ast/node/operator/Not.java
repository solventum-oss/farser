package com.mmm.his.cer.utility.farser.ast.node.operator;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;

/**
 * Implementation of a non-terminal node for use in the AST. This type of node will only have a left
 * child and no right child. It's evaluation should negate the result of the left child.
 *
 * @param <C> The context type used in the terminal nodes.
 * @author Mike Funaro
 */
public class Not<C> extends NonTerminal<C> {

  /**
   * For a not node, we should only set one child. This implementation allows to set the left child
   * only. And should be the only public API for child setting of a Not node.
   *
   * @param child the child for this node.
   */
  @Override
  public void setLeft(BooleanExpression<C> left) {
    // Overridden for updated javadoc.
    super.setLeft(left);
  }

  @Override
  public void setRight(BooleanExpression<C> right) {
    // Not nodes will only have one child and that is the left child. Throw error.
    throw new UnsupportedOperationException("Can only set the left-side child for NOT nodes");
  }

  @Override
  public boolean evaluate(C context) {
    boolean evaluation = left.evaluate(context);
    return !evaluation;
  }

  @Override
  public String toString() {
    return "Not{" + "left=" + left + ", right=" + right + '}';
  }
}
