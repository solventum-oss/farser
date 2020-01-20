package com.mmm.his.cer.utility.farser.ast.nodes;

import java.util.List;
import java.util.Set;

/**
 * Implementation of a non-terminal node for use in the AST. This type of node will only have a left
 * child and no right child. It's evaluation should negate the result of the left child.
 * <br/>
 * Nothing is reported back to the event bus about a negation, since in actuality, negation in our
 * formulas is a existence check.
 *
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public class Not<T> extends NonTerminal<T> {


  /**
   * For a not node, we should only set one child. This method sets the left child only. And should
   * be the only public API for child setting of a Not node.
   *
   * @param child the child for this node.
   */
  public void setChild(BooleanExpression<T> child) {
    setLeft(child);
  }

  @Override
  public void setRight(BooleanExpression right) {
    // Not nodes will only have one child and that is the left child. Throw error.
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean evaluate(List<T> operands, Set<T> accumulator) {
    boolean evaluation = left.evaluate(operands, accumulator);
    return !evaluation;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Not{");
    sb.append("left=").append(left);
    sb.append(", right=").append(right);
    sb.append('}');
    return sb.toString();
  }
}