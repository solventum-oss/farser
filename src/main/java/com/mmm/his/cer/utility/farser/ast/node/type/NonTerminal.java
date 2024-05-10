package com.mmm.his.cer.utility.farser.ast.node.type;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and a
 * right child. Both child nodes have the same evaluation result (data) type, producing a combined
 * boolean evaluation result type.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <E> The result type of each left/right expression
 *
 * @author Thomas Naeff
 */
public abstract class NonTerminal<C, E> implements BooleanExpression<C> {

  protected Expression<C, E> left;
  protected Expression<C, E> right;

  /**
   * Sets the left-side child node.
   *
   * @param left The node to set
   */
  public void setLeft(Expression<C, E> left) {
    this.left = left;
  }

  /**
   * Sets the right-side child node.
   *
   * @param right The node to set
   */
  public void setRight(Expression<C, E> right) {
    this.right = right;
  }

  @Override
  public LtrExpressionIterator<C> iterator() {
    return new LtrExpressionIterator<>(left, right);
  }

  @Override
  public String print() {
    // A default printing behavior. Can be overridden by implementations if needed.
    return getClass().getSimpleName().toUpperCase();
  }

  @Override
  public String toString() {
    return "NonTerminal{" + "left=" + left + ", right=" + right + '}';
  }
}
