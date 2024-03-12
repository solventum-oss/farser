package com.mmm.his.cer.utility.farser.ast.node.type;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and a
 * right child.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @author Mike Funaro
 */
public abstract class NonTerminal<C> implements BooleanExpression<C> {

  protected BooleanExpression<C> left;
  protected BooleanExpression<C> right;

  public void setLeft(BooleanExpression<C> left) {
    this.left = left;
  }

  public void setRight(BooleanExpression<C> right) {
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
