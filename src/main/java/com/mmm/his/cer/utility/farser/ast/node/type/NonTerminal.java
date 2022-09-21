package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and a
 * right child.
 *
 * @param <R> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public abstract class NonTerminal<R> implements BooleanExpression<R> {

  protected BooleanExpression<R> left;
  protected BooleanExpression<R> right;

  public void setLeft(BooleanExpression<R> left) {
    this.left = left;
  }

  public void setRight(BooleanExpression<R> right) {
    this.right = right;
  }

  @Override
  public LtrExpressionIterator<R> iterator() {
    return new LtrExpressionIterator<>(left, right);
  }

  @Override
  public Object print() {
    // A default printing behavior. Can be overridden by implementations if needed.
    return getClass().getSimpleName().toUpperCase();
  }

  @Override
  public String toString() {
    return "NonTerminal{" + "left=" + left + ", right=" + right + '}';
  }
}
