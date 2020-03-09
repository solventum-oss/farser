package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and
 * a right child.
 *<br/>
 * Uses a bus to communicate information outwards during evaluation.
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public abstract class NonTerminal<T> implements BooleanExpression<T> {

  protected BooleanExpression<T> left;
  protected BooleanExpression<T> right;

  public void setLeft(BooleanExpression<T> left) {
    this.left = left;
  }

  public void setRight(BooleanExpression<T> right) {
    this.right = right;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("NonTerminal{");
    sb.append("left=").append(left);
    sb.append(", right=").append(right);
    sb.append('}');
    return sb.toString();
  }
}
