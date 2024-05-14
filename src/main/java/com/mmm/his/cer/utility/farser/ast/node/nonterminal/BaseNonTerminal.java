package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import java.util.Objects;

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
public abstract class BaseNonTerminal<C, E> implements NonTerminal<C, E> {

  protected Expression<C, E> left;
  protected Expression<C, E> right;

  /**
   * Sets the left-side child node.
   *
   * @param left The node to set
   */
  @Override
  public void setLeft(Expression<C, E> left) {
    this.left = left;
  }

  /**
   * Sets the right-side child node.
   *
   * @param right The node to set
   */
  @Override
  public void setRight(Expression<C, E> right) {
    this.right = right;
  }

  @Override
  public String toString() {
    return "NonTerminal{" + "left=" + left + ", right=" + right + '}';
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (!(object instanceof BaseNonTerminal)) {
      return false;
    }
    BaseNonTerminal<?, ?> that = (BaseNonTerminal<?, ?>) object;
    return Objects.equals(left, that.left) && Objects.equals(right, that.right);
  }

  @Override
  public int hashCode() {
    return Objects.hash(left, right);
  }
}
