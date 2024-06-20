package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and a
 * right child. Both child nodes have the same evaluation result (data) type, producing a combined
 * boolean evaluation result type.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <L> The result type of the left expression.
 * @param <R> The result type of the right expression.
 *
 * @author Thomas Naeff
 */
public abstract class BaseNonTerminal<C, L, R> implements NonTerminal<C, L, R> {

  protected Expression<C, L> left;
  protected Expression<C, R> right;

  /**
   * Sets the left-side child node.
   *
   * @param left The node to set
   */
  @Override
  public void setLeft(Expression<C, L> left) {
    this.left = left;
  }

  /**
   * Sets the right-side child node.
   *
   * @param right The node to set
   */
  @Override
  public void setRight(Expression<C, R> right) {
    this.right = right;
  }

  @Override
  public String toString() {
    return "NonTerminal{" + "left=" + left + ", right=" + right + '}';
  }
}
