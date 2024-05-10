package com.mmm.his.cer.utility.farser.ast.node.type;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;

/**
 * Interface for each node of the AST to implement. This will allow the evaluation of the entire
 * boolean expression through recursion.
 *
 * @param <C> The type of context to be used for the terminal node execution.
 * @param <R> The data type of the evaluation result
 *
 * @author Mike Funaro
 * @author Thomas Naeff
 */
public interface Expression<C, R> extends Iterable<Expression<C, ?>> {

  /**
   * Evaluate an expression returning its value.
   *
   * @param context The context that will be used in the evaluation of the node.
   * @return The expression result.
   */
  R evaluate(C context);

  /**
   * Returns an iterator over the expression elements.<br>
   * <br>
   * For terminal nodes, <code>null</code> should not be returned but an empty iterator can be
   * returned (<code>return new ExpressionIterator<>()</code>, this default implementation).
   */
  @Override
  default LtrExpressionIterator<C> iterator() {
    return new LtrExpressionIterator<>();
  }

  /**
   * Returns a printable representation of the node.
   *
   * @return The printable form of this node
   */
  default String print() {
    return toString();
  }

}
