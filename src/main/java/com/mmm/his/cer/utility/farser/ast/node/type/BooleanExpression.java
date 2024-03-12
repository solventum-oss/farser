package com.mmm.his.cer.utility.farser.ast.node.type;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;

/**
 * Interface for each node of the AST to implement. This will allow the evaluation of the entire
 * boolean expression through recursion.
 *
 * @param <C> The type of context to be used for the terminal node execution.
 *
 * @author Mike Funaro
 */
public interface BooleanExpression<C> extends Iterable<BooleanExpression<C>> {

  /**
   * Evaluate an expression returning true or false based on tests against the operands sent in.
   *
   * @param context The context that will be used in the evaluation of the node.
   * @return <code>true</code> or <code>false</code>.
   */
  boolean evaluate(C context);

  /**
   * Returns an iterator over the expression elements.<br>
   * <br>
   * For terminal nodes, <code>null</code> should not be returned but an empty iterator can be
   * returned (<code>return new ExpressionIterator<>()</code> - this default implementation).
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
