package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * Interface for each node of the AST to implement. This will allow the evaluation of the entire
 * boolean expression through recursion.
 *
 * @param <T> The type of context to be used for the terminal node execution.
 *
 * @author Mike Funaro
 */
public interface BooleanExpression<T> extends Iterable<BooleanExpression<T>> {

  /**
   * Evaluate an expression returning true or false based on tests against the operands sent in.
   *
   * @param context The context that will be used in the evaluation of the node.
   * @return <code>true</code> or <code>false</code>.
   */
  boolean evaluate(T context);

  /**
   * Returns an iterator over the expression elements.<br>
   * <br>
   * For terminal nodes, <code>null</code> should not be returned but an empty iterator can be
   * returned (<code>return new ExpressionIterator<>(this)</code> - this default implementation).
   */
  @Override
  default LtrExpressionIterator<C> iterator() {
    return new LtrExpressionIterator<>();
  }

  /**
   * Returns a printable representation of the node.<br>
   * The returned data can be any object, its string form ({@link String#valueOf(Object)}) will be
   * used as printable form.
   *
   * @return The printable form of this node
   */
  default Object print() {
    return toString();
  }

}
