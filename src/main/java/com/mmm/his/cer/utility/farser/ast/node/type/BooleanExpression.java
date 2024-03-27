package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * Interface for each node of the AST to implement. This will allow the evaluation of the entire
 * boolean expression through recursion.
 *
 * @param <C> The type of context to be used for the terminal node execution.
 *
 * @author Mike Funaro
 */
public interface BooleanExpression<C> extends Expression<C, Boolean> {

  /**
   * Evaluate an expression returning true or false based on tests against the operands sent in. The
   * implementation should ensure that it never returns <code>null</code>.
   *
   * @param context The context that will be used in the evaluation of the node.
   * @return <code>true</code> or <code>false</code>. Never <code>null</code>.
   */
  @Override
  Boolean evaluate(C context);

}
