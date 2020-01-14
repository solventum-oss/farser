package com.mmm.his.cer.utility.farser.ast.nodes;

import java.util.List;
import java.util.Set;

/**
 * Interface for each node of the AST to implement. This will allow the evaluation of the entire
 * boolean expression through recursion.
 *
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public interface BooleanExpression<T> {

  /**
   * Evaluate an expression returning true or false based on tests against the operands sent in.
   *
   * @param operands the list of operands to buildExpressionTree the boolean test around
   * @return true or false.
   */
  boolean evaluate(List<T> operands, Set<T> accumulator);
}
