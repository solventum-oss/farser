package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;

/**
 * Wraps together the BaseNonTerminal and Expression classes for easier implementation.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <L> The result type of the left expression.
 * @param <R> The result type of the right expression.
 * @param <E> The return type of this expression. 
 *
 * @author Rowan Simmons
 */
public abstract class BaseNonTerminalExpression<C, L, R, E> extends BaseNonTerminal<C, L, R>
    implements Expression<C, E> {

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
    return "BaseNonTerminalExpression{" + "left=" + left + ", right=" + right + '}';
  }

}
