package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;

/**
 * Wraps together the BaseNonTerminal and Expression classes for easier implementation.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <E> The result type of each left/right expression.
 * @param <R> The return type of this expression. 
 * 
 * @author Rowan Simmons
 */
public abstract class NonTerminalExpression<C, E, R> extends BaseNonTerminal<C, E> 
    implements Expression<C, R> {

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
    return "NonTerminalExpression{" + "left=" + left + ", right=" + right + '}';
  }

}
