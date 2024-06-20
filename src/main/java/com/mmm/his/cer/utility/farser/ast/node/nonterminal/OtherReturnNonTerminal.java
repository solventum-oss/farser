package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

/**
 * Non-terminal expression class for classes that have the same right and left return types.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <L> The result type of the left and right expression.
 * @param <E> The return type of this expression. 
 * 
 * @author Rowan Simmons
 */
public abstract class OtherReturnNonTerminal<C, L, E> 
    extends BaseNonTerminalExpression<C, L, L, E> {

  @Override
  public String toString() {
    return "OtherReturnNonTerminal{" + "left=" + left + ", right=" + right + '}';
  }

}
