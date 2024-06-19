package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and a
 * right child, each of child individually with a boolean return type and with a combined evaluation
 * result of a boolean type.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <E> The return type of the left and right child nodes of this non-terminal node. 
 * @author Mike Funaro
 * @implNote This non-terminal node has a return type of Boolean, which is different from the
 *     return types of the child nodes.
 */
public abstract class BooleanNonTerminal<C, E> extends NonTerminalExpression<C, E, Boolean> {

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
    return "NonTerminal{" + "left=" + left + ", right=" + right + '}';
  }
}
