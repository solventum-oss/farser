package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;

/**
 * A non-terminal base class for set logic.
 *
 * @author Mike Funaro
 */
public abstract class SetNonTerminal<C, E> extends BaseNonTerminal<C, E> implements
    Expression<C, E> {

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
