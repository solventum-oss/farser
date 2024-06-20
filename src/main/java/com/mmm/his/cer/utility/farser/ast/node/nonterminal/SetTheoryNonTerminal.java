package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;

/**
 * A non-terminal base class for Set theory.
 *
 * @author Mike Funaro
 */
public abstract class SetTheoryNonTerminal<C, E> extends SameTypeNonTerminal<C, E> {

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
