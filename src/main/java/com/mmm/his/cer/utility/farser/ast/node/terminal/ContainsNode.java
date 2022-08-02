package com.mmm.his.cer.utility.farser.ast.node.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.ExpressionIterator;
import java.util.Collection;

/**
 * A terminal node that represents an evaluation that centers around the list#contains. This node
 * will check the incoming values to see if the field value is contained with in it.
 *
 * @param <C> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public class ContainsNode<C extends Collection<A>, A> implements BooleanExpression<C> {

  private final A value;

  public ContainsNode(A value) {
    this.value = value;
  }

  @Override
  public boolean evaluate(C context) {
    return context.contains(this.value);
  }

  @Override
  public ExpressionIterator<C> iterator() {
    // Terminal node. Nothing to iterate over further.
    return new ExpressionIterator<>();
  }

  @Override
  public Object print() {
    return value;
  }

  @Override
  public String toString() {
    return "ContainsNode{" + "value='" + value + '\'' + '}';
  }
}
