package com.mmm.his.cer.utility.farser.ast.node.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import java.util.List;
import java.util.Set;

/**
 * A terminal node that represents an an evaluation that centers around the list#contains. This node
 * will check the incoming values to see if the field value is contained with in it.
 *
 * @param <T> The type used in the terminal nodes.
 * @author Mike Funaro
 */
public class ContainsNode<T> implements BooleanExpression<T> {

  private final T value;

  public ContainsNode(T value) {
    this.value = value;
  }

  @Override
  public boolean evaluate(List<T> values, Set<T> accumulator) {
    boolean contains = values.contains(this.value);
    if (contains) {
      accumulator.add(value);
    }
    return contains;
  }

  @Override
  public String toString() {
    return "ContainsNode{" + "value='" + value + '\'' + '}';
  }
}
