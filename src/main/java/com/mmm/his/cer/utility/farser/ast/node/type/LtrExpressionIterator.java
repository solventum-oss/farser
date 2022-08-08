package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.Arrays;
import java.util.Iterator;

/**
 * An iterator which iterates over the complete expression, top down (starting at the root node) and
 * from "left to right" (LTR - in the order of the formula evaluation). The order of the elements it
 * iterates over produces a "Polish/prefix notation" of the formula when printed out in this
 * iteration order.
 *
 * @author Thomas Naeff
 *
 * @param <C> The expression context data type
 */
public class LtrExpressionIterator<C> implements Iterator<BooleanExpression<C>> {

  private final Iterator<BooleanExpression<C>> nodes;
  private LtrExpressionIterator<C> currentIterator;
  private final int currentDepth;

  private LtrExpressionIterator(int depth, Iterator<BooleanExpression<C>> nodes) {
    this.currentDepth = depth;
    this.nodes = nodes;
  }

  private LtrExpressionIterator(int depth, LtrExpressionIterator<C> copy) {
    this(depth, copy.nodes);
  }

  @SafeVarargs
  public LtrExpressionIterator(BooleanExpression<C>... nodes) {
    this(0, Arrays.asList(nodes).iterator());
  }

  /**
   * The depth of the current node in this iterator (the last node that was returned with
   * {@link #next()}).
   *
   * @return The depth of the current node in the tree. The root node is <code>0</code>.
   */
  public int getCurrentDepth() {
    if (currentIterator != null) {
      return currentIterator.getCurrentDepth();
    }
    return currentDepth;
  }

  @Override
  public boolean hasNext() {
    return nodes.hasNext()
        || currentIterator != null && currentIterator.hasNext();
  }

  @Override
  public BooleanExpression<C> next() {
    if (currentIterator != null && currentIterator.hasNext()) {
      // Iterate down the tree first
      BooleanExpression<C> next = currentIterator.next();
      return next;
    } else {
      currentIterator = null;
    }

    BooleanExpression<C> current = nodes.next();
    // Keep node iterator for later to iterate down the tree.
    currentIterator = new LtrExpressionIterator<>(currentDepth + 1, current.iterator());

    return current;
  }


}
