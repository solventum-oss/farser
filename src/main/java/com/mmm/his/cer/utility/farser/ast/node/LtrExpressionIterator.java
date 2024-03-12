package com.mmm.his.cer.utility.farser.ast.node;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
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

  /**
   * The value returned by {@link #getPeekedDepth()} if {@link #peek()} has not been called yet.
   */
  public static final int PEEKED_DEPTH_NONE = -1;

  private final Iterator<BooleanExpression<C>> nodes;
  private final int currentDepth;
  private Integer depthBeforePeek = null;
  private LtrExpressionIterator<C> currentIterator;
  private BooleanExpression<C> peeked = null;

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
    // If peeked, do not return the depth of the latest (peeked) element. Return the depth of the
    // one before.
    if (depthBeforePeek != null) {
      return depthBeforePeek;
    }

    return getCurrentDepthInternal();
  }

  private int getCurrentDepthInternal() {
    if (currentIterator != null) {
      return currentIterator.getCurrentDepth();
    }
    return currentDepth;
  }

  @Override
  public boolean hasNext() {
    // If already advanced due to 'peek', we know it has a next one.
    if (peeked != null) {
      return true;
    }
    return nodes.hasNext()
        || ((currentIterator != null) && currentIterator.hasNext());
  }

  @Override
  public BooleanExpression<C> next() {
    // Only advance if not already done so by 'peek' for example.
    if (peeked == null) {
      return nextInternal();
    } else {
      BooleanExpression<C> tmp = peeked;
      // Reset, to continue normal.
      peeked = null;
      depthBeforePeek = null;
      return tmp;
    }
  }

  private BooleanExpression<C> nextInternal() {
    if ((currentIterator != null) && currentIterator.hasNext()) {
      // Iterate down the tree first
      return currentIterator.next();
    } else {
      // Iterator is used up. Continue with top level iteration.
      currentIterator = null;
    }

    BooleanExpression<C> current = nodes.next();

    // Keep node iterator for later to iterate down the tree.
    currentIterator = new LtrExpressionIterator<>(currentDepth + 1, current.iterator());

    return current;
  }

  /**
   * Peeks at the next element in the iteration, but does not advance the state of this
   * iterator.<br>
   * A subsequent call to {@link #next()} or {@link #peek()} will return the exact same item.
   *
   * @return The next element in the iteration
   */
  public BooleanExpression<C> peek() {
    // Only peek if not already peeked.
    if (peeked == null) {
      depthBeforePeek = getCurrentDepthInternal();
      peeked = nextInternal();
    }
    return peeked;
  }

  /**
   * Returns the depth of the peeked element (the element returned with {@link #peek()}).<br>
   * Returns {@value #PEEKED_DEPTH_NONE} if no element has been peeked yet or if the iterator has
   * already been advanced with {@link #next()}.
   *
   * @return The depth of the peeked element
   */
  public int getPeekedDepth() {
    return peeked != null ? getCurrentDepthInternal() : PEEKED_DEPTH_NONE;
  }

}
