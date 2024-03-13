package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * A printer to output an {@link AbstractSyntaxTree} in visual representation, for example to
 * convert a formula to a different format or simply for debugging or display purposes.
 *
 * @author Thomas Naeff
 *
 */
public final class AbstractSyntaxTreePrinter {

  public static final String DEFAULT_INDENTATION = "  ";

  private AbstractSyntaxTreePrinter() {
    // Private. Only static methods.
  }

  /**
   * Prints a simple tree representation.<br>
   * Uses {@link #printNodeSimple(BooleanExpression)}.
   *
   * @param ast The AST
   * @return The tree representation as string
   */
  public static String printTree(AbstractSyntaxTree<?> ast) {
    return printTree(ast, DEFAULT_INDENTATION, AbstractSyntaxTreePrinter::printNodeSimple);
  }

  /**
   *
   * Prints a tree representation based on the provided node printing function.
   *
   * @param ast       The AST to print
   * @param printNode The function which determines how to print a node. The
   *                  {@link BooleanExpression} function input may be <code>null</code> when the
   *                  printing is past the last node (to possibly finalize/close data structures).
   *                  See {@link #printNodeSimple(BooleanExpression, AstPrinterContext)} for a
   *                  simple starting point.
   * @return
   */
  public static <T> String printTree(AbstractSyntaxTree<T> ast,
      Function<BooleanExpression<T>, String> printNode) {
    return printTree(ast, DEFAULT_INDENTATION, (node, next) -> printNode.apply(node));
  }

  /**
   * Prints a simple tree representation.
   *
   * @param ast       The AST
   * @param printNode The function which determines how to print a node. The
   *                  {@link BooleanExpression} function input may be <code>null</code> when the
   *                  printing is past the last node (to possibly finalize/close data structures).
   *                  See {@link #printNodeSimple(BooleanExpression, AstPrinterContext)} for a
   *                  simple starting point.
   * @return The tree representation as string
   */
  public static <T> String printTree(AbstractSyntaxTree<T> ast,
      BiFunction<BooleanExpression<T>, AstPrinterContext<T>, String> printNode) {
    return printTree(ast, DEFAULT_INDENTATION, printNode);
  }

  /**
   * Prints a simple tree representation.
   *
   * @param ast       The AST
   * @param printNode The function which determines how to print a node. The
   *                  {@link BooleanExpression} function input may be <code>null</code> when the
   *                  printing is past the last node (to possibly finalize/close data structures).
   *                  See {@link #printNodeSimple(BooleanExpression, AstPrinterContext)} for a
   *                  simple starting point.
   * @return The tree representation as string
   */
  public static <T> String printTree(AbstractSyntaxTree<T> ast, String indentation,
      BiFunction<BooleanExpression<T>, AstPrinterContext<T>, String> printNode) {
    StringBuilder sb = new StringBuilder();
    int previousDepth = 0;
    int currentDepth = 0;

    List<String> prefixes = new ArrayList<>();
    LtrExpressionIterator<T> iter = ast.iterator();

    while (iter.hasNext()) {
      // Need to get next first, so that depth of new/current node is available.
      BooleanExpression<T> node = iter.next();
      previousDepth = currentDepth;
      currentDepth = iter.getCurrentDepth();

      String prefix = prefix(prefixes, currentDepth, indentation);
      AstPrinterContext<T> context = createPrinterContext(iter, prefix, previousDepth);
      appendPrinted(sb, printNode, node, context);
    }

    // Call the printing again for each depth, from the current node all the way back up to the
    // root depth. This allows the printer to possibly close any nesting-dependent structures.
    int nextDepth = iter.getPeekedDepth();
    for (; currentDepth >= 1; currentDepth--) {
      String prefix = prefix(prefixes, currentDepth, indentation);
      AstPrinterContext<T> context = new AstPrinterContext<>(prefix, currentDepth,
          AstPrintDirection.UP, null, nextDepth, AstPrintDirection.UP);
      appendPrinted(sb, printNode, null, context);
    }

    return sb.toString();
  }

  /**
   * Handles the printer function execution and adding it to the buffer.
   *
   * @param sb        The buffer to add to
   * @param printNode The printer function
   * @param node      The node to print. May be <code>null</code>.
   * @param context   The printer context
   */
  private static <T> void appendPrinted(StringBuilder sb,
      BiFunction<BooleanExpression<T>, AstPrinterContext<T>, String> printNode,
      BooleanExpression<T> node, AstPrinterContext<T> context) {
    String printed = printNode.apply(node, context);
    // Do not add if printer function returned NULL. Documented behavior on this printer class
    // constructors.
    if (printed != null ) {
      sb.append(printed);
    }
  }

  /**
   * Gathers information to create the {@link AstPrinterContext}.
   *
   * @param iter          The current iterator
   * @param prefix        The current prefix
   * @param previousDepth The depth of the previous node
   * @return The context object
   */
  private static <T> AstPrinterContext<T> createPrinterContext(LtrExpressionIterator<T> iter,
      String prefix, int previousDepth) {
    BooleanExpression<T> peeked = null;
    peeked = iter.hasNext() ? iter.peek() : null;

    int currentDepth = iter.getCurrentDepth();
    int peekedDepth = iter.getPeekedDepth();
    AstPrintDirection direction = AstPrintDirection.fromDepthDelta(currentDepth - previousDepth);
    AstPrintDirection peekedDirection =
        AstPrintDirection.fromDepthDelta(peekedDepth - currentDepth);

    return new AstPrinterContext<>(prefix, currentDepth, direction, peeked, peekedDepth,
        peekedDirection);
  }

  /**
   * This method maintains the list of prefixes for each depth.
   *
   * @param prefixes    The prefixes cache (with the depth as list index). This collections gets
   *                    updated internally.
   * @param depth       The depth of the node
   * @param indentation The indentation to add for each new depth
   * @return The prefix for the <code>currentDepth</code>
   */
  private static String prefix(List<String> prefixes, int depth, String indentation) {
    if (prefixes.size() >= depth) {
      // If a prefix already exists for the current depth, then return it.
      return prefixes.get(depth - 1);
    } else if (depth == 1) {
      // No prefixes cached yet.
      // Start out with an empty prefix.
      prefixes.add("");
      return "";
    } else {
      // Otherwise take the previous prefix, append one additional indentation,
      // then store and return it.
      int previousDepth = depth - 1;
      String previous = prefixes.get(previousDepth - 1);
      String newPrefix = previous + indentation;
      prefixes.add(newPrefix);
      return newPrefix;
    }

  }

  /**
   * The simplest version of printing a node.<br>
   * This implementation indentates the nodes and ignores any "closing" structured indentation at
   * the end.<br>
   * This is a method that could be provided to {@link #printTree(DrgSyntaxTree, Function)}.
   *
   * @param node The node to print
   * @return The printed representation
   */
  public static String printNodeSimple(BooleanExpression<?> node, AstPrinterContext<?> context) {
    // Ignore any "closing" structure at the end of the tree.
    if (node == null) {
      return null;
    }

    StringBuilder sb = new StringBuilder();
    sb.append(context.prefix);
    sb.append(node.print());
    sb.append(System.lineSeparator());
    return sb.toString();
  }


  /***********************************************************************************************************************
   *
   *
   * @author Thomas Naeff
   *
   * @param <T>
   */
  public static class AstPrinterContext<T> {

    /**
     * The tree depth of the current node. Either 0 (zero) or a positive value.
     */
    public final int depth;

    /**
     * The tree depth of the {@link #next} node. Either 0 (zero) or a positive value if
     * {@link #next} is not <code>null</code>, or {@value LtrExpressionIterator#PEEKED_DEPTH_NONE}
     * when {@link #next} is <code>null</code>.
     */
    public final int nextDepth;

    /**
     * The prefix for indentation. Never <code>null</code>.
     */
    public final String prefix;

    /**
     * The direction of the nesting - in relation to the previous node. Never <code>null</code>.
     */
    public final AstPrintDirection direction;

    /**
     * The next direction of the nesting - in relation to the current node. May be <code>null</code>
     * when the last node is reached.
     */
    public final AstPrintDirection nextDirection;

    /**
     * The next node (peek). May be <code>null</code> when the iteration/printing is at or past the
     * last node.
     */
    public final BooleanExpression<T> next;

    private AstPrinterContext(String prefix, int depth, AstPrintDirection direction,
        BooleanExpression<T> next, int nextDepth, AstPrintDirection nextDirection) {
      this.depth = depth;
      this.nextDepth = nextDepth;
      this.prefix = prefix;
      this.direction = direction;
      this.nextDirection = nextDirection;
      this.next = next;
    }

  }

  /**
   * An enumeration with a few flags that inform about the printing direction of one node in
   * relation to its previous node.
   *
   * @author Thomas Naeff
   *
   */
  public enum AstPrintDirection {
    /**
     * Travelling on same level in the tree (depth is the same as the previous node).
     */
    SAME,
    /**
     * Travelling up the tree (depth got reduced compared to the previous node).
     */
    UP,
    /**
     * Travelling down the tree (depth got increased compared to the previous node).
     */
    DOWN;

    /**
     * Returns the direction identifier based on the node depth delta (current/next node depth minus
     * previous node depth).
     *
     * @param depthDelta The depth delta
     * @return The direction
     */
    public static AstPrintDirection fromDepthDelta(int depthDelta) {
      if (depthDelta > 0) {
        return AstPrintDirection.DOWN;
      } else if (depthDelta < 0) {
        return AstPrintDirection.UP;
      }
      return AstPrintDirection.SAME;
    }
  }

}
