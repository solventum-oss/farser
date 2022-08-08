package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;

/**
 * Class that wraps a {@link BooleanExpression} and provides methods to evaluate it.
 *
 * @param <T> the type of context
 * @author Mike Funaro
 */
public class DrgSyntaxTree<T> {

  private BooleanExpression<T> ast;

  public DrgSyntaxTree(BooleanExpression<T> ast) {
    this.ast = ast;
  }

  public void setAst(BooleanExpression<T> ast) {
    this.ast = ast;
  }

  /**
   * Evaluate an expression that was previously built by the parser.
   *
   * @param context the context object that will be used in the evaluation.
   * @return {@link ExpressionResult} ExpressionResult object which will have the data about the
   *         outcome of the evaluation.
   */
  public ExpressionResult<T> evaluateExpression(T context) {
    boolean evaluate = this.ast.evaluate(context);
    return new ExpressionResult<>(evaluate, context);
  }

  /**
   * Prints a simple tree representation.
   *
   * @return The tree representation as string
   */
  public String printTree() {
    return printTree(null);
  }

  /**
   * Prints a simple tree representation.
   *
   * @param context the context object that will be used in the evaluation. May be <code>null</code>
   *                to only print the nodes and not print the evaluation result of each)
   * @return The tree representation as string
   */
  public String printTree(C context) {
    StringBuilder sb = new StringBuilder();

    sb.append(printNode(this.ast, context));

    LtrExpressionIterator<C> iter = this.ast.iterator();
    while (iter.hasNext()) {
      sb.append(System.lineSeparator());
      BooleanExpression<C> node = iter.next();
      sb.append(prefix(iter.getCurrentDepth()));
      sb.append(printNode(node, context));
    }

    return sb.toString();
  }

  /**
   * Prints the output of a single node.
   *
   * @param node    The node to print
   * @param context The context for node evaluation. May be <code>null</code> to skip evaluation
   * @return The node output
   */
  private static <C> StringBuilder printNode(BooleanExpression<C> node, C context) {
    StringBuilder sb = new StringBuilder();
    sb.append(String.valueOf(node.print()));
    if (context != null) {
      boolean result = node.evaluate(context);
      sb.append(" = " + result);
    }
    return sb;
  }

  /**
   * Generates a prefix to structure each line of the tree.
   *
   * @param depth The prefix width
   * @return The prefix
   */
  private static String prefix(int depth) {
    StringBuilder sb = new StringBuilder();
    for (; depth > 0; depth--) {
      sb.append("  ");
    }
    return sb.toString();
  }

}
