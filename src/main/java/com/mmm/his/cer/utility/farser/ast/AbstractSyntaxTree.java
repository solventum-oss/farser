package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;

/**
 * Class that wraps a {@link BooleanExpression} and provides methods to evaluate it.
 *
 * @param <C> the type of context
 * @author Mike Funaro
 * @author Thomas Naeff
 */
public class AbstractSyntaxTree<C, R> {

  private Expression<C, R> ast;

  public AbstractSyntaxTree(Expression<C, R> ast) {
    this.ast = ast;
  }

  public void setAst(Expression<C, R> ast) {
    this.ast = ast;
  }

  private R evaluate(C context) {
    return this.ast.evaluate(context);
  }

  public LtrExpressionIterator<C> iterator() {
    return new LtrExpressionIterator<>(ast);
  }

  public String print() {
    return this.ast.print();
  }

  /**
   * Evaluate an expression that was previously built by the parser.
   *
   * @param context the context object that will be used in the evaluation.
   * @return {@link ExpressionResult} ExpressionResult object which will have the data about the
   *         outcome of the evaluation.
   */
  public ExpressionResult<C, R> evaluateExpression(C context) {
    R result = evaluate(context);
    return new ExpressionResult<>(result, context);
  }

}
