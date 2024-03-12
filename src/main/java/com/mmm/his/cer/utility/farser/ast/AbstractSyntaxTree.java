package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;

/**
 * Class that wraps a {@link BooleanExpression} and provides methods to evaluate it.
 *
 * @param <C> the type of context
 * @author Mike Funaro
 * @author Thomas Naeff
 */
public class AbstractSyntaxTree<C> extends NonTerminal<C> {

  private BooleanExpression<C> ast;

  public AbstractSyntaxTree(BooleanExpression<C> ast) {
    this.ast = ast;
  }

  public void setAst(BooleanExpression<C> ast) {
    this.ast = ast;
  }

  @Override
  public boolean evaluate(C context) {
    return this.ast.evaluate(context);
  }

  /**
   * Evaluate an expression that was previously built by the parser.
   *
   * @param context the context object that will be used in the evaluation.
   * @return {@link ExpressionResult} ExpressionResult object which will have the data about the
   *         outcome of the evaluation.
   */
  public ExpressionResult<C> evaluateExpression(C context) {
    boolean evaluate = evaluate(context);
    return new ExpressionResult<>(evaluate, context);
  }

}
