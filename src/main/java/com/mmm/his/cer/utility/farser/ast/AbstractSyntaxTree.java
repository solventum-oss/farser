package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;

/**
 * Class that wraps a {@link BooleanExpression} and provides methods to evaluate it.
 *
 * @param <C> the type of context
 * @author Mike Funaro
 * @author Thomas Naeff
 */
public class AbstractSyntaxTree<C> extends BooleanNonTerminal<C> {

  private Expression<C, Boolean> ast;

  public AbstractSyntaxTree(Expression<C, Boolean> ast) {
    this.ast = ast;
  }

  public void setAst(Expression<C, Boolean> ast) {
    this.ast = ast;
  }

  @Override
  public Boolean evaluate(C context) {
    return this.ast.evaluate(context);
  }

  @Override
  public LtrExpressionIterator<C> iterator() {
    return new LtrExpressionIterator<>(ast);
  }

  @Override
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
  public ExpressionResult<C> evaluateExpression(C context) {
    boolean evaluate = evaluate(context);
    return new ExpressionResult<>(evaluate, context);
  }

}
