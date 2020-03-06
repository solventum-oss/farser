package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.nodes.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.nodes.Terminal;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Class that wraps a {@link BooleanExpression} and provides methods to evaluate it.
 *
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
   * @param operands the list of operand objects that we want to match against.
   * @return {@link ExpressionResult}
   *     ExpressionResult object which will have a the data about the outcome of the evaluation.
   */
  public ExpressionResult<T> evaluateExpression(List<T> operands) {
    Set<T> matches = new HashSet<>();
    boolean evaluate = this.ast.evaluate(operands, matches);
    // Matches gets added to in non-terminal classes. If the root is a terminal and there is a match
    // we need to add the root to the matches to correctly report outward what was matched.
    if (evaluate && matches.isEmpty() && this.ast instanceof Terminal) {
      matches.add(((Terminal<T>) this.ast).getOperandValue());
    }
    return new ExpressionResult<>(evaluate, matches);
  }
}
