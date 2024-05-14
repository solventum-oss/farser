package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.MaskedContext;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of a non-terminal node for use in the AST. This class represents a logical AND
 * operation with customization of data handling for AHRQ.
 *
 * @param <C> The context type used in the terminal nodes.
 * @param <T> The return type which will be composed in the {@link EvalResult}.
 * @author Mike Funaro
 */
public class And<C extends MaskedContext<T>, T> extends
    BaseNonTerminal<C, EvalResult<T>> implements Expression<C, EvalResult<T>> {

  @Override
  public EvalResult<T> evaluate(C context) {
    List<T> result = new ArrayList<>();
    boolean totalResult;

    EvalResult<T> leftData = left.evaluate(context);

    if (leftData.isPassed()) {
      result = new ArrayList<>(leftData.getResultList());
    }

    EvalResult<T> rightData = right.evaluate(context);

    if (rightData.isPassed()) {
      result.addAll(rightData.getResultList());
    }

    totalResult = leftData.isPassed() && rightData.isPassed();

    return new EvalResult<>(result, totalResult);
  }

  @Override
  public String toString() {
    return "And{" + "left=" + left + ", right=" + right + '}';
  }
  
  
}
