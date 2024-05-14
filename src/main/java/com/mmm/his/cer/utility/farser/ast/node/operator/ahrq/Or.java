package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.MaskedContext;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of a non-terminal node for use in the AST. This class represents a logical OR
 * operation with customization of data handling for AHRQ.
 *
 * @param <C> The context type used in the terminal nodes.
 * @param <T> The return type which will be composed in the {@link EvalResult}.
 * @author Mike Funaro
 */
public class Or<C extends MaskedContext<T>, T> extends
    BaseNonTerminal<C, EvalResult<T>> implements Expression<C, EvalResult<T>> {

  @Override
  public EvalResult<T> evaluate(C context) {

    EvalResult<T> leftData = left.evaluate(context);
    EvalResult<T> rightData = right.evaluate(context);
    List<T> resultList = new ArrayList<>();

    if (leftData.isPassed()) {
      resultList.addAll(leftData.getResultList());
    }

    if (rightData.isPassed()) {
      resultList.addAll(rightData.getResultList());
    }

    boolean totalResult = leftData.isPassed() || rightData.isPassed();

    return new EvalResult<>(resultList, totalResult);
  }

  @Override
  public String toString() {
    return "And{" + "left=" + left + ", right=" + right + '}';
  }
}
