package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import com.mmm.his.cer.utility.farser.ast.node.type.MaskedContext;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of a non-terminal node for use in the AST. This class represents a logical AND
 * operation.
 *
 * @param <C> The context type used in the terminal nodes.
 * @author Mike Funaro
 */
public class And<C extends MaskedContext<T>, T> extends AhrqOperator<C, T> {

  @Override
  public EvalResult<T> evaluate(C context) {
    List<T> result;
    boolean totalResult = false;
    
    EvalResult<T> leftData = left.evaluate(context);

    if(leftData.isPassed()){
      result = leftData.getResultList();
    } else {
     result = new ArrayList<>(); 
    }

    EvalResult<T> rightData = right.evaluate(context);

    if(leftData.isPassed()){
      result.addAll(rightData.getResultList());
    } else {
      result = new ArrayList<>();
    }
    
    totalResult = leftData.isPassed() && rightData.isPassed();
    
    return new EvalResult<>(result, totalResult);
  }

  @Override
  public String toString() {
    return "And{" + "left=" + left + ", right=" + right + '}';
  }
}
