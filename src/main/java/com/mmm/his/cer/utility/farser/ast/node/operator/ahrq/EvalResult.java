package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import java.util.List;
import java.util.Objects;

/**
 * Evaluation result container. The compose list will be any values matched at terminal nodes. 
 * The composed boolean will indicate if the evaluation of the AST resulted in a pass.
 *
 * @author Mike Funaro
 */
public class EvalResult<T> {

  private final List<T> resultList;
  private final boolean passed;

  public EvalResult(List<T> resultList, boolean passed) {
    this.resultList = resultList;
    this.passed = passed;
  }

  public List<T> getResultList() {
    return resultList;
  }

  public boolean isPassed() {
    return passed;
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    }
    if (!(object instanceof EvalResult)) {
      return false;
    }
    EvalResult<?> that = (EvalResult<?>) object;
    return passed == that.passed && Objects.equals(resultList, that.resultList);
  }

  @Override
  public int hashCode() {
    return Objects.hash(resultList, passed);
  }
}
