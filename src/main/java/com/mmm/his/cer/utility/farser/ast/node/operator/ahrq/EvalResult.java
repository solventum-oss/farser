package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import java.util.List;

/**
 * farser.
 *
 * @author Mike Funaro
 */
public class EvalResult<T> {

  private List<T> resultList;
  private boolean passed;

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
}
