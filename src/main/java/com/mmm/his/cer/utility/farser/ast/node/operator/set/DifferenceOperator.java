package com.mmm.his.cer.utility.farser.ast.node.operator.set;

import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import java.util.ArrayList;
import java.util.List;

/**
 * Set logic operator that will handle the difference between two lists of items.
 *
 * @author Mike Funaro
 */
public class DifferenceOperator<C extends LookupContext<T>, T> extends SetLogicOperator<C, T> {

  @Override
  public List<T> applyLogic(List<T> left, List<T> right) {
    // Get the difference between the left and the right.
    List<T> leftData = new ArrayList<>(left);
    leftData.removeAll(right);
    return leftData;
  }

  @Override
  public String print() {
    return "Difference Operator";
  }

  @Override
  public String toString() {
    return super.toString();
  }
}
