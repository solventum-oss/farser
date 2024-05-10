package com.mmm.his.cer.utility.farser.ast.node.operator.set;

import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * msdrg-junior.
 *
 * @author Mike Funaro
 */
public class IntersectionOperator<C extends LookupContext<T>, T> extends SetLogicOperator<C, T> {
  
  @Override
  public List<T> applyLogic(List<T> left, List<T> right) {
    Set<T> leftData = new HashSet<>(left);
    leftData.retainAll(right);
    return new ArrayList<>(leftData);
  }

  @Override
  public String print() {
    return "Intersection Operator";
  }

  @Override
  public String toString() {
    return super.toString();
  }
}
