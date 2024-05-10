package com.mmm.his.cer.utility.farser.ast.node.operator.set;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.SetNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import java.util.ArrayList;
import java.util.List;

/**
 * Base node for performing Set logic. This will ensure that the left and right nodes are processed
 * so that data is fetched, and then perform the "math" as defined in the subclass.
 *
 * @author Mike Funaro
 */
public abstract class SetLogicOperator<C extends LookupContext<T>, T> extends
    SetNonTerminal<C, List<T>> {

  @Override
  public List<T> evaluate(C context) {
    List<T> leftData = new ArrayList<>(this.left.evaluate(context));
    List<T> rightData = this.right.evaluate(context);

    return applyLogic(leftData, rightData);

  }

  /**
   * Method that subclasses will define to perform a specific type of operation on the two lists
   * that are passed in. Generally, all operations should happen to the left list. E.G.
   * left.removeAll(right).
   *
   * @param left  the left list of items.
   * @param right the right list of items.
   * @return the result of the operation.
   */
  public abstract List<T> applyLogic(List<T> left, List<T> right);
}
