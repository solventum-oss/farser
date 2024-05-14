package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
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
public class Not<C extends MaskedContext<T>, T> extends AhrqOperator<C, T> {

  /**
   * For a NOT node, we should only set one child. This implementation allows to set the left child
   * only. And should be the only public API for child setting of a Not node.
   */
  @Override
  public void setLeft(Expression<C, EvalResult<T>> left) {
    // Overridden for updated javadoc.
    super.setLeft(left);
  }

  @Override
  public void setRight(Expression<C, EvalResult<T>> right) {
    // Not nodes will only have one child and that is the left child. Throw error.
    throw new UnsupportedOperationException("Can only set the left-side child for NOT nodes");
  }


  @Override
  public EvalResult<T> evaluate(C context) {

    EvalResult<T> evaluate = left.evaluate(context);

    return new EvalResult<>(new ArrayList<>(), !evaluate.isPassed());
  }

  @Override
  public String toString() {
    return "And{" + "left=" + left + ", right=" + right + '}';
  }
}
