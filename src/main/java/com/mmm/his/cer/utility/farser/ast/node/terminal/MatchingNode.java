package com.mmm.his.cer.utility.farser.ast.node.terminal;

import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.EvalResult;
import com.mmm.his.cer.utility.farser.ast.node.type.AhrqContext;
import com.mmm.his.cer.utility.farser.ast.node.type.Attribute;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Set;

/**
 * A terminal node to be used in AHRQ logical operators. This is a simple matching node but returns
 * a specialized container to handle the requirements of AHRQ ASTs.
 *
 * @author Mike Funaro
 */
public class MatchingNode<C extends AhrqContext> implements Expression<C, EvalResult<Attribute>> {

  private final Attribute value;

  public MatchingNode(Attribute value) {
    this.value = value;
  }

  @Override
  public EvalResult<Attribute> evaluate(C context) {
    Set<Attribute> mask = context.getMask();
    boolean contains = mask.contains(value);

    if (contains) {
      return new EvalResult<>(Arrays.asList(value), true);
    }
    return new EvalResult<>(new ArrayList<>(), false);
  }

  @Override
  public String print() {
    return "Data Lookup key: " + value;
  }
}
