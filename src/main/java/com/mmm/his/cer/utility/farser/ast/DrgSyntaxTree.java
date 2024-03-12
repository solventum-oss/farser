package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;

/**
 * Class that wraps a {@link BooleanExpression} and provides methods to evaluate it.
 *
 * @param <C> the type of context
 * @author Mike Funaro
 * @deprecated Use {@link AbstractSyntaxTree} instead for a non-DRG specific named class version with the
 *             exact same functionality.
 */
@Deprecated
public class DrgSyntaxTree<C> extends AbstractSyntaxTree<C> {

  public DrgSyntaxTree(BooleanExpression<C> ast) {
    super(ast);
  }

}
