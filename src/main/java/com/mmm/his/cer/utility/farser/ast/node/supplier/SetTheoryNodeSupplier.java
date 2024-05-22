package com.mmm.his.cer.utility.farser.ast.node.supplier;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.NonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.operator.set.DifferenceOperator;
import com.mmm.his.cer.utility.farser.ast.node.operator.set.IntersectionOperator;
import com.mmm.his.cer.utility.farser.ast.node.operator.set.UnionOperator;
import com.mmm.his.cer.utility.farser.ast.node.terminal.DataLookupNode;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.set.SetTheoryToken;

/**
 * Set theory node supplier. Contains the knowledge to create terminal and non-terminal nodes for
 * Set theory evaluation.
 *
 * @param <T> The type that is returned from the LookupContext implementation
 * @author Mike Funaro
 */
public class SetTheoryNodeSupplier<T> implements NodeSupplier<SetTheoryToken, LookupContext<T>> {


  @Override
  public Expression<LookupContext<T>, ?> createNode(SetTheoryToken token) {
    return new DataLookupNode<>(token.value);
  }

  @Override
  public NonTerminal<LookupContext<T>, ?> createNonTerminalNode(SetTheoryToken token) {
    switch (token.type) {
      case DIFFERENCE:
        return new DifferenceOperator<>();
      case UNION:
        return new UnionOperator<>();
      case INTERSECTION:
        return new IntersectionOperator<>();
      default:
        throw new FarserException("Operator type " + token + "not implemented");
    }
  }
}
