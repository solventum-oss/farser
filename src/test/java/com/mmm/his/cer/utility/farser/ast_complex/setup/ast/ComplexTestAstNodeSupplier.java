package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import com.mmm.his.cer.utility.farser.ast.node.operator.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.Or;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.lexer.FarserException;

public class ComplexTestAstNodeSupplier implements NodeSupplier<ComplexTestToken, ComplexTestAstContext> {

  @Override
  public Expression<ComplexTestAstContext, ?> createNode(final ComplexTestToken inToken) {
    return new ComplexTestTerminalNode(inToken);
  }


  @Override
  public NonTerminal<ComplexTestAstContext, ?> createNonTerminalNode(ComplexTestToken token) {

    switch (token.type) {
      case GT:
        return new ComplexTestGreaterThanOperator<>();
      case LT:
        return new ComplexTestLessThanOperator<>();
      case EQUAL:
        return new ComplexTestEqualOperator<>();
      case AND:
        return new And<>();
      case OR:
        return new Or<>();
      case NOT:
        return new Not<>();
      case IN:
        return new ComplexTestInOperator<>();
      case IN_TABLE:
        return new ComplexTestInTableOperator<>();
      default:
        throw new FarserException("Operator type " + token + " not implemented");
    }

  }


}
