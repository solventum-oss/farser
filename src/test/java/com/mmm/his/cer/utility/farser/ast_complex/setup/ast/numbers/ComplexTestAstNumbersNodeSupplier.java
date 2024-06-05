package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.numbers;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.Or;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.strings.ComplexTestInOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.strings.ComplexTestInTableOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.lexer.FarserException;

public class ComplexTestAstNumbersNodeSupplier implements NodeSupplier<ComplexTestToken, ComplexTestAstNumbersContext> {
  
  private boolean isInteger(String value) {
    boolean result = true;
    try {
      Integer.parseInt(value);
    }
    catch(NumberFormatException exc) {
      result = false;
    }
    return result;
  }

  @Override
  public Expression<ComplexTestAstNumbersContext, ?> createNode(final ComplexTestToken inToken) {
    if (isInteger(inToken.value)) {
      return new ComplexTestTerminalNumberNode(inToken);
    }
    return new ComplexTestTerminalLookupNode(inToken);
  }


  @Override
  public BaseNonTerminal<ComplexTestAstNumbersContext, ?> createNonTerminalNode(ComplexTestToken token) {

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
