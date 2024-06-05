package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.strings;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.lexer.FarserException;

public class ComplexTestAstStringsNodeSupplier implements NodeSupplier<ComplexTestToken, ComplexTestAstStringsContext> {

  @Override
  public Expression<ComplexTestAstStringsContext, ?> createNode(final ComplexTestToken inToken) {
    return new ComplexTestTerminalValueNode(inToken);
  }


  @Override
  public BaseNonTerminal<ComplexTestAstStringsContext, ?> createNonTerminalNode(ComplexTestToken token) {

    switch (token.type) {
      case IN:
        return new ComplexTestInOperator<>();
      case IN_TABLE:
        return new ComplexTestInTableOperator<>();
      default:
        throw new FarserException("Operator type " + token + " not implemented");
    }
  }


}
