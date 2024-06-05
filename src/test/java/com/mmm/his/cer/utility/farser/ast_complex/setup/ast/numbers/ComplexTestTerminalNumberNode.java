package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.numbers;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

public class ComplexTestTerminalNumberNode implements Expression<ComplexTestAstNumbersContext, Integer> {

  private final ComplexTestToken token;

  public ComplexTestTerminalNumberNode(ComplexTestToken token) {
    this.token = token;
  }

  @Override
  public Integer evaluate(ComplexTestAstNumbersContext context) {
    return Integer.parseInt(token.getValue());
  }

  @Override
  public String print() {
    return token.value;
  }

  @Override
  public String toString() {
    return token.value;
  }

}
