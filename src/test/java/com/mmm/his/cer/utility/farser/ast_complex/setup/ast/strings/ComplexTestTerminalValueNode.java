package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.strings;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

public class ComplexTestTerminalValueNode implements Expression<ComplexTestAstStringsContext, String> {

  private final ComplexTestToken token;

  public ComplexTestTerminalValueNode(ComplexTestToken token) {
    this.token = token;
  }

  @Override
  public String evaluate(ComplexTestAstStringsContext context) {
    return token.value;
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

