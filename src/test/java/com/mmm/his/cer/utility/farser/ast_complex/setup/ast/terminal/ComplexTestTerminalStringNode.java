package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

public class ComplexTestTerminalStringNode implements Expression<ComplexTestAstContext, String> {

  private final ComplexTestToken token;

  public ComplexTestTerminalStringNode(ComplexTestToken token) {
    this.token = token;
  }

  @Override
  public String evaluate(ComplexTestAstContext context) {
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

