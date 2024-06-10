package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

public class ComplexTestTerminalContainsNode implements Expression<ComplexTestAstContext, Boolean> {
  
  private final ComplexTestToken token;

  public ComplexTestTerminalContainsNode(ComplexTestToken token) {
    this.token = token;
  }

  @Override
  public Boolean evaluate(ComplexTestAstContext context) {
    return context.contains(token.getValue());
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
