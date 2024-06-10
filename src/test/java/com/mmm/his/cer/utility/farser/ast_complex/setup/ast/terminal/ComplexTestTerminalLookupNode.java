package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class ComplexTestTerminalLookupNode implements Expression<ComplexTestAstContext, Integer> {

  private final ComplexTestToken token;

  public ComplexTestTerminalLookupNode(ComplexTestToken token) {
    this.token = token;
  }

  @Override
  public Integer evaluate(ComplexTestAstContext context) {
    return context.getIntegerMapping(token.value).orElse(null);
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
