package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class ComplexTestTerminalNode implements BooleanExpression<ComplexTestAstContext> {

  private final ComplexTestToken token;

  public ComplexTestTerminalNode(ComplexTestToken token) {
    this.token = token;
  }

  @Override
  public Boolean evaluate(ComplexTestAstContext context) {
    return Boolean.valueOf(token.getValue());
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
