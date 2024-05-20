package com.mmm.his.cer.utility.farser.ast_complex.setup.lex;

import com.mmm.his.cer.utility.farser.ast_complex.setup.ComplexTestTokenType;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class ComplexTestToken implements LexerToken<ComplexTestTokenType> {

  public final ComplexTestTokenType type;
  public final String value;

  public ComplexTestToken(ComplexTestTokenType type, String value) {
    this.type = type;
    this.value = value;
  }

  @Override
  public ComplexTestTokenType getType() {
    return type;
  }

  @Override
  public String getValue() {
    return value;
  }

  @Override
  public String toString() {
    return type + ":" + value;
  }

}
