package com.mmm.his.cer.utility.farser.ast_complex.setup.lex;

import com.mmm.his.cer.utility.farser.ast_complex.setup.ComplexTestTokenType;
import com.mmm.his.cer.utility.farser.lexer.LexerTokenFactory;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class ComplexTestTokenFactory implements LexerTokenFactory<ComplexTestToken, ComplexTestTokenType> {

  @Override
  public ComplexTestToken create(ComplexTestTokenType tokenType, String value) {
    if (tokenType == ComplexTestTokenType.SPACE) {
      // Ignore spaces
      return null;
    }

    return new ComplexTestToken(tokenType, value);
  }
}
