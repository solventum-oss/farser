package com.mmm.his.cer.utility.farser.lexer.domain;

import com.mmm.his.cer.utility.farser.lexer.LexerTokenFactory;

/**
 * The factory which creates a {@link DomainCodeLexerToken}.
 *
 * @author Thomas Naeff
 */
public class DomainCodeTokenFactory
    implements LexerTokenFactory<DomainCodeLexerToken, DomainCodeToken> {

  @Override
  public DomainCodeLexerToken create(DomainCodeToken tokenType, String value) {
    if (tokenType == DomainCodeToken.SPACE) {
      // Ignore spaces
      return null;
    }

    return new DomainCodeLexerToken(tokenType, value);
  }

}
