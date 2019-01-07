package com.mmm.his.cer.utility.farser.lexer.domain;

import com.mmm.his.cer.utility.farser.lexer.LexerTokenFactory;

/**
 * The factory which creates a {@link DomainCodeLexerToken}.
 * 
 * @author a5rn0zz
 *
 */
public class DomainCodeTokenFactory
    implements LexerTokenFactory<DomainCodeLexerToken, DomainCodeToken> {

  @Override
  public DomainCodeLexerToken create(DomainCodeToken tokenType, String value) {
    return new DomainCodeLexerToken(tokenType, value);
  }

}
