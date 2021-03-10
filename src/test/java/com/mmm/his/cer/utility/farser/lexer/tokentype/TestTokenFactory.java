package com.mmm.his.cer.utility.farser.lexer.tokentype;

import com.mmm.his.cer.utility.farser.lexer.LexerTokenFactory;

/**
 * The factory which creates a {@link TestLexerToken}.
 *
 * @author a5rn0zz
 */
public class TestTokenFactory implements LexerTokenFactory<TestLexerToken, TestToken> {

  @Override
  public TestLexerToken create(TestToken tokenType, String value) {
    return new TestLexerToken(tokenType, value);
  }


}
