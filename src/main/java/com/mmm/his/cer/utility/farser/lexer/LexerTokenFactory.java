package com.mmm.his.cer.utility.farser.lexer;


/**
 * The factory that creates your implementation of a {@link LexerToken}.
 *
 * @param <L> The {@link LexerToken} type
 * @param <T> The {@link TokenType} type
 * @author Thomas Naeff
 */
public interface LexerTokenFactory<L extends LexerToken<T>, T extends TokenType<?>> {

  /**
   * Creates the {@link LexerToken} based on the token type and the value.<br />
   * Can return <code>null</code> to ignore a token.
   *
   * @param tokenType The token type
   * @param value     The token value
   * @return The lexer token instance, or <code>null</code> to ignore the token
   */
  L create(T tokenType, String value);

}
