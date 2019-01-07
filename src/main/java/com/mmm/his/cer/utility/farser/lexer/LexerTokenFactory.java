package com.mmm.his.cer.utility.farser.lexer;


/**
 * The factory that creates your implementation of a {@link LexerToken}.
 * 
 * @author a5rn0zz
 *
 * @param <L> The {@link LexerToken} type
 * @param <T> The {@link TokenType} type
 */
public interface LexerTokenFactory<L extends LexerToken<T>, T extends TokenType<?>> {

  /**
   * Creates the {@link LexerToken} based on the token type and the value.
   * 
   * @param tokenType The token type
   * @param value The token value
   * @return The lexer token instance
   */
  public L create(T tokenType, String value);

}
