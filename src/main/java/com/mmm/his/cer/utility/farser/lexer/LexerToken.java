package com.mmm.his.cer.utility.farser.lexer;

/**
 * Token class which represents the pieces of a lexed string.
 *
 * @param <T> The type of tokens used
 * @author Mike Funaro
 */
public interface LexerToken<T extends TokenType<?>> {


  /**
   * The token.
   *
   * @return The token
   */
  T getType();

  /**
   * The value of the token.<br />
   * Is the same as {@link TokenType#getValue()} for any {@link TokenType} other than the ones
   * marked with {@link CommonTokenType#ATOM}.
   *
   * @return The token value
   */
  String getValue();

}
