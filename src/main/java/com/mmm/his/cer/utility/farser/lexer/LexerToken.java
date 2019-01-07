package com.mmm.his.cer.utility.farser.lexer;

/**
 * Token class which represents the pieces of a lexed string.
 * 
 * @author a30w4zz
 *
 * @param <T> The type of tokens used
 * 
 */
public interface LexerToken<T extends TokenType<?>> {


  /**
   * The token.
   * 
   * @return The token
   */
  public T getType();

  /**
   * The value of the token.<br />
   * Is the same as {@link TokenType#getValue()} for any {@link TokenType} other than the ones
   * marked with {@link CommonTokenType#ATOM}.
   * 
   * @return The token value
   */
  public String getValue();

}
