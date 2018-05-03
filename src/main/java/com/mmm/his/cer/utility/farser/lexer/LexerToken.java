package com.mmm.his.cer.utility.farser.lexer;


import java.util.Optional;

/**
 * Token class that can be used to represent the pieces of a DRG formula.
 * 
 * @author a30w4zz
 *
 */
public class LexerToken {

  /**
   * Separates {@link #prefix} from the {@link #value}.
   */
  public static final String PREFIX_SEPARATOR = ":";

  public final TokenType type;
  public final String value;
  public final Optional<String> prefix;

  /**
   * A new token.
   * 
   * @param type The token type
   * @param value The token value
   */
  public LexerToken(TokenType type, String value) {
    this.type = type;
    this.value = value;
    this.prefix = Optional.empty();
  }

  /**
   * A new token.
   * 
   * @param type The token type
   * @param value The token value
   * @param prefix The prefix (if there is one)
   */
  public LexerToken(TokenType type, String value, Optional<String> prefix) {
    this.type = type;
    this.value = value;
    this.prefix = prefix;
  }


  public TokenType getType() {
    return type;
  }

  public String getValue() {
    return value;
  }

  public Optional<String> getPrefix() {
    return prefix;
  }


  @Override
  public String toString() {
    if (type == TokenType.ATOM) {
      return "ATOM<" + value + ">" + (prefix.isPresent() ? " with prefix: " + prefix.get() : "");
    }
    return type.toString();
  }
}
