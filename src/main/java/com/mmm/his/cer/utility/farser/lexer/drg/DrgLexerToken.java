package com.mmm.his.cer.utility.farser.lexer.drg;


import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;

import java.util.Optional;
import java.util.regex.Pattern;

/**
 * Token class that can be used to represent the pieces of a lexed string.
 * 
 * @author a30w4zz
 *
 */
public class DrgLexerToken implements LexerToken<DrgFormulaToken> {
  /**
   * {@link #PREFIX_SEPARATOR_CHAR} as string.
   */
  public static final String PREFIX_SEPARATOR_STRING = String.valueOf(":");

  /**
   * The regex pattern for {@link #PREFIX_SEPARATOR_STRING}.
   */
  public static final Pattern PREFIX_SEPARATOR_PATTERN =
      Pattern.compile(DrgLexerToken.PREFIX_SEPARATOR_STRING);


  public final DrgFormulaToken type;
  public final String value;

  /**
   * Separated from value with {@link #PREFIX_SEPARATOR_CHAR}.
   */
  public final Optional<String> prefix;

  /**
   * A new token with the value from the {@link TokenType}.
   * 
   * @param type The token type
   */
  public DrgLexerToken(DrgFormulaToken type) {
    this.type = type;
    this.value = type.getValue().orElse(null);
    this.prefix = Optional.empty();
  }

  /**
   * A new token with a provided value.
   * 
   * @param type The token type
   * @param value The token value
   */
  public DrgLexerToken(DrgFormulaToken type, String value) {
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
  public DrgLexerToken(DrgFormulaToken type, String value, Optional<String> prefix) {
    this.type = type;
    this.value = value;
    this.prefix = prefix;
  }


  @Override
  public DrgFormulaToken getType() {
    return type;
  }

  @Override
  public String getValue() {
    return value;
  }

  public Optional<String> getPrefix() {
    return prefix;
  }


  @Override
  public String toString() {
    if (type == DrgFormulaToken.ATOM) {
      return type.name() + "<" + value + ">"
          + (prefix.isPresent() ? " with prefix " + prefix.get() : "");
    }
    return type.toString();
  }
}
