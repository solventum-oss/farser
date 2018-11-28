package com.mmm.his.cer.utility.farser.lexer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Major types of tokens that we need to be concerned with when Lexing a DRG formula.
 * 
 * @author a30w4zz
 *
 */
public enum TokenType {
  LPAREN('('),
  RPAREN(')'),
  ATOM(null),
  AND('&'),
  OR('|'),
  NOT('~');

  /**
   * All the values of the existing {@link TokenType}s which are not <code>null</code>.
   */
  public static final List<Character> TOKEN_TYPE_VALUES;

  static {
    List<Character> nonNullValues = new ArrayList<>();
    for (TokenType type : values()) {
      Optional<Character> value = type.getCharValue();
      if (value.isPresent()) {
        nonNullValues.add(value.get());
      }
    }

    TOKEN_TYPE_VALUES = Collections.unmodifiableList(nonNullValues);
  }

  private Optional<Character> value = null;
  private Optional<String> stringValue = null;

  /**
   * A new token type.
   * 
   * @param value The token value, or <code>null</code> if there is no specific value associated
   *        with this token type
   */
  private TokenType(Character value) {
    this.value = Optional.ofNullable(value);
    this.stringValue = Optional.ofNullable(String.valueOf(value));
  }

  public Optional<Character> getCharValue() {
    return value;
  }

  public Optional<String> getStringValue() {
    return stringValue;
  }

  /**
   * Returns the {@link TokenType} which has the given character value.
   * 
   * @param value The character value to look for
   * @return The optional token type if there is one
   */
  public static Optional<TokenType> getForValue(char value) {
    for (TokenType type : values()) {
      Optional<Character> typeValue = type.getCharValue();
      if (typeValue.isPresent() && typeValue.get().equals(value)) {
        return Optional.of(type);
      }
    }

    return Optional.empty();
  }

}
