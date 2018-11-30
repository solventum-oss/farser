package com.mmm.his.cer.utility.farser.lexer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * All recognized types of tokens that we need to be concerned with when lexing a string.
 * 
 * @author a30w4zz
 *
 */
public enum TokenType {

  /**
   * Left parenthesis.
   */
  LPAREN('('),

  /**
   * Right parenthesis.
   */
  RPAREN(')'),

  /**
   * Any substring which is not in a set of defined token characters here. This can be a list name
   * in the application of our DRG formulas, it can be a method name etc.<br />
   * The atom token type here has no defined value. It will be available as {@link LexerToken} with
   * the value set to the non-token substring.
   */
  ATOM(null),

  /**
   * Logical AND.
   */
  AND('&'),

  /**
   * Logical OR.
   */
  OR('|'),

  /**
   * Logical NOT.
   */
  NOT('~'),

  /**
   * A comma separating multiple function parameters.
   */
  COMMA(',');


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
