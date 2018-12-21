package com.mmm.his.cer.utility.farser.lexer;

import java.util.Optional;

/**
 * All recognized types of tokens that we need to be concerned with when lexing a string.
 * 
 * @author a30w4zz
 *
 */
public enum TokenType {

  /*
   ************************************************************************************************
   *
   * The order of appearance of these tokens matters.
   * 
   * The token lookup happens as they appear in this order, meaning that a "<" checked before a "<="
   * will return the "<" and "=" as separate token for an input string of "<=", and not the "<="
   * token as it should.
   * 
   * For optimization it is advised to (when possible) group tokens with the same token value length
   * together. The token lookup retrieves a new substring whenever the token value length changes
   * and avoiding that would be beneficial.
   * 
   */

  /**
   * Any substring which is not in a set of defined token characters here in {@link TokenType}. This
   * can be a list name in the application of our DRG formulas, it can be a method name etc.<br />
   * The atom token type here has no defined value. It will be available as {@link LexerToken} with
   * the value set as the non-token substring.
   */
  ATOM(null),

  /**
   * Assigning a value to a variable.
   */
  ASSIGN(":="),

  /**
   * A logical less-than-equal check (e.g. in an if-statement).
   */
  LT_EQUAL("<="),

  /**
   * A logical greater-than-equal check (e.g. in an if-statement).
   */
  GT_EQUAL(">="),

  /**
   * Left parenthesis.
   */
  LPAREN("("),

  /**
   * Right parenthesis.
   */
  RPAREN(")"),

  /**
   * Logical AND.
   */
  AND("&"),

  /**
   * Logical OR.
   */
  OR("|"),

  /**
   * Logical NOT.
   */
  NOT("~"),

  /**
   * A comma separating multiple function parameters.
   */
  COMMA(","),

  /**
   * A logical is-equal check (e.g. in an if-statement).
   */
  EQUAL("="),

  /**
   * A logical less-than check (e.g. in an if-statement).
   */
  GREATER_THAN(">"),

  /**
   * A logical greater-than check (e.g. in an if-statement).
   */
  LESS_THAN("<");

  /*
   * 
   * See comment at the beginning of the class about the order of the appearance of these tokens.
   * 
   ************************************************************************************************/

  private Optional<String> value = null;

  /**
   * A new token type.
   * 
   * @param value The token value, or <code>null</code> if there is no specific value associated
   *        with this token type
   */
  private TokenType(String value) {
    this.value = Optional.ofNullable(value);
  }

  public Optional<String> getValue() {
    return value;
  }

  /**
   * Returns the {@link TokenType} which has the given character value.
   * 
   * @param value The character value to look for
   * @return The optional token type if there is one
   */
  public static Optional<TokenType> getForValue(String value) {
    for (TokenType type : values()) {
      Optional<String> typeValue = type.getValue();
      if (typeValue.isPresent() && typeValue.get().equals(value)) {
        return Optional.of(type);
      }
    }

    return Optional.empty();
  }

}
