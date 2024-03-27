package com.mmm.his.cer.utility.farser.lexer.tokentype;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Optional;

/**
 * All recognized types of tokens that we need to be concerned with when lexing a DRG formula.
 *
 * @author a30w4zz
 */
public enum TestToken implements TokenType<TestToken> {

  /**
   * Any substring which is not in a set of defined token characters here in {@link TokenType}. This
   * can be a list name in the application of our DRG formulas, it can be a method name etc.<br />
   * The atom token type here has no defined value. It will be available as {@link LexerToken} with
   * the value set as the non-token substring.
   */
  ATOM(
      null,
      CommonTokenType.ATOM),

  /**
   *
   */
  SPACE(
      "",
      CommonTokenType.SPACE),

  SOME_TOKEN(
      "x"),

  /**
   * Multiple characters in a token.
   */
  MULTI_CHARACTER_TOKEN(
      "###"),

  /**
   * Multiple characters in a token, containing characters which also exists as
   * {@link #SINGLE_IN_MULTI} and {@link #MULTI_IN_MULTI}.
   */
  MULTI_IN_SINGLE_TOKEN(
      "*!!*"),

  /**
   * A single character which also exists in {@link #MULTI_CHARACTER_TOKEN}.
   */
  SINGLE_IN_MULTI(
      "!"),

  /**
   * Multiple characters which also exists in {@link #MULTI_CHARACTER_TOKEN}.
   */
  MULTI_IN_MULTI(
      "!!");

  private final Optional<String> value;
  private final Optional<CommonTokenFlag> commonType;

  /**
   * A new token type.
   *
   * @param value      The token value, or <code>null</code> if not used
   * @param commonType The common token type, or <code>null</code> if not needed
   */
  TestToken(String value, CommonTokenFlag commonType) {
    this.value = Optional.ofNullable(value);
    this.commonType = Optional.ofNullable(commonType);

  }

  /**
   * A new token type.
   *
   * @param value The token value
   */
  TestToken(String value) {
    this(value, null);

  }

  @Override
  public Optional<String> getValue() {
    return value;
  }

  @Override
  public Optional<CommonTokenFlag> getCommonTokenType() {
    return commonType;
  }

}
