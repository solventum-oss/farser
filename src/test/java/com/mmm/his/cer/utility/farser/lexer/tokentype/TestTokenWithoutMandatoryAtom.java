package com.mmm.his.cer.utility.farser.lexer.tokentype;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Optional;

/**
 * A test token enum without the mandatory {@link CommonTokenType#ATOM} entry.
 *
 * @author Thomas Naeff
 *
 */
public enum TestTokenWithoutMandatoryAtom implements TokenType<TestTokenWithoutMandatoryAtom> {

  /**
   * Any substring which is not in a set of defined token characters here in {@link TokenType}. This
   * can be a list name in the application of our DRG formulas, it can be a method name etc.<br />
   * The atom token type here has no defined value. It will be available as {@link LexerToken} with
   * the value set as the non-token substring.
   */
  // ATOM(null,
  // CommonTokenType.ATOM),

  /**
   *
   */
  SPACE(
      "",
      CommonTokenType.SPACE),

  SOME_TOKEN(
      "x");

  private final Optional<String> value;
  private final Optional<CommonTokenFlag> commonType;

  /**
   * A new token type.
   *
   * @param value      The token value, or <code>null</code> if not used
   * @param commonType The common token type, or <code>null</code> if not needed
   */
  TestTokenWithoutMandatoryAtom(String value, CommonTokenFlag commonType) {
    this.value = Optional.ofNullable(value);
    this.commonType = Optional.ofNullable(commonType);

  }

  /**
   * A new token type.
   *
   * @param value The token value
   */
  TestTokenWithoutMandatoryAtom(String value) {
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
