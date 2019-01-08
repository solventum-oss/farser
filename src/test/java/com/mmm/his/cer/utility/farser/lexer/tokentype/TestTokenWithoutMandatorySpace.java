package com.mmm.his.cer.utility.farser.lexer.tokentype;

import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;

import java.util.Optional;

/**
 * All recognized types of tokens that we need to be concerned with when lexing a DRG formula.
 * 
 * @author a30w4zz
 *
 */
public enum TestTokenWithoutMandatorySpace implements
    TokenType<TestTokenWithoutMandatorySpace> {

  /**
   * Any substring which is not in a set of defined token characters here in {@link TokenType}. This
   * can be a list name in the application of our DRG formulas, it can be a method name etc.<br />
   * The atom token type here has no defined value. It will be available as {@link LexerToken} with
   * the value set as the non-token substring.
   */
  ATOM(null,
      CommonTokenType.ATOM),

  /**
   * 
   */
  // SPACE("",
  // CommonTokenType.SPACE),

  SOME_TOKEN("x");

  private final Optional<String> value;
  private final Optional<CommonTokenType> commonType;

  /**
   * A new token type.
   * 
   * @param value The token value, or <code>null</code> if not used
   * @param commonType The common token type, or <code>null</code> if not needed
   */
  private TestTokenWithoutMandatorySpace(String value, CommonTokenType commonType) {
    this.value = Optional.ofNullable(value);
    this.commonType = Optional.ofNullable(commonType);

  }

  /**
   * A new token type.
   * 
   * @param value The token value
   */
  private TestTokenWithoutMandatorySpace(String value) {
    this(value, null);

  }

  @Override
  public Optional<String> getValue() {
    return value;
  }

  @Override
  public Optional<CommonTokenType> getCommonTokenType() {
    return commonType;
  }


}
