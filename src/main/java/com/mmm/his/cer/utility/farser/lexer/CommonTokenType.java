package com.mmm.his.cer.utility.farser.lexer;


/**
 * These common token types can be used to mark tokens in your own {@link TokenType} implementation.
 * Some of them are mandatory and have to exist in your own {@link TokenType} implementation, others
 * are not mandatory.
 *
 * @author Thomas Naeff
 */
public enum CommonTokenType {


  /**
   * An atom token is the content which does not match any other token (e.g. all the characters in
   * between recognized tokens).<br />
   * A token marked with this type is mandatory in any token type implementation.<br />
   * The token implemented with this common type does not need a value (its
   * {@link TokenType#getValue()} can return <code>null</code>).
   */
  ATOM(true),

  /**
   * A space symbol in the input string.<br />
   * This token is special as it combines any number of spaces following each other in one single
   * token as its token value (e.g. "a&nbsp;&nbsp;&nbsp;&nbsp;b" results in tokens "a",
   * "&nbsp;&nbsp;&nbsp;&nbsp;" and "b").<br />
   * A token marked with this type is mandatory in any token type implementation.<br />
   * The token implemented with this common type does not need a value as it is handled as special
   * case internally, but returning a blank " " with {@link TokenType#getValue()} is encouraged for
   * readability.<br />
   * See {@link #SPACE_PATTERN} for the pattern used to match this token.
   */
  SPACE(true),

  /**
   * A left parenthesis "(".
   */
  LPAREN(false),

  /**
   * A right parenthesis ")".
   */
  RPAREN(false),

  /**
   * A negation/not.
   */
  NOT(false),

  /**
   * An <code>AND</code> operator.
   */
  AND(false),

  /**
   * An <code>OR</code> operator.
   */
  OR(false);

  /**
   * One or more spaces in a non-capturing group. <br />
   * The non-capturing group is important to avoid extra groups to be captured once the complete
   * token pattern is assembled.
   */
  public static final String SPACE_PATTERN = "(?: )+";

  private final boolean mandatory;

  CommonTokenType(boolean mandatory) {
    this.mandatory = mandatory;
  }

  public boolean isMandatory() {
    return mandatory;
  }

}
