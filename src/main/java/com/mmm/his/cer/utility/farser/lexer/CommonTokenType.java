package com.mmm.his.cer.utility.farser.lexer;


/**
 * These common token types can be used to mark tokens in your own {@link TokenType} implementation.
 * Some of them are mandatory and have to exist in your own {@link TokenType} implementation, others
 * are not mandatory.
 * 
 * @author a5rn0zz
 *
 */
public enum CommonTokenType {

  /**
   * An atom token is the content which does not match any other token (e.g. all the characters in
   * between recognized tokens).<br />
   * A token marked with this type is mandatory in any token type implementation.
   */
  ATOM(true),

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
  NOT(false);

  private final boolean mandatory;

  private CommonTokenType(boolean mandatory) {
    this.mandatory = mandatory;
  }

  public boolean isMandatory() {
    return mandatory;
  }

}
