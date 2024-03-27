package com.mmm.his.cer.utility.farser.lexer.drg;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.AstTokenType;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Optional;

/**
 * All recognized types of tokens that we need to be concerned with when lexing a <b>HIS DRG grouper
 * logic</b> formula string. This token enum also includes configuration types (flags) for building
 * an AST.
 *
 * @author Mike Funaro
 */
public enum DrgFormulaToken implements TokenType<DrgFormulaToken>, AstTokenType<DrgFormulaToken> {

  /**
   * Any substring which is not in a set of defined token characters here in {@link TokenType}. This
   * can be a list name in the application of our DRG formulas, it can be a method name etc.<br />
   * The atom token type here has no defined value. It will be available as {@link LexerToken} with
   * the value set as the non-token substring.
   */
  ATOM(null, CommonTokenType.ATOM),

  /**
   * A space in the formula string. Gets filtered out in {@link DrgFormulaTokenFactory}.
   */
  SPACE(" ", CommonTokenType.SPACE),

  /**
   * Left parenthesis.
   */
  LPAREN("(", AstCommonTokenType.LPAREN),

  /**
   * Right parenthesis.
   */
  RPAREN(")", AstCommonTokenType.RPAREN),

  /**
   * Logical AND.
   */
  AND("&", AstCommonTokenType.AND, 1),

  /**
   * Logical OR.
   */
  OR("|", AstCommonTokenType.OR, 2),

  /**
   * Logical NOT.
   */
  NOT("~", AstCommonTokenType.NOT);

  private final String value;
  private final CommonTokenFlag commonType;
  private final int operatorPrecedence;

  /**
   * A new token type.
   *
   * @param value      The token value, or <code>null</code> if not used
   * @param commonType The common token type, or <code>null</code> if not needed
   */
  DrgFormulaToken(String value, CommonTokenFlag commonType) {
    this(value, commonType, AstTokenType.NOT_AN_OPERATOR);
  }

  /**
   * A new token type - an operator with precedence.
   *
   * @param value              The token value, or <code>null</code> if not used
   * @param operatorPrecedence The precedence of the operator
   */
  DrgFormulaToken(String value, CommonTokenFlag commonType, int operatorPrecedence) {
    this.value = value;
    this.commonType = commonType;
    this.operatorPrecedence = operatorPrecedence;
  }

  /**
   * A new token type.
   *
   * @param value The token value
   */
  DrgFormulaToken(String value) {
    this(value, null);

  }

  @Override
  public Optional<String> getValue() {
    return Optional.ofNullable(value);
  }

  @Override
  public Optional<CommonTokenFlag> getCommonTokenType() {
    return Optional.ofNullable(commonType);
  }

  @Override
  public int getOperatorPrecedence() {
    return operatorPrecedence;
  }

}
