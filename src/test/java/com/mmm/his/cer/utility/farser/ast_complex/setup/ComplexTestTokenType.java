package com.mmm.his.cer.utility.farser.ast_complex.setup;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.AstTokenType;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Optional;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public enum ComplexTestTokenType
    implements
    TokenType<ComplexTestTokenType>,
    AstTokenType<ComplexTestTokenType> {

  ATOM(null, CommonTokenType.ATOM),
  SPACE(" ", CommonTokenType.SPACE),
  LPAREN("(", AstCommonTokenType.LPAREN),
  RPAREN(")", AstCommonTokenType.RPAREN),
  NOT("!", AstCommonTokenType.NOT),

  IF("IF"),
  THEN("THEN"),
  ELSE("ELSE"),
  ELSEIF("ELSE IF"),
  ENDIF("ENDIF"),

  // Operator precedence: Lower value = stronger bond
  GT(">", 1),
  LT("<", 1),
  EQUAL("=", 2),
  AND("&", 3),
  OR("|", 4),

  IN("IN", 1),
  IN_TABLE("IN TABLE", 1);

  private final String value;
  private final CommonTokenFlag commonType;
  private final int operatorPrecedence;

  ComplexTestTokenType(String value, CommonTokenFlag commonType) {
    this.value = value;
    this.commonType = commonType;
    this.operatorPrecedence = AstTokenType.NOT_AN_OPERATOR;
  }

  ComplexTestTokenType(String value, int operatorPrecedence) {
    this.value = value;
    this.commonType = null;
    this.operatorPrecedence = operatorPrecedence;
  }

  ComplexTestTokenType(String value) {
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
