package com.mmm.his.cer.utility.farser.lexer.set;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.AstTokenType;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Optional;

/**
 * Token type for handling set logic.
 *
 * @author Mike Funaro
 */
public enum SetLogicTokenType implements TokenType<SetLogicTokenType>,
    AstTokenType<SetLogicTokenType> {
  ATOM(null, CommonTokenType.ATOM),
  SPACE(" ", CommonTokenType.SPACE),
  LPAREN("(", AstCommonTokenType.LPAREN),
  RPAREN(")", AstCommonTokenType.RPAREN),
  INTERSECTION("&", 2),
  DIFFERENCE("-", 3),
  UNION("|", 1);

  private final String value;
  private final CommonTokenFlag commonType;
  private final int operatorPrecedence;

  SetLogicTokenType(String value, CommonTokenFlag commonType) {
    this.value = value;
    this.commonType = commonType;
    this.operatorPrecedence = NOT_AN_OPERATOR;
  }

  SetLogicTokenType(String value, int operatorPrecedence) {
    this.value = value;
    this.operatorPrecedence = operatorPrecedence;
    this.commonType = null;
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
    return this.operatorPrecedence;
  }
}
