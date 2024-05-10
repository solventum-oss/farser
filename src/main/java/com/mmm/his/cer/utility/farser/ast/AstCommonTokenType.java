package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;

/**
 * Common token types for AST ({@link AstDescentParser}) logic.
 *
 * @author Thomas Naeff
 *
 */
public enum AstCommonTokenType implements CommonTokenFlag {

  /**
   * A left parenthesis "(".
   */
  LPAREN,

  /**
   * A right parenthesis ")".
   */
  RPAREN,

  /**
   * A negation/not.
   */
  NOT,

  /**
   * An "AND" operator.<br>
   * <br>
   * This flag exists for backwards compatibility when a {@link NodeSupplier} is used without the
   * {@link NodeSupplier#createNonTerminalNode(LexerToken)} method being overridden.
   *
   * @deprecated Instead of using this flag, it is preferred to override
   *             {@link NodeSupplier#createNonTerminalNode(LexerToken)} with your custom token
   *             types.
   */
  @Deprecated
  AND,

  /**
   * An "OR" operator.<br>
   * <br>
   * This flag exists for backwards compatibility when a {@link NodeSupplier} is used without the
   * {@link NodeSupplier#createNonTerminalNode(LexerToken)} method being overridden.
   *
   * @deprecated Instead of using this flag, it is preferred to override
   *             {@link NodeSupplier#createNonTerminalNode(LexerToken)} with your custom token
   *             types.
   */
  @Deprecated
  OR;

}
