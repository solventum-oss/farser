package com.mmm.his.cer.utility.farser.ast.node.type;

import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.node.operator.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.Or;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;

/**
 * Interface for calling applications to implement so that they can provide custom terminal nodes
 * for special logic to be evaluated within the AST.
 *
 * @param <T> the token type that will be used to create the node, based on the list of lexed tokens
 *            that are used to create the AST
 * @param <C> the parametric type on {@link BooleanExpression} terminal nodes - the context data
 *            passed in when the AST is evaluated
 *
 * @author Mike Funaro
 */
public interface NodeSupplier<L extends LexerToken<?>, C> {

  /**
   * Create a terminal node. This is type defined on the class. The input will be a token of a
   * particular type which is used in the body of the method to create an instance of
   * {@link BooleanExpression} that is type defined again using the types on the class.
   *
   * @param token The formula token/operand for which to create the node for
   * @return BooleanExpression that was instantiated in this method.
   */
  BooleanExpression<C> createNode(L token);

  default NonTerminal<C> createNonTerminalNode(L token) {
    AstCommonTokenType type =
        (AstCommonTokenType) token.getCommonType()
        .orElseThrow(() -> new UnsupportedOperationException(
            "The non-terminal node supplier can only create nodes with a "
                + CommonTokenType.class.getSimpleName()));
    switch (type) {
      case RIGHT:
        return new And<>();
      case LEFT:
        return new Or<>();
      case NOT:
        return new Not<>();
      default:
        throw new UnsupportedOperationException(
            "Invalid "
                + CommonTokenType.class.getSimpleName()
                + "."
                + type
                + " for the non-terminal node supplier");
    }
  }


}
