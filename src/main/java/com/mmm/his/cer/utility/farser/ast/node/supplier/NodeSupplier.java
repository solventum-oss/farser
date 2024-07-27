package com.mmm.his.cer.utility.farser.ast.node.supplier;

import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.node.nonterminal.NonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.Or;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import java.util.List;

/**
 * Interface for calling applications to implement so that they can provide custom terminal nodes
 * for special logic to be evaluated within the AST.
 *
 * @param <C> the parametric type on {@link Expression} terminal nodes - the context data passed in
 *            when the AST is evaluated
 *
 * @author Mike Funaro
 */
public interface NodeSupplier<L extends LexerToken<?>, C> {

  /**
   * Create a terminal node. This is type defined on the class. The input will be a token of a
   * particular type which is used in the body of the method to create an instance of
   * {@link Expression} that is type defined again using the types on the class.
   *
   * @param token The formula token/operand for which to create the node for
   * @return Expression that was instantiated in this method.
   */
  Expression<C, ?> createNode(L token);

  /**
   * Creates a terminal node with arguments.
   *
   * @param functionToken The token to create the function node for.
   * @param args The tokens to supply as arguments.
   * @return the expression that was instantiated in this method.
   */
  default Expression<C, ?> createNode(L functionToken, List<L> args) {
    //default so this is not necessary for implementations
    throw new UnsupportedOperationException(
            "createNode with args has to be overridden to support function calls with arguments");
  }

  /**
   * Creates a non-terminal node (e.g. an operand node).
   *
   * @param token The formula token/operand for which to create the node for
   * @return Non-terminal expression that was instantiated in this method.
   */
  default NonTerminal<C, ?> createNonTerminalNode(L token) {
    /*
     * Default implementation to satisfy existing token type (DRG and Domain) implementations which
     * relied on having only AND and OR nodes and this non-terminal-node creation implemented.
     */

    AstCommonTokenType type = (AstCommonTokenType) token.getCommonType()
        .orElseThrow(() -> new UnsupportedOperationException(
            "The default non-terminal node supplier can only create nodes with a "
                + AstCommonTokenType.class.getSimpleName()
                + ". Override this method for custom types."));

    switch (type) {
      case AND:
        return new And<>();
      case OR:
        return new Or<>();
      case NOT:
        return new Not<>();
      default:
        throw new UnsupportedOperationException(
            "Invalid "
                + AstCommonTokenType.class.getSimpleName()
                + "."
                + type
                + " for the non-terminal node supplier");
    }
  }


}
