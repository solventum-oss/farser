package com.mmm.his.cer.utility.farser.ast.parser;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.AstTokenType;
import com.mmm.his.cer.utility.farser.ast.node.nonterminal.NonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Recursive descent parser that will build an Abstract syntax tree from a formula (list of tokens).
 *
 * @param <L> The type of the token container
 * @param <T> The type of the token type (enum)
 * @param <C> The type of the context used when evaluating the AST
 * @author Mike Funaro
 * @author Thomas Naeff
 */
public class AstDescentParser<L extends LexerToken<T>, T extends TokenType<?>, C, R> {

  private L currentToken;
  private LinkedList<L> tokens;
  private final NodeSupplier<L, C> nodeSupplier;

  /**
   * Ctor.
   *
   * @param tokens       list of tokens to parse into the Abstract syntax tree. May be
   *                     <code>null</code>.
   * @param nodeSupplier a factory which creates nodes for the tree.
   */
  public AstDescentParser(List<L> tokens, NodeSupplier<L, C> nodeSupplier) {
    if (nodeSupplier == null) {
      throw new FarserException(
          "Please provide at least a default supplier argument to "
              + DescentParser.class.getSimpleName()
              + " constructor");
    }
    setTokens(tokens);
    this.nodeSupplier = nodeSupplier;
  }

  public static <L extends LexerToken<T>, T extends TokenType<?>, C, R> AstDescentParser<L, T, C,
      R> of(LinkedList<L> tokens, NodeSupplier<L, C> nodeSupplier) {
    return new AstDescentParser<>(tokens, nodeSupplier);
  }

  /**
   * Sets new tokens so that we can build another AST using the same setup parser. Uses the
   * same {@link NodeSupplier}s which were set when the {@link AstDescentParser} was created.
   */
  public void setTokens(List<L> tokens) {
    this.currentToken = null;
    this.tokens = tokens instanceof LinkedList
        ? (LinkedList<L>) tokens : createLinkedList(tokens);
    if (!this.tokens.isEmpty()) {
      // Position at first token
      this.currentToken = this.tokens.pop();
    }
  }

  /**
   * Build the abstract syntax tree.
   */
  public AbstractSyntaxTree<C, R> buildTree() {
    Expression<C, R> root = expression(null, AstTokenType.NOT_AN_OPERATOR);
    return new AbstractSyntaxTree<>(root);
  }
  
  /**
   * Build the abstract syntax tree from the provided formula.
   *
   * @param tokens list of tokens to parse into the Abstract syntax tree.
   */
  public AbstractSyntaxTree<C, R> buildTree(List<L> tokens) {
    setTokens(tokens);
    Expression<C, R> root = expression(null, AstTokenType.NOT_AN_OPERATOR);
    return new AbstractSyntaxTree<>(root);
  }

  private LinkedList<L> createLinkedList(List<L> tokens) {
    LinkedList<L> linkedList = new LinkedList<>();
    if (tokens != null) {
      linkedList.addAll(tokens);
    }
    return linkedList;
  }

  /**
   * Expression method which will build the lower precedence elements after parsing a term.
   *
   * @param <X>                    A dummy data type for the node evaluation result types to avoid
   *                               the use of <code>?</code> and the need for (unchecked) casting.
   *                               In general, it can not programmatically guarantee that one nodes
   *                               evaluation return type matches the other. It has to rely on
   *                               runtime (class cast) exceptions when malformed formulas or
   *                               implementations are used.
   * @param left                   The node to be used (or passed further down) as left-side node
   * @param leftOperatorPrecedence The operator precedence of the provided <code>left</code> node
   * @return Potentially a new (non-terminal/operator) node with a potential new evaluation return
   *     type. Or the input <code>left</code> node passed through with a matching evaluation
   *     return type.
   */
  private <X> Expression<C, X> expression(Expression<C, X> left, int leftOperatorPrecedence) {
    left = term(left, leftOperatorPrecedence);
    // Higher value means lower precedence
    while (getCurrentTokenAstType().isLowerOrSamePrecedence(leftOperatorPrecedence)) {
      NonTerminal<C, X> operator = uncheckedCast(
          nodeSupplier.createNonTerminalNode(currentToken));
      // Save the current operator precedence before advancing the token iterator
      int operatorPrecedence = getCurrentOperatorPrecedence();
      this.eat();
      operator.setLeft(left);
      Expression<C, X> right = term(left, operatorPrecedence);
      operator.setRight(right);
      // The non-terminal/operator node, as combination of left/right evaluation, may have a
      // different evaluation return type than the individual left/right nodes.
      left = uncheckedCast(operator);
    }
    return left;
  }

  /**
   * Term method which will build the higher precedence elements after parsing the factors or
   * operands.
   *
   * @param <X>                    A dummy data type for the node evaluation result types to avoid
   *                               the use of <code>?</code> and the need for (unchecked) casting.
   *                               In general, it can not programmatically guarantee that one nodes
   *                               evaluation return type matches the other. It has to rely on
   *                               runtime (class cast) exceptions when malformed formulas or
   *                               implementations are used.
   * @param left                   The node to be used (or passed further down) as left-side node
   * @param leftOperatorPrecedence The operator precedence of the provided <code>left</code> node
   * @return Potentially a new (non-terminal/operator) node with a potential new evaluation return
   *     type. Or the input <code>left</code> node passed through with a matching evaluation
   *     return type.
   */
  private <X> Expression<C, X> term(Expression<C, X> left, int leftOperatorPrecedence) {
    left = factor(left, leftOperatorPrecedence);
    while (getCurrentTokenAstType().isHigherPrecedence(leftOperatorPrecedence)) {
      NonTerminal<C, X> operator = uncheckedCast(
          nodeSupplier.createNonTerminalNode(currentToken));
      // Save the current operator precedence before advancing the token iterator
      int operatorPrecedence = getCurrentOperatorPrecedence();
      this.eat();
      operator.setLeft(left);
      Expression<C, X> right = term(left, operatorPrecedence);
      operator.setRight(right);
      // The non-terminal/operator node, as combination of left/right evaluation, may have a
      // different evaluation return type than the individual left/right nodes.
      left = uncheckedCast(operator);
    }
    return left;
  }

  /**
   * Method which will build the negation/not node, with only one child node.
   *
   * @param <R>                    The (boolean) return type of the negated <code>left</code> node,
   *                               as well as the (boolean) return type of the returned not-node.
   *                               This data type is not set as {@link Boolean} to avoid for
   *                               (unchecked) casting. In general, it can not programmatically
   *                               guarantee that one nodes evaluation return type matches the
   *                               other. It has to rely on runtime (class cast) exceptions when
   *                               malformed formulas or implementations are used.
   * @param left                   The node to be used (or passed further down) as left-side node
   * @param leftOperatorPrecedence The operator precedence of the provided <code>left</code> node
   * @return A new (non-terminal/operator) node with a new evaluation return type
   */
  private <R> Expression<C, R> not(Expression<C, R> left, int leftOperatorPrecedence) {
    NonTerminal<C, R> operator = uncheckedCast(
        nodeSupplier.createNonTerminalNode(currentToken));
    this.eat(AstCommonTokenType.NOT); // Move iterator if 'NOT'
    left = factor(left, leftOperatorPrecedence);
    operator.setLeft(left);
    // The non-terminal/operator node, as combination of left/right evaluation, may have a
    // different evaluation return type than the individual left/right nodes.
    return uncheckedCast(operator);
  }

  /**
   * Factor out a single operand.
   *
   * @param <X>                    A dummy data type for the node evaluation result types to avoid
   *                               the use of <code>?</code> and the need for (unchecked) casting.
   *                               In general, it can not programmatically guarantee that one nodes
   *                               evaluation return type matches the other. It has to rely on
   *                               runtime (class cast) exceptions when malformed formulas or
   *                               implementations are used.
   * @param left                   The node to be used (or passed further down) as left-side node
   * @param leftOperatorPrecedence The operator precedence of the provided <code>left</code> node
   * @return Potentially a new (non-terminal/operator or ATOM) node with a potential new evaluation
   *     return type. Or the input <code>left</code> node passed through with a matching
   *     evaluation return type.
   */
  private <X> Expression<C, X> factor(Expression<C, X> left, int leftOperatorPrecedence) {
    TokenType<?> tokenType = currentToken.getType();
    // Get common type for generic checking.
    // Ok to return 'null', it is only used in NPE safe logic below.
    CommonTokenFlag commonType = tokenType.getCommonTokenType().orElse(null);
    if (commonType == CommonTokenType.ATOM) {
      left = handleAtomToken();
    } else if (commonType == AstCommonTokenType.LPAREN) {
      this.eat(AstCommonTokenType.LPAREN); // Move iterator if 'LPAREN'
      left = this.expression(left, leftOperatorPrecedence);
      this.eat(AstCommonTokenType.RPAREN); // Move iterator if 'RPAREN'
    } else if (commonType == AstCommonTokenType.NOT) {
      left = not(left, leftOperatorPrecedence);
    } else {
      throw new FarserException("Expression malformed on token " + currentToken);
    }
    return left;
  }
  
  private <X> Expression<C, X> handleAtomToken() {
    Expression<C, X> left;
    if (getNextCommonTokenFlag() != AstCommonTokenType.LPAREN) {
      // Atom without args
      left = uncheckedCast(nodeSupplier.createNode(currentToken));
      this.eat(CommonTokenType.ATOM); // Move iterator if 'ATOM'
    } else {
      left = createNodeWithArgs();
    }
    return left;
  }

  private CommonTokenFlag getCommonTokenFlag() {
    return currentToken.getType().getCommonTokenType().orElse(null);
  }

  private CommonTokenFlag getNextCommonTokenFlag() {
    L next = tokens.peek();
    if (next != null) {
      return next.getCommonType().orElse(null);
    }
    return null;
  }

  /**
   * Verifies that a token is what is expected. If not, an exception is thrown.
   *
   * @param expected The expected token.
   */
  private void verifyFunctionTokenValidity(AstCommonTokenType expected) {
    if (getCommonTokenFlag() != expected) {
      throw new FarserException("Function expression malformed on token '" + currentToken
              + "'. Expected '" + expected + "'");
    }
  }

  /**
   * Creates a node with arguments.
   *
   * @return The created terminal node.
   */
  private <X> Expression<C, X> createNodeWithArgs() {
    final L functionToken = currentToken; // save the function name
    this.eat();
    verifyFunctionTokenValidity(AstCommonTokenType.LPAREN);
    this.eat();

    CommonTokenFlag commonType = getCommonTokenFlag();
    List<L> args = new ArrayList<>();

    while (!this.tokens.isEmpty()
            && commonType != AstCommonTokenType.RPAREN
            && commonType != AstCommonTokenType.LPAREN) {
      if (commonType != AstCommonTokenType.FUNCTION_ARGS_SEPARATOR) {
        args.add(currentToken);
      }
      this.eat();
      commonType = getCommonTokenFlag();
    }
    verifyFunctionTokenValidity(AstCommonTokenType.RPAREN);
    this.eat();

    return uncheckedCast(nodeSupplier.createNode(functionToken, args));
  }

  /**
   * Move the iterator forward if the current token matches the one passed in.
   *
   * @param type the type of the token to eat.
   */
  private void eat(CommonTokenFlag type) {
    // TODO determine if token type checking is needed. Why only advance when token type matches?
    // The 'eat' call seems to always get called from within an if/while anyways where the type is
    // already known. Except for one single case 'eat(CommonTokenType.RPAREN)' where the RPAREN is
    // assumed.
    if (currentToken.getType().isEqual(type) && this.tokens.peek() != null) {
      currentToken = this.tokens.pop();
    }
  }

  /**
   * Move the iterator forward.
   */
  private void eat() {
    if (this.tokens.peek() != null) {
      currentToken = this.tokens.pop();
    }
  }

  /**
   * Casts the current token type to {@link AstTokenType}.<br>
   * Also checks the token type when casting to {@link AstTokenType} to ensure it implements that
   * type. Informs the user if implementation is wrong.
   *
   * @return The cast token type of the current token
   */
  private AstTokenType<?> getCurrentTokenAstType() {
    TokenType<?> type = currentToken.getType();
    if (!(type instanceof AstTokenType)) {
      throw new FarserException("The token type "
          + type.getClass()
          + " does not implement "
          + AstTokenType.class.getName());
    }
    return (AstTokenType<?>) type;
  }


  /**
   * Gets the {@link AstTokenType#getOperatorPrecedence()} by casting the {@link TokenType} to
   * {@link AstTokenType}.<br>
   * No type checking is done to avoid unnecessary overhead. This call always follows an
   * {@link #getCurrentTokenAstType()} which does type checking.
   *
   * @return The operator precedence of the current token
   */
  private int getCurrentOperatorPrecedence() {
    TokenType<?> type = currentToken.getType();
    return ((AstTokenType<?>) type).getOperatorPrecedence();
  }

  /**
   * A helper method to do an unchecked cast and suppress the warning. Only for situations where
   * programmatic generic type checking is not possible and we have to rely on runtime (class cast)
   * exceptions.<br>
   * This avoids creating a temporary intermediate variable to attach the
   * <code>@SuppressWarnings("unchecked")</code> to.
   *
   * @param <I>         The input data type
   * @param <O>         The type to cast to
   * @param inputObject The object to cast
   * @return The cast object
   */
  @SuppressWarnings("unchecked")
  private static <I, O> O uncheckedCast(I inputObject) {
    return (O) inputObject;
  }

}
