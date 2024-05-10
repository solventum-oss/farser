package com.mmm.his.cer.utility.farser.ast.parser;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.AstTokenType;
import com.mmm.his.cer.utility.farser.ast.DrgSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.NonTerminal;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Collections;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Map;

/**
 * Recursive descent parser that will build an Abstract syntax tree from a formula (list of tokens).
 *
 * @author Mike Funaro
 * @author Thomas Naeff
 *
 * @param <L> The type of the token container
 * @param <T> The type of the token type (enum)
 * @param <C> The type of the context used when evaluating the AST
 */
public class AstDescentParser<L extends LexerToken<T>, T extends TokenType<?>, C> {

  private L currentToken;
  private Iterator<L> tokenIterator;
  private final NodeSupplier<L, C> nodeSupplier;
  @Deprecated
  private final Map<String, NodeSupplier<L, C>> suppliers;

  /**
   * Ctor.
   *
   * @param tokenIterator list of tokens to parse into the Abstract syntax tree. May be
   *                      <code>null</code>.
   * @param nodeSupplier  a factory which creates nodes for the tree.
   */
  public AstDescentParser(Iterator<L> tokenIterator,
      NodeSupplier<L, C> nodeSupplier) {
    this(tokenIterator, nodeSupplier, null);
  }

  /**
   * Ctor.
   *
   * @param tokenIterator list of tokens to parse into the Abstract syntax tree. May be
   *                      <code>null</code>.
   * @param nodeSupplier  a factory which creates nodes for the tree. This supplier is used by
   *                      default when <code>suppliers</code> does not contain a node specific
   *                      supplier
   * @param suppliers     A map with node suppliers specific to certain tokens (token value as map
   *                      key). May be <code>null</code>. <code>Deprecated</code>, because
   *                      <code>nodeSupplier</code> can perform the same (return a specific node
   *                      based on the token value) plus more (work with the complete lexer token
   *                      data).
   */
  public AstDescentParser(Iterator<L> tokenIterator,
      NodeSupplier<L, C> nodeSupplier,
      @Deprecated Map<String, NodeSupplier<L, C>> suppliers) {
    setTokenIterator(tokenIterator);

    if (nodeSupplier == null) {
      throw new FarserException(
          "Please provide at least a default supplier argument to "
              + DescentParser.class.getSimpleName()
              + " constructor");
    }
    this.nodeSupplier = nodeSupplier;

    // If there is no map, instantiate new map to avoid NPEs. If nothing is in the map the
    // nodeSupplier takes over.
    this.suppliers = suppliers == null ? Collections.emptyMap() : suppliers;
  }

  /**
   * Set a new tokenIterator so that we can build another AST using the same setup parser. Uses the
   * same {@link NodeSupplier}s which were set when the {@link AstDescentParser} was created.
   */
  public void setTokenIterator(Iterator<L> tokenIterator) {
    this.tokenIterator = tokenIterator;
    // Position at first token
    this.currentToken = tokenIterator != null ? tokenIterator.next() : null;
  }

  /**
   * Build the abstract syntax tree.
   *
   * @deprecated Call {@link #buildTree()} instead for a non-DRG specific named AST class version
   *             with the exact same functionality.
   */
  @Deprecated
  public DrgSyntaxTree<C> buildExpressionTree() {
    Expression<C, Boolean> root = expression(null, AstTokenType.NOT_AN_OPERATOR);
    return new DrgSyntaxTree<>(root);
  }

  /**
   * Build the abstract syntax tree from the provided formula/tokens.
   *
   * @param tokenIterator list of tokens to parse into the Abstract syntax tree.
   *
   * @deprecated Call {@link #buildTree(ListIterator)} instead for a non-DRG specific named AST
   *             class version with the exact same functionality.
   */
  @Deprecated
  public DrgSyntaxTree<C> buildExpressionTree(ListIterator<L> tokenIterator) {
    setTokenIterator(tokenIterator);
    Expression<C, Boolean> root = expression(null, AstTokenType.NOT_AN_OPERATOR);
    return new DrgSyntaxTree<>(root);
  }

  /**
   * Build the abstract syntax tree.
   */
  public AbstractSyntaxTree<C> buildTree() {
    Expression<C, Boolean> root = expression(null, AstTokenType.NOT_AN_OPERATOR);
    return new AbstractSyntaxTree<>(root);
  }

  /**
   * Build the abstract syntax tree from the provided formula.
   *
   * @param tokenIterator list of tokens to parse into the Abstract syntax tree.
   */
  public AbstractSyntaxTree<C> buildTree(ListIterator<L> tokenIterator) {
    setTokenIterator(tokenIterator);
    Expression<C, Boolean> root = expression(null, AstTokenType.NOT_AN_OPERATOR);
    return new AbstractSyntaxTree<>(root);
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
   *         type. Or the input <code>left</code> node passed through with a matching evaluation
   *         return type.
   */
  private <X> Expression<C, X> expression(Expression<C, X> left, int leftOperatorPrecedence) {
    left = term(left, leftOperatorPrecedence);
    // Higher value means lower precedence
    while (getCurrentTokenAstType().isLowerOrSamePrecedence(leftOperatorPrecedence)) {
      NonTerminal<C, X> operator = uncheckedCast(nodeSupplier.createNonTerminalNode(currentToken));
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
   *         type. Or the input <code>left</code> node passed through with a matching evaluation
   *         return type.
   */
  private <X> Expression<C, X> term(Expression<C, X> left, int leftOperatorPrecedence) {
    left = factor(left, leftOperatorPrecedence);
    while (getCurrentTokenAstType().isHigherPrecedence(leftOperatorPrecedence)) {
      NonTerminal<C, X> operator = uncheckedCast(nodeSupplier.createNonTerminalNode(currentToken));
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
    NonTerminal<C, R> operator = uncheckedCast(nodeSupplier.createNonTerminalNode(currentToken));
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
   *         return type. Or the input <code>left</code> node passed through with a matching
   *         evaluation return type.
   */
  private <X> Expression<C, X> factor(Expression<C, X> left, int leftOperatorPrecedence) {
    TokenType<?> tokenType = currentToken.getType();
    // Get common type for generic checking.
    // Ok to return 'null', it is only used in NPE safe logic below.
    CommonTokenFlag commonType = tokenType.getCommonTokenType().orElse(null);
    if (commonType == CommonTokenType.ATOM) {
      NodeSupplier<L, C> supplier = suppliers.getOrDefault(currentToken.getValue(), nodeSupplier);
      left = uncheckedCast(supplier.createNode(currentToken));
      this.eat(CommonTokenType.ATOM); // Move iterator if 'ATOM'
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
    if (currentToken.getType().isEqual(type) && this.tokenIterator.hasNext()) {
      currentToken = this.tokenIterator.next();
    }
  }

  /**
   * Move the iterator forward.
   *
   * @param type the type of the token to eat.
   */
  private void eat() {
    if (this.tokenIterator.hasNext()) {
      currentToken = this.tokenIterator.next();
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
