package com.mmm.his.cer.utility.farser.ast.parser;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AstCommonTokenType;
import com.mmm.his.cer.utility.farser.ast.DrgSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
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
import java.util.function.Function;

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
   * @param nodeSupplier  a factory which creates nodes for the tree. As standard java functional
   *                      interface.
   */
  public AstDescentParser(Iterator<L> tokenIterator,
      Function<L, BooleanExpression<C>> nodeSupplier) {
    // Simply "wraps" the Java functional interface as 'NodeSupplier'.
    this(tokenIterator, nodeSupplier::apply, null);
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
    System.out.println("current: " + currentToken.getValue());
  }

  /**
   * Build the abstract syntax tree.
   *
   * @deprecated Call {@link #buildTree()} instead for a non-DRG specific named AST class version
   *             with the exact same functionality.
   */
  @Deprecated
  public DrgSyntaxTree<C> buildExpressionTree() {
    BooleanExpression<C> root = expression(null);
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
    BooleanExpression<C> root = expression(null);
    return new DrgSyntaxTree<>(root);
  }

  /**
   * Build the abstract syntax tree.
   */
  public AbstractSyntaxTree<C> buildTree() {
    BooleanExpression<C> root = expression(null);
    return new AbstractSyntaxTree<>(root);
  }

  /**
   * Build the abstract syntax tree from the provided formula.
   *
   * @param tokenIterator list of tokens to parse into the Abstract syntax tree.
   */
  public AbstractSyntaxTree<C> buildTree(ListIterator<L> tokenIterator) {
    setTokenIterator(tokenIterator);
    BooleanExpression<C> root = expression(null);
    return new AbstractSyntaxTree<>(root);
  }

  /**
   * Expression method which will build the OR after parsing a term.
   */
  private BooleanExpression<C> expression(BooleanExpression<C> root) {
    root = term(root);
    while (currentToken.getType().isEqual(AstCommonTokenType.LEFT)) {
      NonTerminal<C> or = nodeSupplier.createNonTerminalNode(currentToken);
      this.eat(currentToken.getType().getCommonTokenTypeOrThrow()); // Move iterator if 'OR'
      or.setLeft(root);
      root = term(root);
      or.setRight(root);
      root = or;
    }
    return root;
  }

  /**
   * Term method which will build the AND after parsing the factors or operands.
   */
  private BooleanExpression<C> term(BooleanExpression<C> root) {
    root = factor(root);
    while (currentToken.getType().isEqual(AstCommonTokenType.RIGHT)) {
      NonTerminal<C> and = nodeSupplier.createNonTerminalNode(currentToken);
      this.eat(currentToken.getType().getCommonTokenTypeOrThrow()); // Move iterator if 'AND'
      and.setLeft(root);
      root = factor(root);
      and.setRight(root);
      root = and;
    }
    return root;
  }

  /**
   * Factor out a single operand.
   */
  private BooleanExpression<C> factor(BooleanExpression<C> root) {
    TokenType<?> tokenType = currentToken.getType();
    // Get common type for generic checking. Ok to return 'null', it is only used in NPE safe logic
    // below.
    CommonTokenFlag commonType = tokenType.getCommonTokenType().orElse(null);
    if (commonType == CommonTokenType.ATOM) {
      NodeSupplier<L, C> supplier = suppliers.getOrDefault(
          currentToken.getValue(), nodeSupplier);
      root = supplier.createNode(currentToken);
      this.eat(CommonTokenType.ATOM); // Move iterator if 'ATOM'
    } else if (commonType == AstCommonTokenType.LPAREN) {
      this.eat(AstCommonTokenType.LPAREN); // Move iterator if 'LPAREN'
      root = this.expression(root);
      this.eat(AstCommonTokenType.RPAREN);
    } else if (commonType == AstCommonTokenType.NOT) {
      NonTerminal<C> not = nodeSupplier.createNonTerminalNode(currentToken);
      this.eat(AstCommonTokenType.NOT); // Move iterator if 'NOT'
      root = factor(root);
      not.setLeft(root);
      root = not;
    } else {
      throw new FarserException("Expression malformed on token " + currentToken);
    }

    return root;
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

}
