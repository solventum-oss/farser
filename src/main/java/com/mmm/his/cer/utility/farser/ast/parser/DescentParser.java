package com.mmm.his.cer.utility.farser.ast.parser;

import com.mmm.his.cer.utility.farser.ast.DrgSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.node.operator.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.Or;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.lexer.CommonTokenType;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Collections;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.Map;

/**
 * Recursive descent parser that will build an Abstract syntax tree from a formula (list of formula
 * tokens).
 *
 * @author Mike Funaro
 *
 * @param <T> The type of the context used when evaluating the AST
 */
public class DescentParser<T> {

  private DrgLexerToken currentToken;
  private Iterator<DrgLexerToken> tokenIterator;
  private final NodeSupplier<DrgLexerToken, T> defaultSupplier;
  private final Map<String, NodeSupplier<DrgLexerToken, T>> suppliers;

  /**
   * Ctor.
   *
   * @param tokenIterator   list of tokens to parse into the Abstract syntax tree. May be
   *                        <code>null</code>.
   * @param defaultSupplier a factory which creates nodes for the tree. This supplier is used by
   *                        default when <code>suppliers</code> does not contain a node specific
   *                        supplier
   * @param suppliers       A map with node suppliers specific to certain tokens (token value as map
   *                        key). May be <code>null</code>.
   */
  public DescentParser(Iterator<DrgLexerToken> tokenIterator,
      NodeSupplier<DrgLexerToken, T> defaultSupplier,
      Map<String, NodeSupplier<DrgLexerToken, T>> suppliers) {
    this.tokenIterator = tokenIterator;
    this.currentToken = tokenIterator != null ? tokenIterator.next() : null;
    if (defaultSupplier == null) {
      throw new FarserException(
          "Please provide at least a default supplier argument to "
              + DescentParser.class.getSimpleName()
              + " constructor");
    }
    this.defaultSupplier = defaultSupplier;

    // If there is no map, instantiate new map to avoid NPEs. If nothing is in the map the
    // defaultSupplier takes over.
    if (suppliers == null) {
      this.suppliers = Collections.emptyMap();
    } else {
      this.suppliers = suppliers;
    }
  }

  /**
   * Set a new tokenIterator so that we can build another AST using the same setup parser. Uses the
   * same {@link NodeSupplier}s which were set when the {@link DescentParser} was created.
   */
  public void setTokenIterator(ListIterator<DrgLexerToken> tokenIterator) {
    this.tokenIterator = tokenIterator;
    this.currentToken = tokenIterator.next();
  }

  /**
   * Build the abstract syntax tree.
   */
  public DrgSyntaxTree<T> buildExpressionTree() {
    BooleanExpression<T> root = expression(null);
    return new DrgSyntaxTree<>(root);
  }

  /**
   * Build the abstract syntax tree from the provided formula/tokens.
   *
   * @param tokenIterator list of tokens to parse into the Abstract syntax tree.
   */
  public DrgSyntaxTree<T> buildExpressionTree(ListIterator<DrgLexerToken> tokenIterator) {
    setTokenIterator(tokenIterator);
    BooleanExpression<T> root = expression(null);
    return new DrgSyntaxTree<>(root);
  }

  /**
   * Expression method which will build the OR after parsing a term.
   */
  private BooleanExpression<T> expression(BooleanExpression<T> root) {
    root = term(root);
    TokenType<?> tokenType;
    while ((tokenType = currentToken.getType()) == DrgFormulaToken.OR) {
      this.eat(tokenType); // Move iterator if 'OR'
      Or<T> or = new Or<>();
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
  private BooleanExpression<T> term(BooleanExpression<T> root) {
    root = factor(root);
    TokenType<?> tokenType;
    while ((tokenType = currentToken.getType()) == DrgFormulaToken.AND) {
      this.eat(tokenType); // Move iterator if 'AND'
      And<T> and = new And<>();
      and.setLeft(root);
      root = factor(root);
      and.setRight(root);
      root = and;
    }
    return root;
  }

  /**
   * Factor out a single the operands.
   */
  private BooleanExpression<T> factor(BooleanExpression<T> root) {
    TokenType<?> tokenType = currentToken.getType();
    // Get common type for generic checking. Ok to return 'null', it is only used in NPE safe logic
    // below.
    CommonTokenType commonType = tokenType.getCommonTokenType().orElse(null);
    if (commonType == CommonTokenType.ATOM) {
      NodeSupplier<DrgLexerToken, T> nodeSupplier = suppliers.getOrDefault(
          currentToken.value, defaultSupplier);
      root = nodeSupplier.createNode(currentToken);
      this.eat(tokenType); // Move iterator if 'ATOM'
    } else if (commonType == CommonTokenType.LPAREN) {
      this.eat(tokenType); // Move iterator if 'LPAREN'
      root = this.expression(root);
      TokenType<?> rightParen =
          TokenType.getForCommonTypeMandatory(tokenType.getClass(), CommonTokenType.RPAREN);
      this.eat(rightParen); // Move iterator if 'RPAREN'
    } else if (commonType == CommonTokenType.NOT) {
      this.eat(tokenType); // Move iterator if 'NOT'
      Not<T> not = new Not<>();
      root = factor(root);
      not.setChild(root);
      root = not;
    } else {
      throw new FarserException("Expression malformed on token "
          + currentToken);
    }

    return root;
  }

  /**
   * Move the iterator forward if the current token matches the one passed in.
   *
   * @param type the type of the token to eat.
   */
  private void eat(TokenType<?> type) {
    if ((currentToken.getType() == type) && this.tokenIterator.hasNext()) {
      currentToken = this.tokenIterator.next();
    }
  }

}
