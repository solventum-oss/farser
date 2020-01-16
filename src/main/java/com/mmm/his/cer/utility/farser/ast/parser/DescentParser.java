package com.mmm.his.cer.utility.farser.ast.parser;

import com.mmm.his.cer.utility.farser.ast.nodes.And;
import com.mmm.his.cer.utility.farser.ast.nodes.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.nodes.Not;
import com.mmm.his.cer.utility.farser.ast.nodes.Operand;
import com.mmm.his.cer.utility.farser.ast.nodes.Or;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;

import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.function.Function;

/**
 * Recursive descent parser that will buildExpressionTree an Abstract syntax tree from a grouper
 * formula.
 *
 * @author Mike Funaro
 */
public class DescentParser<T> {

  private BooleanExpression<T> root;
  private DrgLexerToken currentToken;
  private ListIterator<DrgLexerToken> tokenIterator;
  private Function<DrgLexerToken, T> terminalObjectSupplier;

  /**
   * Ctor.
   *
   * @param tokenIterator          list of tokens to parse into the Abstract syntax tree.
   * @param terminalObjectSupplier the object that will take the current token and return an object
   *                               of the generic T defined for this class.
   */
  public DescentParser(ListIterator<DrgLexerToken> tokenIterator,
      Function<DrgLexerToken, T> terminalObjectSupplier) {
    this.tokenIterator = tokenIterator;
    this.currentToken = tokenIterator.next();
    this.terminalObjectSupplier = terminalObjectSupplier;
  }

  /**
   * Build the abstract syntax tree.
   */
  public void buildExpressionTree() {
    expression();
  }

  /**
   * Expression method which will buildExpressionTree the OR after parsing a term.
   */
  private void expression() {
    term();
    while (currentToken.getType() == DrgFormulaToken.OR) {
      this.eat(DrgFormulaToken.OR);
      Or<T> or = new Or<>();
      or.setLeft(root);
      term();
      or.setRight(root);
      root = or;
    }
  }

  /**
   * Term method which will buildExpressionTree the AND after parsing the factors or operands.
   */
  private void term() {
    factor();
    while (currentToken.getType() == DrgFormulaToken.AND) {
      this.eat(DrgFormulaToken.AND);
      And<T> and = new And<>();
      and.setLeft(root);
      factor();
      and.setRight(root);
      root = and;
    }
  }

  /**
   * Factor out a single the operands.
   */
  private void factor() {
    if (currentToken.getType() == DrgFormulaToken.ATOM) {

      root = new Operand<>(terminalObjectSupplier.apply(currentToken));
      this.eat(DrgFormulaToken.ATOM);
    } else if (currentToken.getType() == DrgFormulaToken.LPAREN) {
      this.eat(DrgFormulaToken.LPAREN);
      this.expression();
      this.eat(DrgFormulaToken.RPAREN);
    } else if (currentToken.getType() == DrgFormulaToken.NOT) {
      this.eat(DrgFormulaToken.NOT);
      Not<T> not = new Not<>();
      factor();
      not.setChild(root);
      root = not;
    } else {
      throw new RuntimeException("Expression Malformed");
    }
  }

  /**
   * Move the iterator forward if the current token matches the one passed in.
   *
   * @param type the type of the token to eat.
   */
  private void eat(DrgFormulaToken type) {
    if (currentToken.getType() == type && this.tokenIterator.hasNext()) {
      currentToken = this.tokenIterator.next();
    }
  }

  /**
   * Public API method that should be called following building the Abstract syntax tree.
   *
   * @param operands the list of operand objects that we want to match against.
   * @return {@link ExpressionResult}
   *     ExpressionResult object which will have a the data about the outcome of the evaluation.
   */
  public ExpressionResult<T> evaluateExpression(List<T> operands) throws IllegalStateException {
    if (root == null) {
      throw new IllegalStateException("AST must be built first before it can be evaluated");
    }
    Set<T> matches = new HashSet<>();
    boolean evaluate = this.root.evaluate(operands, matches);
    return new ExpressionResult<>(evaluate, matches);
  }
}
