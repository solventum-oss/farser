package com.mmm.his.cer.utility.farser.ast_ahrq;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter;
import com.mmm.his.cer.utility.farser.ast.node.nonterminal.NonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.EvalResult;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.Or;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.terminal.MatchingNode;
import com.mmm.his.cer.utility.farser.ast.node.type.AhrqContext;
import com.mmm.his.cer.utility.farser.ast.node.type.Attribute;
import com.mmm.his.cer.utility.farser.ast.node.type.AttributeType;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * @author Mike Funaro
 */
public class AstTest {

  AhrqNodeSupplier nodeSupplier = new AhrqNodeSupplier();
  DrgFormulaTokenFactory factory = new DrgFormulaTokenFactory();

  @Test
  public void simpleAnd() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("A & G");
    assertThat(result.getResult().isPassed(), is(true));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{new Attribute(AttributeType.DX, "A"),
            new Attribute(AttributeType.DX, "B")}));
  }

  @Test
  public void simpleOr() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("A | B");
    assertThat(result.getResult().isPassed(), is(true));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{new Attribute(AttributeType.DX, "A"),
            new Attribute(AttributeType.DX, "B")}));
  }

  @Test
  public void simpleNot() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("~B");
    assertThat(result.getResult().isPassed(), is(false));
    assertThat(result.getResult().getResultList().isEmpty(), is(true));
  }

  @Test
  public void simpleNotWithAnd() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("A & ~B");
    assertThat(result.getResult().isPassed(), is(false));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{new Attribute(AttributeType.DX, "A")}));
  }

  @Test
  public void simpleNotWithOR() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("A | ~B");
    assertThat(result.getResult().isPassed(), is(true));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{new Attribute(AttributeType.DX, "A")}));
  }

  @Test
  public void simpleOrWithNotFirst() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("~B | A");
    assertThat(result.getResult().isPassed(), is(true));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{new Attribute(AttributeType.DX, "A")}));
  }

  @Test
  public void negatedGroup() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate("~(G | A)");
    assertThat(result.getResult().isPassed(), is(false));
    assertThat(result.getResult().getResultList().isEmpty(), is(true));
  }

  @Test
  public void complexLeftSide() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate(
        "(~((G | F) & Z) & (A & B)) & C");
    assertThat(result.getResult().isPassed(), is(true));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{new Attribute(AttributeType.DX, "A"),
            new Attribute(AttributeType.DX, "B"), new Attribute(AttributeType.DX, "C")}));
  }

  @Test
  public void complexLeftSideFail() {
    ExpressionResult<AhrqContext, EvalResult<String>> result = evaluate(
        "(~((G | F) & Z) & (X & Y)) & C");
    assertThat(result.getResult().isPassed(), is(false));
    assertThat(result.getResult().getResultList().toArray(),
        Matchers.arrayContaining(new Object[]{ new Attribute(AttributeType.DX, "C")}));
  }

  /**
   * Helper method to lex, build tree from, and evaluate the formula.
   *
   * @param formula the formula to use.
   * @return the ExpressionResult.
   */
  private ExpressionResult<AhrqContext, EvalResult<String>> evaluate(String formula) {
    // Lex the tokens
    List<DrgLexerToken> tokens = Lexer.lex(DrgFormulaToken.class, formula, factory);

    // Build tree
    AstDescentParser<DrgLexerToken, DrgFormulaToken, AhrqContext, EvalResult<String>> parser =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

    AbstractSyntaxTree<AhrqContext, EvalResult<String>> ast = parser.buildTree();

    Attribute attributeA = new Attribute(AttributeType.DX, "A");
    Attribute attributeB = new Attribute(AttributeType.DX, "B");
    Attribute attributeC = new Attribute(AttributeType.DX, "C");
    Attribute attributeD = new Attribute(AttributeType.DX, "D");
    return ast.evaluateExpression(new AhrqContext(
        new HashSet<>(Arrays.asList(attributeA, attributeB, attributeC, attributeD))));
  }

  public static class AhrqNodeSupplier implements NodeSupplier<DrgLexerToken, AhrqContext> {


    @Override
    public Expression<AhrqContext, ?> createNode(DrgLexerToken token) {
      return new MatchingNode<>(new Attribute(AttributeType.DX, token.value));
    }

    @Override
    public NonTerminal<AhrqContext, ?> createNonTerminalNode(DrgLexerToken token) {
      switch (token.type) {
        case AND:
          return new And<>();
        case OR:
          return new Or<>();
        case NOT:
          return new Not<>();
        default:
          throw new FarserException("Operator type " + token + "not implemented");
      }
    }
  }

}
