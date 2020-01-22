package com.mmm.his.cer.utility.farser.ast;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import org.hamcrest.Matchers;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * @author Mike Funaro
 */
public class AstTest {

  @Test
  public void testTrueEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A|B) & (D|E)");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();

    List<String> mask = Arrays.asList("E", "A");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testFalseEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A|B) & (D|E)");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    ExpressionResult<String> evaluation = ast.evaluateExpression(Collections.singletonList("A"));

    assertThat(evaluation.isMatched(), is(false));
  }

  @Test
  public void testComplexTrueEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | ((B & C) & (D | E | (F & G)))");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Arrays.asList("B", "C", "F", "G");
    ExpressionResult<String> evaluation = ast
        .evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testComplexFalseEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | ((B & C) & (D | E | (F & G)))");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    ExpressionResult<String> evaluation = ast.evaluateExpression(Arrays.asList("B", "C", "G"));

    assertThat(evaluation.isMatched(), is(false));
  }

  @Test
  public void testLeftSideComplexLeftSideEval() {
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("((B & C) & (D | E | (F & G))) | A");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Arrays.asList("G", "F", "C", "B");
    ExpressionResult<String> evaluation = ast
        .evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testLeftSideComplexRightSideEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("((B & C) & (D | E | (F & G))) | A");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Collections.singletonList("A");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testWeirdTrueEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A | B| C| (D & (G & (F|H)))))");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Arrays.asList("D", "G", "H");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testWeirdFalseEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A | B| C| (D & (G & (F|H)))))");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    ExpressionResult<String> evaluation = ast.evaluateExpression(Arrays.asList("D", "H"));

    assertThat(evaluation.isMatched(), is(false));
  }

  @Test
  public void testNegation() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | ~B");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Collections.singletonList("A");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testNegationPresent() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | ~B");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Collections.singletonList("B");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(false));
  }

  @Test
  @Ignore("Not sure this is a valid test")
  public void testNegationOtherThanTrue() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | ~B");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Collections.singletonList("G");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testCustomObjectOperandTrueEval() {
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | C");
    DescentParser<CustomTestOperand> parser = new DescentParser<>(lexerTokens.listIterator(),
        drgLexerToken -> new CustomTestOperand(drgLexerToken.getValue(),
            drgLexerToken.getPrefix().orElse("NOT_NEEDED")));

    DrgSyntaxTree<CustomTestOperand> ast = parser.buildExpressionTree();
    List<CustomTestOperand> mask = Collections
        .singletonList(new CustomTestOperand("A", "NOT_NEEDED"));
    ExpressionResult<CustomTestOperand> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }

  @Test
  public void testCustomObjectOperandFalseEval() {
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("PDX:A | C");
    DescentParser<CustomTestOperand> parser = new DescentParser<>(lexerTokens.listIterator(),
        drgLexerToken -> new CustomTestOperand(drgLexerToken.getValue(),
            drgLexerToken.getPrefix().orElse("NOT_NEEDED")));

    DrgSyntaxTree<CustomTestOperand> ast = parser.buildExpressionTree();
    ExpressionResult<CustomTestOperand> evaluation = ast
        .evaluateExpression(Collections.singletonList(new CustomTestOperand("A", "NOT_NEEDED")));

    assertThat(evaluation.isMatched(), is(false));
  }

  @Test
  public void testEvalOfAnotherAst() {
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | ~B");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);

    DrgSyntaxTree<String> ast = parser.buildExpressionTree();

    parser.setTokenIterator(DrgFormulaLexer.lex("A | (B & C").listIterator());

    DrgSyntaxTree<String> ast2 = parser.buildExpressionTree();

    List<String> mask = Collections.singletonList("A");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);

    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));

    List<String> mask2 = Arrays.asList("B", "C");
    ExpressionResult<String> evaluation2 = ast2.evaluateExpression(mask2);
    assertThat(evaluation2.isMatched(), is(true));
    assertThat(evaluation2.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask2.toArray()));
  }

  @Test
  public void testSingleListEval() {
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A");
    DescentParser<String> parser = new DescentParser<>(lexerTokens.listIterator(),
        DrgLexerToken::getValue);
    DrgSyntaxTree<String> ast = parser.buildExpressionTree();
    List<String> mask = Collections.singletonList("A");
    ExpressionResult<String> evaluation = ast.evaluateExpression(mask);
    assertThat(evaluation.isMatched(), is(true));
    assertThat(evaluation.getMatches().toArray(),
        Matchers.arrayContainingInAnyOrder(mask.toArray()));
  }



  /**
   * Test implementation.
   */
  private class CustomTestOperand {

    private String value;
    private String prefix;

    public CustomTestOperand(String value, String prefix) {
      this.value = value;
      this.prefix = prefix;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (!(o instanceof CustomTestOperand)) {
        return false;
      }
      CustomTestOperand that = (CustomTestOperand) o;
      return value.equals(that.value) &&
          prefix.equals(that.prefix);
    }

    @Override
    public int hashCode() {
      return Objects.hash(value, prefix);
    }
  }
}
