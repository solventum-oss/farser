package com.mmm.his.cer.utility.farser.ast_set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.node.supplier.SetLogicNodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import com.mmm.his.cer.utility.farser.lexer.set.SetFormulaTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.set.SetLogicToken;
import com.mmm.his.cer.utility.farser.lexer.set.SetLogicTokenType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * @author Mike Funaro
 */
public class SetAstTest {

  TestContext context = new TestContext();
  SetLogicNodeSupplier nodeSupplier = new SetLogicNodeSupplier();
  SetFormulaTokenFactory factory = new SetFormulaTokenFactory();

  @Test
  public void differenceSimple() {
    String input = "A - B";

    // Lex the tokens
    List<SetLogicToken> tokens = Lexer.lex(SetLogicTokenType.class, input, factory);

    // Build tree
    AstDescentParser<SetLogicToken, SetLogicTokenType, LookupContext<String>, List<String>> ast =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

    ExpressionResult<LookupContext<String>, List<String>> result = ast.buildTree()
        .evaluateExpression(context);

    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "C")));
  }

  @Test
  public void intersectionSimple() {
    String input = "A & B";

    // Lex the tokens
    List<SetLogicToken> tokens = Lexer.lex(SetLogicTokenType.class, input, factory);

    // Build tree
    AstDescentParser<SetLogicToken, SetLogicTokenType, LookupContext<String>, List<String>> parser =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

    AbstractSyntaxTree<LookupContext<String>, List<String>> ast = parser.buildTree();

    ExpressionResult<LookupContext<String>, List<String>> result = ast.evaluateExpression(context);

    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("B")));
  }

  @Test
  public void unionSimple() {
    String input = "A | B";

    // Lex the tokens
    List<SetLogicToken> tokens = Lexer.lex(SetLogicTokenType.class, input, factory);

    // Build tree
    AstDescentParser<SetLogicToken, SetLogicTokenType, LookupContext<String>, List<String>> parser =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

    AbstractSyntaxTree<LookupContext<String>, List<String>> ast = parser.buildTree();

    ExpressionResult<LookupContext<String>, List<String>> result = ast.evaluateExpression(context);

    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "B", "C", "D", "E")));
  }

  //
  //  @Test
  //  public void testUnion() {
  //    List<SetLexerToken> lexerTokens = SetFormulaLexer.lex("A | B");
  //    SetDescentParser<TestContext, String> parser = new SetDescentParser<>(
  //        lexerTokens.listIterator(), defaultSupplier);
  //
  //    SetSyntaxTree<TestContext, String> ast = parser.buildExpressionTree();
  //    SetExpressionResult<TestContext, String> evaluation = ast.evaluateExpression(
  //        new TestContext());
  //    TestContext context = evaluation.getContext();
  //    assertThat(context.getResultData().toArray(),
  //        is(Matchers.arrayContaining("A", "B", "C", "D", "E")));
  //
  //  }
  //
  //  @Test
  //  public void testComplexMinus() {
  //    List<SetLexerToken> lexerTokens = SetFormulaLexer.lex("A - (B - C)");
  //    SetDescentParser<TestContext, String> parser = new SetDescentParser<>(
  //        lexerTokens.listIterator(), defaultSupplier);
  //
  //    SetSyntaxTree<TestContext, String> ast = parser.buildExpressionTree();
  //    SetExpressionResult<TestContext, String> evaluation = ast.evaluateExpression(
  //        new TestContext());
  //    TestContext context = evaluation.getContext();
  //    assertThat(context.getResultData().toArray(),
  //        is(Matchers.arrayContaining("A", "B", "C")));
  //
  //  }
  //
  //  @Test
  //  public void testComplexMinusThenUnion() {
  //    List<SetLexerToken> lexerTokens = SetFormulaLexer.lex("A | (B - C)");
  //    SetDescentParser<TestContext, String> parser = new SetDescentParser<>(
  //        lexerTokens.listIterator(), defaultSupplier);
  //
  //    SetSyntaxTree<TestContext, String> ast = parser.buildExpressionTree();
  //    SetExpressionResult<TestContext, String> evaluation = ast.evaluateExpression(
  //        new TestContext());
  //    TestContext context = evaluation.getContext();
  //    assertThat(context.getResultData().toArray(),
  //        is(Matchers.arrayContaining("A", "B", "C", "E")));
  //
  //  }
  //
  //  @Test
  //  public void testComplexIntersectionThenUnion() {
  //    List<SetLexerToken> lexerTokens = SetFormulaLexer.lex("A | (B & C)");
  //    SetDescentParser<TestContext, String> parser = new SetDescentParser<>(
  //        lexerTokens.listIterator(), defaultSupplier);
  //
  //    SetSyntaxTree<TestContext, String> ast = parser.buildExpressionTree();
  //    SetExpressionResult<TestContext, String> evaluation = ast.evaluateExpression(
  //        new TestContext());
  //    TestContext context = evaluation.getContext();
  //    assertThat(context.getResultData().toArray(),
  //        is(Matchers.arrayContaining("A", "B", "C", "D")));
  //
  //  }
  //
  //  @Test
  //  public void testComplexUnionThenIntersection() {
  //    List<SetLexerToken> lexerTokens = SetFormulaLexer.lex("A & (B | C)");
  //    SetDescentParser<TestContext, String> parser = new SetDescentParser<>(
  //        lexerTokens.listIterator(), defaultSupplier);
  //
  //    SetSyntaxTree<TestContext, String> ast = parser.buildExpressionTree();
  //    SetExpressionResult<TestContext, String> evaluation = ast.evaluateExpression(
  //        new TestContext());
  //    TestContext context = evaluation.getContext();
  //    assertThat(context.getResultData().toArray(),
  //        is(Matchers.arrayContaining("B")));
  //  }
  //
  //  @Test
  //  public void testComplexLeftSideUnionThenIntersection() {
  //    List<SetLexerToken> lexerTokens = SetFormulaLexer.lex("(A & B) | C)");
  //    SetDescentParser<TestContext, String> parser = new SetDescentParser<>(
  //        lexerTokens.listIterator(), defaultSupplier);
  //
  //    SetSyntaxTree<TestContext, String> ast = parser.buildExpressionTree();
  //    SetExpressionResult<TestContext, String> evaluation = ast.evaluateExpression(
  //        new TestContext());
  //
  //    TestContext context = evaluation.getContext();
  //    assertThat(context.getResultData().toArray(),
  //        is(Matchers.arrayContaining("B", "D", "F")));
  //  }

  public class TestContext implements LookupContext<String> {

    Map<String, List<String>> runtimeData = new HashMap<>();
    List<String> resultData = new ArrayList<>();

    public TestContext() {
      runtimeData.put("A", Arrays.asList("A", "B", "C"));
      runtimeData.put("B", Arrays.asList("B", "D", "E"));
      runtimeData.put("C", Arrays.asList("F", "B", "D"));
    }

    @Override
    public List<String> lookupData(String key) {
      return runtimeData.get(key);
    }

  }
}
