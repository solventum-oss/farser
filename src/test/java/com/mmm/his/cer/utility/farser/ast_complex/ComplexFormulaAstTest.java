package com.mmm.his.cer.utility.farser.ast_complex;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ComplexTestTokenType;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstNodeSupplier;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertThrows;
import org.junit.Test;

public class ComplexFormulaAstTest {

  private static final ComplexTestTokenFactory factory = new ComplexTestTokenFactory();
  private static final NodeSupplier<ComplexTestToken, ComplexTestAstContext> defaultNodeSupplier =
      new ComplexTestAstNodeSupplier();


  @Test
  public void printSimpleTwoExpressionsGreaterThanAnd() throws Exception {
    String input = "A > 5 & B = 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "AND",
        "  GREATER-THAN",
        "    A",
        "    5",
        "  EQUAL",
        "    B",
        "    2"
    }));

  }

  @Test
  public void printSimpleTwoExpressionsGreaterLessThanAndOr() throws Exception {
    String input = "A = 1 & B > 5 | C < 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "OR",
        "  AND",
        "    EQUAL",
        "      A",
        "      1",
        "    GREATER-THAN",
        "      B",
        "      5",
        "  LESS-THAN",
        "    C",
        "    2"
    }));

  }

  @Test
  public void printSimpleTwoExpressionsGreaterLessThanAndOr_andOrOrderSwapped() throws Exception {
    String input = "A = 1 | B > 5 & C < 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "OR",
        "  EQUAL",
        "    A",
        "    1",
        "  AND",
        "    GREATER-THAN",
        "      B",
        "      5",
        "    LESS-THAN",
        "      C",
        "      2"
    }));

  }

  @Test
  public void printMultipleOperatorsWithSamePrecedence() throws Exception {
    
    // the purpose of this is to ensure that operators with same precedence still evaluate the 
    // left-side part first, then the right-side part.
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "X & Y & Z", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "AND",
        "  AND",
        "    X",
        "    Y",
        "  Z"
    }));
  }

  @Test
  public void evaluateMultipleOperatorsWithSamePrecedenceTrue() throws Exception {
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "X & Y & Z", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    List<String> data = Arrays.asList("X", "Y", "Z");

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext(data));

    assertThat(result.getResult(), is(true));
  }

  @Test
  public void evaluateMultipleOperatorsWithSamePrecedenceFalse() throws Exception {
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "X & Y & Z", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    List<String> data = Arrays.asList("X", "Y", "W");

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext(data));

    assertThat(result.getResult(), is(false));
  }

  @Test
  public void testPrintAndEvaluateDifferentLeftAndRightTypes() throws Exception {
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "X > Y > Z", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "GREATER-THAN",
        "  GREATER-THAN",
        "    X",
        "    Y",
        "  Z"
    }));

    // While this can parse and print, evaluation fails because the return types of the children 
    // on the left and right side are different. Errors like this are only caught at runtime.
    assertThrows(ClassCastException.class, () -> ast.evaluateExpression(new ComplexTestAstContext()));
  }

  @Test
  public void testSneakyDifferentLeftAndRightTypes() throws Exception {
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "x & Y & Z", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    List<String> data = Arrays.asList("Y", "Z");

    // This fails because x returns a string, and Y and Z return a Boolean. 
    assertThrows(ClassCastException.class, () -> ast.evaluateExpression(new ComplexTestAstContext(data)));
  }

  @Test
  public void printMultipleOperatorsWithSamePrecedencePlusBoolean() throws Exception {

    // Two ">" operators which have the same precedence, connected with "AND". Should properly
    // build the AST left-to-right.
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "3 > 2 & 2 > 1", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(
            lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "AND",
        "  GREATER-THAN",
        "    3",
        "    2",
        "  GREATER-THAN",
        "    2",
        "    1",
    }));

  }

  @Test
  public void evaluateSimpleTrueResult() {
    String input = "A > 5 & B = 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    Map<String, Integer> runtimeData = new HashMap<>();
    runtimeData.put("A", 6);
    runtimeData.put("B", 2);

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext(runtimeData));

    assertThat(result.getResult(), is(true));
  }

  @Test
  public void evaluateSimpleFalseResult() {
    String input = "A > 5 & B = 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    Map<String, Integer> runtimeData = new HashMap<>();
    runtimeData.put("A", 4);
    runtimeData.put("B", 2);

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext(runtimeData));

    assertThat(result.getResult(), is(false));
  }

  @Test
  public void evaluateComplexTrueResult() {
    String input = "(A = 8 & B > 5) & (C < 2 | D = 1)";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    Map<String, Integer> runtimeData = new HashMap<>();
    runtimeData.put("A", 8);
    runtimeData.put("B", 6);
    runtimeData.put("C", 6);
    runtimeData.put("D", 1);


    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext(runtimeData));

    assertThat(result.getResult(), is(true));
  }

  @Test
  public void evaluateComplexFalseResult() {
    String input = "(A = 8 & B > 5) & (C < 2 | D = 1)";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    Map<String, Integer> runtimeData = new HashMap<>();
    runtimeData.put("A", 8);
    runtimeData.put("B", 1);
    runtimeData.put("C", 6);
    runtimeData.put("D", 1);

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext(runtimeData));

    assertThat(result.getResult(), is(false));
  }

  @Test
  public void printTwoWordTokenWithOneWordAmbiguity() throws Exception {
    // Token "IN TABLE" could also get recognized as "IN", but it should get resolved as "IN TABLE"
    String input = "a IN TABLE abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "IN-TABLE",
        "  a",
        "  abc"
    }));

  }

  @Test
  public void printSingleWordTokenWithTwoWordAmbiguity() throws Exception {
    // Token "IN" also exists as "IN TABLE", but it should get resolved as "IN"
    String input = "a IN abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "IN",
        "  a",
        "  abc"
    }));

  }

  @Test
  public void evaluateInTableTrue() throws Exception {
    // Token "IN TABLE" could also get recognized as "IN", but it should get resolved as "IN TABLE"
    String input = "a IN TABLE abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext());

    assertThat(result.getResult(), is(true));
  }

  @Test
  public void evaluateInTableFalse() throws Exception {
    // Token "IN TABLE" could also get recognized as "IN", but it should get resolved as "IN TABLE"
    String input = "d IN TABLE abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext, Boolean> ast = parser.buildTree();

    ExpressionResult<ComplexTestAstContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstContext());

    assertThat(result.getResult(), is(false));
  }

}
