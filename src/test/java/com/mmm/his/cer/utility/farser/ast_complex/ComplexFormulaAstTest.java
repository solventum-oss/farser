package com.mmm.his.cer.utility.farser.ast_complex;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter;
import com.mmm.his.cer.utility.farser.ast.PrintingTest;
import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ComplexTestTokenType;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstNodeSupplier;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import java.util.List;
import org.junit.Test;

public class ComplexFormulaAstTest {

  private static final ComplexTestTokenFactory factory = new ComplexTestTokenFactory();
  private static final NodeSupplier<ComplexTestToken, ComplexTestAstContext> defaultNodeSupplier =
      new ComplexTestAstNodeSupplier();


  @Test
  public void simpleTwoExpressionsGreaterThanAnd() throws Exception {
    String input = "A > 5 & B = 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

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
  public void simpleTwoExpressionsGreaterLessThanAndOr() throws Exception {
    String input = "A = 1 & B > 5 | C < 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

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
  public void simpleTwoExpressionsGreaterLessThanAndOr_andOrOrderSwapped() throws Exception {
    String input = "A = 1 | B > 5 & C < 2";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

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
  public void testMultipleOperatorsWithSamePrecedence() throws Exception {

    // Do two greather-than signs following each other make sense? Not necessarily - different
    // programming languages seem to handle such a case differently (some allow it and it means "3 >
    // 2 && 2 > 1", others fail to compile). In any case, the purpose of this is to ensure that
    // operators with same precedence still evaluate the left-side part first, then the right-side
    // part.
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "3 > 2 > 1", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "GREATER-THAN",
        "  GREATER-THAN",
        "    3",
        "    2",
        "  1"
    }));

  }

  @Test
  public void testMultipleOperatorsWithSamePrecedencePlusBoolean() throws Exception {

    // Two ">" operators which have the same precedence, connected with "AND". Should properly
    // build the AST left-to-right.
    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "3 > 2 & 2 > 1", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(
            lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

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
  public void testSimplestFormula_reversedOperandPrecedence() throws Exception {

    // OR is first in the formula, but should have a "weaker bond" than the AND
    List<ComplexTestToken> lexerTokens = Lexer.lex(ComplexTestTokenType.class, "A | B & C", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast, PrintingTest::printNode);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "01OR",
        "02  A",
        "02  AND",
        "03    B",
        "03    C"
    }));

  }

  @Test
  public void testSimplestFormula_leftToRightOperandPrecedence() throws Exception {

    // The "stronger" AND operand appears first, then the "weaker" OR operand
    List<ComplexTestToken> lexerTokens = Lexer.lex(ComplexTestTokenType.class, "A & B | C", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast, PrintingTest::printNode);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "01OR",
        "02  AND",
        "03    A",
        "03    B",
        "02  C"
    }));

  }

  @Test
  public void testPrintTreeWithPeek() throws Exception {

    List<ComplexTestToken> lexerTokens =
        Lexer.lex(ComplexTestTokenType.class, "(A & B | C) & D | (E & F)", factory);
    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser = new AstDescentParser<>(
        lexerTokens.listIterator(), defaultNodeSupplier);

    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast, PrintingTest::printNodeWithPeek);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "01OR next=AND",
        "02  AND next=OR",
        "03    OR next=AND",
        "04      AND next=A",
        "05        A next=B",
        "05        B next=C",
        "04      C next=D",
        "03    D next=AND",
        "02  AND next=E",
        "03    E next=F",
        "03    F next=NONE",
        "03    ", // Including the "closing" structure of each node
        "02  ",
        "01"
    }));

  }

  @Test
  public void twoWordTokenWithOneWordAmbiguity() throws Exception {
    // Token "IN TABLE" could also get recognized as "IN", but it should get resolved as "IN TABLE"
    String input = "a IN TABLE abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

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
  public void singleWordTokenWithTwoWordAmbiguity() throws Exception {
    // Token "IN" also exists as "IN TABLE", but it should get resolved as "IN"
    String input = "a IN abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstContext> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "IN",
        "  a",
        "  abc"
    }));

  }

}
