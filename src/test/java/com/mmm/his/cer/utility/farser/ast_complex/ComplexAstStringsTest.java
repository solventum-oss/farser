package com.mmm.his.cer.utility.farser.ast_complex;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.strings.ComplexTestAstStringsContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.strings.ComplexTestAstStringsNodeSupplier;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ComplexTestTokenType;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import java.util.List;
import org.junit.Test;

public class ComplexAstStringsTest {

  private static final ComplexTestTokenFactory factory = new ComplexTestTokenFactory();
  private static final NodeSupplier<ComplexTestToken, ComplexTestAstStringsContext> defaultNodeSupplier =
      new ComplexTestAstStringsNodeSupplier();


  @Test
  public void printTwoWordTokenWithOneWordAmbiguity() throws Exception {
    // Token "IN TABLE" could also get recognized as "IN", but it should get resolved as "IN TABLE"
    String input = "a IN TABLE abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstStringsContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstStringsContext, Boolean> ast = parser.buildTree();

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

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstStringsContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstStringsContext, Boolean> ast = parser.buildTree();

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

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstStringsContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstStringsContext, Boolean> ast = parser.buildTree();

    ExpressionResult<ComplexTestAstStringsContext, Boolean> result = 
        ast.evaluateExpression(new ComplexTestAstStringsContext());

    assertThat(result.getResult(), is(true));
  }

  @Test
  public void evaluateInTableFalse() throws Exception {
    // Token "IN TABLE" could also get recognized as "IN", but it should get resolved as "IN TABLE"
    String input = "d IN TABLE abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstStringsContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstStringsContext, Boolean> ast = parser.buildTree();

    ExpressionResult<ComplexTestAstStringsContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstStringsContext());

    assertThat(result.getResult(), is(false));
  }

  @Test
  public void evaluateInTrue() throws Exception {
    // Token "IN" also exists as "IN TABLE", but it should get resolved as "IN"
    String input = "a IN abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstStringsContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstStringsContext, Boolean> ast = parser.buildTree();

    ExpressionResult<ComplexTestAstStringsContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstStringsContext());

    assertThat(result.getResult(), is(true));
  }

  @Test
  public void evaluateInFalse() throws Exception {
    // Token "IN" also exists as "IN TABLE", but it should get resolved as "IN"
    String input = "x IN abc";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstStringsContext, Boolean> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);
    AbstractSyntaxTree<ComplexTestAstStringsContext, Boolean> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast);
    String[] lines = printed.split(System.lineSeparator());

    ExpressionResult<ComplexTestAstStringsContext, Boolean> result =
        ast.evaluateExpression(new ComplexTestAstStringsContext());

    assertThat(result.getResult(), is(false));
  }

}
