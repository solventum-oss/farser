package com.mmm.his.cer.utility.farser.ast;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AstTest.MaskedContext;
import com.mmm.his.cer.utility.farser.ast.AstTest.StringOperandSupplier;
import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Collections;
import java.util.List;
import org.junit.Test;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class OperandOrderTest {



  @Test
  public void testOperandOrder1_noParenthesis() throws Exception {

    String formula = "A & B | C";
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex(formula);
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    System.out.println(formula);
    System.out.println(printed);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "OR",
        "  AND",
        "    A",
        "    B",
        "  C"
    }));

  }

  @Test
  public void testOperandOrder1_withParenthesisForAnd() throws Exception {

    String formula = "(A & B) | C";
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex(formula);
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    System.out.println(formula);
    System.out.println(printed);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "OR",
        "  AND",
        "    A",
        "    B",
        "  C"
    }));

  }

  @Test
  public void testOperandOrder1_withParenthesisForOr() throws Exception {

    String formula = "A & (B | C)";
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex(formula);
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    System.out.println(formula);
    System.out.println(printed);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "AND",
        "  A",
        "  OR",
        "    B",
        "    C"
    }));

  }

  @Test
  public void testOperandOrder2_noParenthesis() throws Exception {

    String formula = "C | A & B";
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex(formula);
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    System.out.println(formula);
    System.out.println(printed);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "OR",
        "  C",
        "  AND",
        "    A",
        "    B"
    }));

  }

  @Test
  public void testOperandOrder2_withParenthesisForAnd() throws Exception {

    String formula = "C | (A & B)";
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex(formula);
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    System.out.println(formula);
    System.out.println(printed);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "OR",
        "  C",
        "  AND",
        "    A",
        "    B"
    }));

  }

  @Test
  public void testOperandOrder2_withParenthesisForOr() throws Exception {

    String formula = "(C | A) & B";
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex(formula);
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    System.out.println(formula);
    System.out.println(printed);
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "AND",
        "  OR",
        "    C",
        "    A",
        "  B"
    }));

  }

}
