package com.mmm.his.cer.utility.farser.ast;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AstTest.MaskedContext;
import com.mmm.his.cer.utility.farser.ast.AstTest.StringOperandSupplier;
import com.mmm.his.cer.utility.farser.ast.AstTest.TestContext;
import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.Test;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class PrintingTest {

  @Test
  public void testPrintTree() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    String printed = ast.printTree();
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "OR",
        "  AND",
        "    OR",
        "      AND",
        "        A",
        "        B",
        "      C",
        "    D",
        "  AND",
        "    E",
        "    F"
    }));

  }

  @Test
  public void testPrintTreeEvaluated() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();
    List<String> mask = Arrays.asList("A", "C");

    String printed = ast.printTree(new TestContext<>(mask));
    String[] lines = printed.split(System.lineSeparator());

    assertThat(lines, is(new String[] {
        "OR = false",
        "  AND = false",
        "    OR = true",
        "      AND = false",
        "        A = true",
        "        B = false",
        "      C = true",
        "    D = false",
        "  AND = false",
        "    E = false",
        "    F = false"
    }));

  }

}
