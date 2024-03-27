package com.mmm.his.cer.utility.farser.ast;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter.AstPrinterContext;
import com.mmm.his.cer.utility.farser.ast.AstTest.StringOperandSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.ast.setup.MaskedContext;
import com.mmm.his.cer.utility.farser.ast.setup.TestContext;
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

    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast, PrintingTest::printNode);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
    assertThat(lines, is(new String[] {
        "01OR",
        "02  AND",
        "03    OR",
        "04      AND",
        "05        A",
        "05        B",
        "04      C",
        "03    D",
        "02  AND",
        "03    E",
        "03    F"
    }));

  }

  @Test
  public void testSimplestFormula_reversedOperandPrecedence() throws Exception {

    // OR is first in the formula, but should have a "weaker bond" than the AND
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A | B & C");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

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
    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A & B | C");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

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

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    String printed = AbstractSyntaxTreePrinter.printTree(ast, PrintingTest::printNodeWithPeek);
    String[] lines = printed.split(System.lineSeparator());

    //System.out.println(printed);
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
  public void testPrintTreeEvaluated() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();
    List<String> mask = Arrays.asList("A", "C");

    NodePrinterWithContextData<MaskedContext<String>> nodePrinter =
        new NodePrinterWithContextData<>(new TestContext<>(mask));

    String printed = AbstractSyntaxTreePrinter.printTree(ast, nodePrinter::printNode);
    String[] lines = printed.split(System.lineSeparator());

    // System.out.println(printed);
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


  /********************************************************************************************************
   *
   *
   * @author Thomas Naeff
   *
   * @param <T>
   */
  private static class NodePrinterWithContextData<T> {

    private final T evaluationContext;

    private NodePrinterWithContextData(T printerContext) {
      this.evaluationContext = printerContext;
    }

    /**
     * Prints the output of a single node.
     *
     * @param node           The node to print
     * @param printerContext The printerContext for node evaluation. May be <code>null</code> to
     *                       skip evaluation
     * @return The node output
     */
    public String printNode(Expression<T, ?> node, AstPrinterContext<?> printerContext) {
      StringBuilder sb = new StringBuilder();
      if (node != null) {
        sb.append(printerContext.prefix);
        sb.append(node.print());
        if (evaluationContext != null) {
          Object result = node.evaluate(evaluationContext);
          sb.append(" = " + result);
        }
        sb.append(System.lineSeparator());
      }
      return sb.toString();
    }

  }


  public static String printNode(Expression<?, ?> node, AstPrinterContext<?> printerContext) {
    if (node == null) {
      return null;
    }

    StringBuilder sb = new StringBuilder();
    // Print node depth to verify it
    sb.append(String.format("%02d", printerContext.depth));
    sb.append(printerContext.prefix);
    sb.append(node.print());
    sb.append(System.lineSeparator());
    return sb.toString();
  }

  public static String printNodeWithPeek(Expression<?, ?> node,
      AstPrinterContext<?> printerContext) {
    StringBuilder sb = new StringBuilder();
    // Print depth to ensure it does not get affected by the peek
    sb.append(String.format("%02d", printerContext.depth));
    sb.append(printerContext.prefix);
    if (node != null) {
      sb.append(node.print());
      sb.append(" next=");
      sb.append(printerContext.next == null ? "NONE" : printerContext.next.print());
    }
    sb.append(System.lineSeparator());
    return sb.toString();
  }

}
