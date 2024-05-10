package com.mmm.his.cer.utility.farser.ast;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter.AstPrintDirection;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter.AstPrinterContext;
import com.mmm.his.cer.utility.farser.ast.AstTest.StringOperandSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.ast.setup.MaskedContext;
import com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import org.junit.Test;

/**
 *
 *
 * @author Thomas Naeff
 *
 */
public class PrintingJsonTest {

  @Test
  public void testPrintTreeAsJson() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F & G)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    NodePrinterJson nodePrinter = new NodePrinterJson();

    String printed = AbstractSyntaxTreePrinter.printTree(ast, nodePrinter::printNode);
    printed = "{" + System.lineSeparator() + printed + System.lineSeparator() + "}";

    String[] lines = printed.split(System.lineSeparator());

    String[] expected = new String[] {
        "{",
        "\"OR\": {",
        "  \"AND\": {",
        "    \"OR\": {",
        "      \"AND\": [\"A\", \"B\"      ], ",
        "      \"1\": \"C\"",
        "    }, ",
        "    \"2\": \"D\"",
        "  }, ",
        "  \"AND\": {",
        "    \"AND\": [\"E\", \"F\"    ], ",
        "    \"3\": \"G\"",
        "    }",
        "  }",
        "",
        "}"
    };
    // System.out.println("=== expected ===");
    // System.out.println(Arrays.stream(expected).collect(Collectors.joining(System.lineSeparator())));
    // System.out.println("=== actual ===");
    // System.out.println(printed);
    assertThat(lines, is(expected));

  }


  /********************************************************************************************************
   * A very rudimentary implementation to print the formula as JSON. This was just implemented to
   * get it to work and see if it can be possible. A lot in this code is not very nice :)
   *
   * @author Thomas Naeff
   *
   * @param <T>
   */
  private static class NodePrinterJson {

    private LinkedList<String> closingBrackets = new LinkedList<>();
    private int generatedKey = 1;

    private NodePrinterJson() {}

    public String printNode(Expression<?, ?> node, AstPrinterContext<?> printerContext) {
      StringBuilder sb = new StringBuilder();

      if ((printerContext.direction == AstPrintDirection.UP) && !closingBrackets.isEmpty()) {
        // sb.append(String.format("%02d", printerContext.depth));
        sb.append(printerContext.prefix);
        String bracket = closingBrackets.removeLast();
        sb.append(bracket);
        // Do not add a comma when printing is past the last node
        if (node != null) {
          sb.append(", ");
        }
        sb.append(System.lineSeparator());
      }

      if (node != null) {
        // sb.append(String.format("%02d", printerContext.depth));
        if ((node instanceof BooleanNonTerminal) && (printerContext.next instanceof BooleanNonTerminal)) {
          sb.append(printerContext.prefix);
          // sb.append(node.getClass().getSimpleName());
          sb.append("\"");
          sb.append(node.print());
          sb.append("\": {");
          closingBrackets.add("}");
          sb.append(System.lineSeparator());
        } else if ((node instanceof BooleanNonTerminal)
            && (printerContext.next instanceof BooleanExpression)) {
          sb.append(printerContext.prefix);
          // sb.append(node.getClass().getSimpleName());
          sb.append("\"");
          sb.append(node.print());
          sb.append("\": [");
          closingBrackets.add("]");
        } else if ((node instanceof BooleanExpression)
            && (printerContext.next instanceof BooleanExpression)) {
          if (printerContext.direction == AstPrintDirection.UP) {
            sb.append(printerContext.prefix);
          }
          // sb.append(node.getClass().getSimpleName());

          // If the element is in an object, add some generated key to be valid JSON
          if (closingBrackets.getLast().contains("}")) {
            sb.append("\"");
            sb.append(generatedKey);
            sb.append("\": ");
            generatedKey += 1;
          }

          sb.append("\"");
          sb.append(node.print());
          sb.append("\"");
          if ((printerContext.direction != AstPrintDirection.UP)
              && (printerContext.nextDirection != AstPrintDirection.UP)) {
            sb.append(", ");
          }
          if (printerContext.direction == AstPrintDirection.UP) {
            sb.append(System.lineSeparator());
          }
        } else {
          sb.append(printerContext.prefix);
          // sb.append(node.getClass().getSimpleName());

          // If the element is in an object, add some generated key to be valid JSON
          if (closingBrackets.getLast().contains("}")) {
            sb.append("\"");
            sb.append(generatedKey);
            sb.append("\": ");
            generatedKey += 1;
          }

          sb.append("\"");
          sb.append(node.print());
          sb.append("\"");
          sb.append(System.lineSeparator());
        }
      }

      return sb.toString();
    }
  }

}
