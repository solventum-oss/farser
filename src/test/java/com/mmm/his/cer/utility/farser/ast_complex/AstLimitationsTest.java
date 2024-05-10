package com.mmm.his.cer.utility.farser.ast_complex;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertThrows;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter;
import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ComplexTestTokenType;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstNodeSupplier;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import java.util.List;
import org.junit.Test;


/**
 * The tests in here test the limitations of the AST - situations where the current implementation
 * falls down just because it is not (yet) made for these scenarios.
 *
 * @author Thomas Naeff
 *
 */
public class AstLimitationsTest {

  private static final ComplexTestTokenFactory factory = new ComplexTestTokenFactory();
  private static final NodeSupplier<ComplexTestToken, ComplexTestAstContext> defaultNodeSupplier =
      new ComplexTestAstNodeSupplier();


  @Test
  public void testTwoOperandsNextToEachOther() throws Exception {
    // A formula for example to count based on a value and a lookup table.
    // Since 'IN TABLE' is an operator and also '>' is an operator, AST building fails.
    String input = "a IN TABLE > 5";
    List<ComplexTestToken> tokens = Lexer.lex(ComplexTestTokenType.class, input, factory);

    AstDescentParser<ComplexTestToken, ComplexTestTokenType, ComplexTestAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), defaultNodeSupplier);

    FarserException exc = assertThrows(FarserException.class, () -> parser.buildTree());
    assertThat(exc.getMessage(), is("Expression malformed on token GT:>"));

  }

  @Test
  public void doesNotProperlyWork_OperatorWithParameters() throws Exception {
    // In this formula, the parenthesis are not recognised as operator parameter content.
    String input = "a IN(1, 2, 3)";
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
        "  1," // ... stuff is missing here
    }));

  }

}
