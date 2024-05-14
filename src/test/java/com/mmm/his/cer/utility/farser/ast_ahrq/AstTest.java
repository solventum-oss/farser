package com.mmm.his.cer.utility.farser.ast_ahrq;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.DrgSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.node.nonterminal.NonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.EvalResult;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.ahrq.Or;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.terminal.MatchingNode;
import com.mmm.his.cer.utility.farser.ast.node.type.AhrqContext;
import com.mmm.his.cer.utility.farser.ast.node.type.Attribute;
import com.mmm.his.cer.utility.farser.ast.node.type.AttributeType;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import com.mmm.his.cer.utility.farser.ast.node.type.MaskedContext;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.ast.setup.TestContext;
import com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer;
import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import org.hamcrest.Matchers;
import org.junit.Test;

/**
 * @author Mike Funaro
 */
public class AstTest {
  AhrqNodeSupplier nodeSupplier = new AhrqNodeSupplier();
  DrgFormulaTokenFactory factory = new DrgFormulaTokenFactory();

  @Test
  public void testTrueEval() {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(BILATERAL) & (D|E)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), suppliers);

    DrgSyntaxTree<MaskedContext<String>> ast = parser.buildExpressionTree();

    List<String> mask = Collections.singletonList("E");
    ExpressionResult<MaskedContext<String>, Boolean> evaluation = ast.evaluateExpression(
        new TestContext<>(mask));
    MaskedContext<String> context = evaluation.getContext();

    assertThat(evaluation.getResult(), is(true));
    assertThat(context.getMatches().toArray(),
        Matchers.arrayContaining(new Object[]{"luck", "E"}));
  }

  /**
   * Helper method to lex, build tree from, and evaluate the formula.
   *
   * @param formula the formula to use.
   * @return the ExpressionResult.
   */
  private ExpressionResult<LookupContext<String>, List<String>> evaluate(String formula) {
    // Lex the tokens
    List<DrgLexerToken> tokens = Lexer.lex(DrgFormulaToken.class, formula, factory);

    // Build tree
    AstDescentParser<DrgLexerToken, DrgFormulaToken, AhrqContext, EvalResult<String>> parser =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

    AbstractSyntaxTree<MaskedContext<String>, EvalResult<String>> ast = parser.buildTree();

    Attribute attributeA = new Attribute(AttributeType.DX, "A");
    Attribute attributeB = new Attribute(AttributeType.DX, "B");
    Attribute attributeC = new Attribute(AttributeType.DX, "C");
    Attribute attributeD = new Attribute(AttributeType.DX, "D");
    return ast.evaluateExpression(new  AhrqContext(new HashSet<>(Arrays.asList(attributeA, attributeB, attributeC, attributeD))));
  }

  public class AhrqNodeSupplier implements NodeSupplier<DrgLexerToken, AhrqContext> {


    @Override
    public Expression<AhrqContext, ?> createNode(DrgLexerToken token) {
      return new MatchingNode<>(token.value);
    }

    @Override
    public NonTerminal<MaskedContext<String>, ?> createNonTerminalNode(DrgLexerToken token) {
      switch (token.type) {
        case AND:
          return new And<>();
        case OR:
          return new Or<>();
        case NOT:
          return new Not<>();
        default:
          throw new FarserException("Operator type " + token + "not implemented");
      }
    }
  }

}
