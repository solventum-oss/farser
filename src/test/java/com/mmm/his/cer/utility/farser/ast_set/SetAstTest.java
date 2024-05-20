package com.mmm.his.cer.utility.farser.ast_set;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTree;
import com.mmm.his.cer.utility.farser.ast.AbstractSyntaxTreePrinter;
import com.mmm.his.cer.utility.farser.ast.node.supplier.SetTheoryNodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser;
import com.mmm.his.cer.utility.farser.ast.parser.ExpressionResult;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import com.mmm.his.cer.utility.farser.lexer.set.SetFormulaTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.set.SetTheoryToken;
import com.mmm.his.cer.utility.farser.lexer.set.SetTheoryTokenType;
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
  SetTheoryNodeSupplier<String> nodeSupplier = new SetTheoryNodeSupplier<>();
  SetFormulaTokenFactory factory = new SetFormulaTokenFactory();

  @Test
  public void differenceSimple() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A - B");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "C")));
  }

  @Test
  public void intersectionSimple() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A & B");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("B")));
  }

  @Test
  public void unionSimple() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A | B");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "B", "C", "D", "E")));
  }

  @Test
  public void complexMinus() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A - (B - C)");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "B", "C")));
  }

  @Test
  public void complexMinusThenUnion() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A | (B - C)");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "B", "C", "E")));
  }

  @Test
  public void complexIntersectionThenUnion() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A | (B & C)");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("A", "B", "C", "D")));
  }

  @Test
  public void complexUnionThenIntersection() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("A & (B | C)");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("B")));
  }

  @Test
  public void complexLeftSideUnionThenIntersection() {
    ExpressionResult<LookupContext<String>, List<String>> result = evaluate("(A & B) | C)");
    assertThat(result.getResult().toArray(), is(Matchers.arrayContaining("B", "D", "F")));
  }

  /**
   * Helper method to lex, build tree from, and evaluate the formula.
   *
   * @param formula the formula to use.
   * @return the ExpressionResult.
   */
  private ExpressionResult<LookupContext<String>, List<String>> evaluate(String formula) {
    // Lex the tokens
    List<SetTheoryToken> tokens = Lexer.lex(SetTheoryTokenType.class, formula, factory);

    // Build tree
    AstDescentParser<SetTheoryToken, SetTheoryTokenType, LookupContext<String>, List<String>> parser =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

    AbstractSyntaxTree<LookupContext<String>, List<String>> ast = parser.buildTree();
    System.out.println(AbstractSyntaxTreePrinter.printTree(ast));
    return ast.evaluateExpression(context);
  }

  /**
   * Context class to use in this testing. It contains fake runtime data to be used.
   */
  private static class TestContext implements LookupContext<String> {

    Map<String, List<String>> runtimeData = new HashMap<>();

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
