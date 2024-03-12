package com.mmm.his.cer.utility.farser.ast;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIterableContainingInOrder.contains;
import static org.junit.Assert.assertThrows;

import com.mmm.his.cer.utility.farser.ast.AstTest.StringOperandSupplier;
import com.mmm.his.cer.utility.farser.ast.node.LtrExpressionIterator;
import com.mmm.his.cer.utility.farser.ast.node.type.BooleanExpression;
import com.mmm.his.cer.utility.farser.ast.parser.DescentParser;
import com.mmm.his.cer.utility.farser.ast.setup.MaskedContext;
import com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.junit.Test;

public class LtrExpressionIteratorTest {

  @Test
  public void testFullTreeIteration() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    // System.out.println(lexerTokens);
    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    LtrExpressionIterator<MaskedContext<String>> iter = ast.iterator();

    assertThat(iter.hasNext(), is(true));
    assertThat(iter.getCurrentDepth(), is(0));

    // This printed order may look not right - see 'LtrExpressionIterator' javadoc about iteration
    // order and notation.
    assertThat(iter.next().print(), is("OR"));
    assertThat(iter.getCurrentDepth(), is(1));

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(2));

    assertThat(iter.next().print(), is("OR"));
    assertThat(iter.getCurrentDepth(), is(3));

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(4));

    assertThat(iter.next().print(), is("A"));
    assertThat(iter.getCurrentDepth(), is(5));

    assertThat(iter.next().print(), is("B"));
    assertThat(iter.getCurrentDepth(), is(5));

    assertThat(iter.next().print(), is("C"));
    assertThat(iter.getCurrentDepth(), is(4));

    assertThat(iter.next().print(), is("D"));
    assertThat(iter.getCurrentDepth(), is(3));

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(2));

    assertThat(iter.next().print(), is("E"));
    assertThat(iter.getCurrentDepth(), is(3));

    assertThat(iter.next().print(), is("F"));
    assertThat(iter.getCurrentDepth(), is(3));

    assertThrows(NoSuchElementException.class, iter::next);
    assertThat(iter.hasNext(), is(false));

  }

  @Test
  public void testFullTreeIterationWithPeek() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("(A & B | C) & D | (E & F)");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    // System.out.println(lexerTokens);
    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    LtrExpressionIterator<MaskedContext<String>> iter = ast.iterator();

    assertThat(iter.hasNext(), is(true));
    assertThat(iter.getCurrentDepth(), is(0));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.peek().print(), is("OR"));
    assertThat(iter.getCurrentDepth(), is(0));
    assertThat(iter.getPeekedDepth(), is(1));

    // Peek again, nothing should change
    assertThat(iter.peek().print(), is("OR"));
    assertThat(iter.getCurrentDepth(), is(0));
    assertThat(iter.getPeekedDepth(), is(1));
    // --

    // This printed order may look not right - see 'LtrExpressionIterator' javadoc about iteration
    // order and notation.
    assertThat(iter.next().print(), is("OR"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(1));
    assertThat(iter.peek().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(1));
    assertThat(iter.getPeekedDepth(), is(2));

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(2));
    assertThat(iter.peek().print(), is("OR"));
    assertThat(iter.getCurrentDepth(), is(2));
    assertThat(iter.getPeekedDepth(), is(3));

    assertThat(iter.next().print(), is("OR"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.peek().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.getPeekedDepth(), is(4));

    // -- Peek again, nothing should change
    assertThat(iter.peek().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.getPeekedDepth(), is(4));
    // --

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(4));
    assertThat(iter.peek().print(), is("A"));
    assertThat(iter.getCurrentDepth(), is(4));
    assertThat(iter.getPeekedDepth(), is(5));

    assertThat(iter.next().print(), is("A"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(5));
    assertThat(iter.peek().print(), is("B"));
    assertThat(iter.getCurrentDepth(), is(5));
    assertThat(iter.getPeekedDepth(), is(5));

    assertThat(iter.next().print(), is("B"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(5));
    assertThat(iter.peek().print(), is("C"));
    assertThat(iter.getCurrentDepth(), is(5));
    assertThat(iter.getPeekedDepth(), is(4));

    assertThat(iter.next().print(), is("C"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(4));
    assertThat(iter.peek().print(), is("D"));
    assertThat(iter.getCurrentDepth(), is(4));
    assertThat(iter.getPeekedDepth(), is(3));

    assertThat(iter.next().print(), is("D"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.peek().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.getPeekedDepth(), is(2));

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(2));
    assertThat(iter.peek().print(), is("E"));
    assertThat(iter.getCurrentDepth(), is(2));
    assertThat(iter.getPeekedDepth(), is(3));

    assertThat(iter.next().print(), is("E"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.peek().print(), is("F"));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.getPeekedDepth(), is(3));

    assertThat(iter.next().print(), is("F"));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));
    assertThat(iter.getCurrentDepth(), is(3));
    assertThrows(NoSuchElementException.class, iter::peek);
    assertThat(iter.getCurrentDepth(), is(3));
    assertThat(iter.getPeekedDepth(), is(LtrExpressionIterator.PEEKED_DEPTH_NONE));

    assertThrows(NoSuchElementException.class, iter::next);
    assertThat(iter.hasNext(), is(false));

  }

  @Test
  public void testFullTreeIterationToString() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A & B | C");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    // System.out.println(lexerTokens);
    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    Iterator<BooleanExpression<MaskedContext<String>>> iter = ast.iterator();

    List<String> printed = new ArrayList<String>();
    while (iter.hasNext()) {
      String print = iter.next().print();
      printed.add(print);
    }

    // left-to-right/"polish notation" order
    // OR
    // |-AND
    // |..|-A
    // |..\-B
    // \-C
    assertThat(printed, contains("OR", "AND", "A", "B", "C"));

  }

  @Test
  public void testSingleAtomIterator() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    // System.out.println(lexerTokens);
    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    LtrExpressionIterator<MaskedContext<String>> iter = ast.iterator();

    assertThat(iter.hasNext(), is(true));
    assertThat(iter.getCurrentDepth(), is(0));

    assertThat(iter.next().print(), is("A"));
    assertThat(iter.getCurrentDepth(), is(1));

    assertThrows(NoSuchElementException.class, iter::next);
    assertThat(iter.hasNext(), is(false));

  }

  @Test
  public void testSingleOperandIterator() throws Exception {

    List<DrgLexerToken> lexerTokens = DrgFormulaLexer.lex("A & B");
    DescentParser<MaskedContext<String>> parser = new DescentParser<>(lexerTokens.listIterator(),
        new StringOperandSupplier(), Collections.emptyMap());

    // System.out.println(lexerTokens);
    AbstractSyntaxTree<MaskedContext<String>> ast = parser.buildTree();

    LtrExpressionIterator<MaskedContext<String>> iter = ast.iterator();

    assertThat(iter.hasNext(), is(true));
    assertThat(iter.getCurrentDepth(), is(0));

    assertThat(iter.next().print(), is("AND"));
    assertThat(iter.getCurrentDepth(), is(1));

    assertThat(iter.next().print(), is("A"));
    assertThat(iter.getCurrentDepth(), is(2));

    assertThat(iter.next().print(), is("B"));
    assertThat(iter.getCurrentDepth(), is(2));

    assertThrows(NoSuchElementException.class, iter::next);
    assertThat(iter.hasNext(), is(false));

  }

}
