package com.mmm.his.cer.utility.farser.lexer;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;

import org.hamcrest.collection.IsIterableContainingInOrder;
import org.junit.Test;

import java.util.List;

/**
 * farser
 *
 * @author a30w4zz
 */
public class DrgFormulaLexerTest {

  @Test
  public void testLexFormula1() throws Exception {
    String input = "larynx |(~PDX:dxlarx & ~otlarynx)";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.LPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.RPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.RPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testLexFormula2() throws Exception {
    // Testing some weird spacing
    String input = " larynx|(~ PDX : dxlarx&~otlarynx ) ";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.LPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.RPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.RPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaEqual() throws Exception {
    // Equals test and variable assigning - the variable assigning does not make sense in this setup
    // but it is ok for testing. The Lexer does not validate, it just splits into tokens.
    String input = " a = 1 | b := 2";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.EQUAL));
    assertThat(lex.get(index).getValue(), is(TokenType.EQUAL.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("b"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ASSIGN));
    assertThat(lex.get(index).getValue(), is(TokenType.ASSIGN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaLtGtEq() throws Exception {
    // Equals test and greather-than and less-than
    String input = " a >= 1 | b <= 2 | c > 3 | d < 4";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.GT_EQUAL));
    assertThat(lex.get(index).getValue(), is(TokenType.GT_EQUAL.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("b"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LT_EQUAL));
    assertThat(lex.get(index).getValue(), is(TokenType.LT_EQUAL.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("c"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.GREATER_THAN));
    assertThat(lex.get(index).getValue(), is(TokenType.GREATER_THAN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("3"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("d"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LESS_THAN));
    assertThat(lex.get(index).getValue(), is(TokenType.LESS_THAN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("4"));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaWithMethod() throws Exception {
    String input = "A | B & someFunction ( 1, 2,3) & C";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("A"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("B"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("someFunction"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.LPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.COMMA));
    assertThat(lex.get(index).getValue(), is(TokenType.COMMA.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.COMMA));
    assertThat(lex.get(index).getValue(), is(TokenType.COMMA.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("3"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.RPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.RPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("C"));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testGetValues1() throws Exception {
    String input = "larynx |(PDX:dxlarx & ~otlarynx)";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    List<String> listNames = DrgFormulaLexer.getValues(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }


  @Test
  public void testGetValues2() throws Exception {
    String input = " larynx|( PDX : dxlarx&~otlarynx ) ";
    List<LexerToken> lex = DrgFormulaLexer.lex(input);
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    List<String> listNames = DrgFormulaLexer.getValues(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }


}
