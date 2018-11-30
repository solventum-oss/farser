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
    List<LexerToken> lex = DrgFormulaLexer.lex("larynx |(~PDX:dxlarx & ~otlarynx)");

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.LPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.RPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.RPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testLexFormula2() throws Exception {
    // Testing some weird spacing
    List<LexerToken> lex = DrgFormulaLexer.lex(" larynx|(~ PDX : dxlarx&~otlarynx ) ");

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.LPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.NOT));
    assertThat(lex.get(index).getValue(), is(TokenType.NOT.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.RPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.RPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaWithMethod() throws Exception {
    List<LexerToken> lex = DrgFormulaLexer.lex("A | B & someFunction ( 1, 2,3) & C");

    int index = 0;

    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("A"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.OR));
    assertThat(lex.get(index).getValue(), is(TokenType.OR.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("B"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("someFunction"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.LPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.LPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.COMMA));
    assertThat(lex.get(index).getValue(), is(TokenType.COMMA.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.COMMA));
    assertThat(lex.get(index).getValue(), is(TokenType.COMMA.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("3"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.RPAREN));
    assertThat(lex.get(index).getValue(), is(TokenType.RPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.AND));
    assertThat(lex.get(index).getValue(), is(TokenType.AND.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(TokenType.ATOM));
    assertThat(lex.get(index).getValue(), is("C"));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testGetValues1() throws Exception {
    List<LexerToken> lex = DrgFormulaLexer.lex("larynx |(PDX:dxlarx & ~otlarynx)");
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};
    List<String> listNames = DrgFormulaLexer.getValues(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }


  @Test
  public void testGetValues2() throws Exception {
    List<LexerToken> lex = DrgFormulaLexer.lex(" larynx|( PDX : dxlarx&~otlarynx ) ");
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};
    List<String> listNames = DrgFormulaLexer.getValues(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }


}
