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
  public void testLexFormula() throws Exception {
    List<LexerToken> lex = DrgFormulaLexer.lex("larynx |(~PDX:dxlarx & ~otlarynx)");

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).type, is(TokenType.ATOM));
    assertThat(lex.get(index).value, is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.OR));
    assertThat(lex.get(index).value, is(TokenType.OR.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.LPAREN));
    assertThat(lex.get(index).value, is(TokenType.LPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.NOT));
    assertThat(lex.get(index).value, is(TokenType.NOT.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.ATOM));
    assertThat(lex.get(index).value, is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).type, is(TokenType.AND));
    assertThat(lex.get(index).value, is(TokenType.AND.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.NOT));
    assertThat(lex.get(index).value, is(TokenType.NOT.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.ATOM));
    assertThat(lex.get(index).value, is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).type, is(TokenType.RPAREN));
    assertThat(lex.get(index).value, is(TokenType.RPAREN.getStringValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testGetValues() throws Exception {
    List<LexerToken> lex = DrgFormulaLexer.lex("larynx |(PDX:dxlarx & ~otlarynx)");
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};
    List<String> listNames = DrgFormulaLexer.getValues(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }



}
