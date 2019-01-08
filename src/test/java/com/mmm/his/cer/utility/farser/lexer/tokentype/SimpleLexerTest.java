package com.mmm.his.cer.utility.farser.lexer.tokentype;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

import com.mmm.his.cer.utility.farser.lexer.FarserException;
import com.mmm.his.cer.utility.farser.lexer.Lexer;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.List;

/**
 * farser
 *
 * @author a30w4zz
 */
public class SimpleLexerTest {

  @Rule
  public final ExpectedException exception = ExpectedException.none();


  @Test
  public void testSpaces() throws Exception {
    String input = "anAtom x  afterTwoSpaces";
    List<TestLexerToken> lex = Lexer.lex(TestToken.class, input, new TestTokenFactory());

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(TestToken.ATOM));
    assertThat(lex.get(index).getValue(), is("anAtom"));

    index++;
    assertThat(lex.get(index).getType(), is(TestToken.SPACE));
    assertThat(lex.get(index).getValue(), is(" "));

    index++;
    assertThat(lex.get(index).getType(), is(TestToken.SOME_TOKEN));
    assertThat(lex.get(index).getValue(), is(TestToken.SOME_TOKEN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(TestToken.SPACE));
    assertThat(lex.get(index).getValue(), is("  "));

    index++;
    assertThat(lex.get(index).getType(), is(TestToken.ATOM));
    assertThat(lex.get(index).getValue(), is("afterTwoSpaces"));

    assertThat(lex.size(), is(index + 1));

  }



  @Test
  public void testMandatoryCommonTokenTypes() throws Exception {

    exception.expect(FarserException.class);
    exception
        .expectMessage("com.mmm.his.cer.utility.farser.lexer.CommonTokenType.SPACE is mandatory. "
            + "No token found in com.mmm.his.cer.utility.farser.lexer.tokentype.TestTokenWithoutMandatorySpace which is marked with this mandatory common type.");

    // Run lexer without even providing an input or a factory. It should fail before that.
    Lexer.lex(TestTokenWithoutMandatorySpace.class, "", null);

  }



}
