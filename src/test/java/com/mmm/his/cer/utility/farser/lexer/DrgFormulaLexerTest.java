package com.mmm.his.cer.utility.farser.lexer;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;

import com.mmm.his.cer.utility.farser.lexer.domain.DomainCodeLexerToken;
import com.mmm.his.cer.utility.farser.lexer.domain.DomainCodeToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import org.hamcrest.collection.IsIterableContainingInOrder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.security.InvalidParameterException;
import java.util.List;

/**
 * farser
 *
 * @author a30w4zz
 */
public class DrgFormulaLexerTest {

  @Rule
  public final ExpectedException exception = ExpectedException.none();


  @Test
  public void testLexFormula1() throws Exception {
    String input = "larynx |(~PDX:dxlarx & ~otlarynx)";
    List<DrgLexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DrgFormulaToken.ATOM));
    assertThat(lex.get(index).getValue(), is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.OR));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.LPAREN));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.LPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.NOT));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.ATOM));
    assertThat(lex.get(index).getValue(), is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.AND));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.AND.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.NOT));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.ATOM));
    assertThat(lex.get(index).getValue(), is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.RPAREN));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.RPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testLexFormula2() throws Exception {
    // Testing some weird spacing
    String input = " larynx|(~ PDX:dxlarx&~otlarynx ) ";
    List<DrgLexerToken> lex = DrgFormulaLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DrgFormulaToken.ATOM));
    assertThat(lex.get(index).getValue(), is("larynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.OR));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.OR.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.LPAREN));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.LPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.NOT));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.ATOM));
    assertThat(lex.get(index).getValue(), is("dxlarx"));
    assertThat(lex.get(index).prefix.get(), is("PDX"));

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.AND));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.AND.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.NOT));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.NOT.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.ATOM));
    assertThat(lex.get(index).getValue(), is("otlarynx"));
    assertFalse(lex.get(index).prefix.isPresent());

    index++;
    assertThat(lex.get(index).getType(), is(DrgFormulaToken.RPAREN));
    assertThat(lex.get(index).getValue(), is(DrgFormulaToken.RPAREN.getValue().get()));
    assertFalse(lex.get(index).prefix.isPresent());

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaEqual() throws Exception {
    // Equals test and variable assigning - the variable assigning does not make sense in this setup
    // but it is ok for testing. The DrgFormulaLexer does not validate, it just splits into tokens.
    String input = " a = 1 or b := 2";
    List<DomainCodeLexerToken> lex = DomainCodeLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.EQUAL));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.EQUAL.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("b"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ASSIGN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.ASSIGN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaLtGtEq() throws Exception {
    // Equals test and greather-than and less-than.
    String input = " a >= 1 or b <= 2 or c > 3 or d < 4";
    List<DomainCodeLexerToken> lex = DomainCodeLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.GT_EQUAL));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.GT_EQUAL.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("b"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.LT_EQUAL));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.LT_EQUAL.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("c"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.GREATER_THAN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.GREATER_THAN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("3"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("d"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.LESS_THAN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.LESS_THAN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("4"));

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaSingleMultiCharacterToken() throws Exception {
    // Test to ensure that a single character token is properly recognized even when the character
    // is used in a multi character token as well.
    // ">" also appears within ">=".
    String input = " a >= 1 or c > 3";
    List<DomainCodeLexerToken> lex = DomainCodeLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.GT_EQUAL));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.GT_EQUAL.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("c"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.GREATER_THAN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.GREATER_THAN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("3"));

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testNoTokenBetweenAtoms() throws Exception {
    // Test to ensure that a single character token is properly recognized even when the character
    // is used in a multi character token as well.
    // ">" also appears within ">=".
    String input = " a b";
    List<DomainCodeLexerToken> lex = DomainCodeLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("b"));

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testLexFormulaWithMethod() throws Exception {
    String input = "A or B and someFunction ( 1, 2,3) and C";
    List<DomainCodeLexerToken> lex = DomainCodeLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    int index = 0;

    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("A"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("B"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("someFunction"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.LPAREN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.LPAREN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("1"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.COMMA));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.COMMA.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.COMMA));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.COMMA.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("3"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.RPAREN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.RPAREN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("C"));

    assertThat(lex.size(), is(index + 1));

  }

  @Test
  public void testOperandAndOperatorWithVariousSpacing() throws Exception {
    // Tests that operators are recognized properly even though there is no spacing.
    // And test that multiple spaces do not interfere.
    String input = "someAtom and otherAtom or anAtomWith_oror and anAtomWith_andandand "
        + "and a   =   2 or(not operatorWithoutSpacing)and something "
        + "and not    somethingNegated and>invalidAndGreaterThanButLexingDoesNotCare a:=b";
    List<DomainCodeLexerToken> lex = DomainCodeLexer.lex(input);

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    // Rather than checking equality on lists, make sure values from Lex are what we expect

    int index = 0;

    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("someAtom"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("otherAtom"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("anAtomWith_oror"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("anAtomWith_andandand"));


    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));


    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.EQUAL));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.EQUAL.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("2"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.OR));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.OR.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.LPAREN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.LPAREN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.NOT));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.NOT.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("operatorWithoutSpacing"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.RPAREN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.RPAREN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("something"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.NOT));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.NOT.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("somethingNegated"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.AND));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.AND.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.GREATER_THAN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.GREATER_THAN.getValue().get()));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("invalidAndGreaterThanButLexingDoesNotCare"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("a"));

    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ASSIGN));
    assertThat(lex.get(index).getValue(), is(DomainCodeToken.ASSIGN.getValue().get()));


    index++;
    assertThat(lex.get(index).getType(), is(DomainCodeToken.ATOM));
    assertThat(lex.get(index).getValue(), is("b"));


    assertThat(lex.size(), is(index + 1));

  }


  @Test
  public void testGetValues1() throws Exception {
    String input = "larynx |(PDX:dxlarx & ~otlarynx)";
    List<DrgLexerToken> lex = DrgFormulaLexer.lex(input);
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    List<String> listNames = DrgFormulaLexer.getListNames(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }


  @Test
  public void testGetValues2() throws Exception {
    String input = " larynx|( PDX:dxlarx&~otlarynx ) ";
    List<DrgLexerToken> lex = DrgFormulaLexer.lex(input);
    String[] expected = new String[] {"larynx", "dxlarx", "otlarynx"};

    System.out.println("");
    System.out.println("Input: " + input);
    System.out.println("Lexed: " + lex);

    List<String> listNames = DrgFormulaLexer.getListNames(lex);
    assertThat(listNames, IsIterableContainingInOrder.contains(expected));
  }

  @Test
  public void testInvalidDrgFormulaPrefix() throws Exception {
    // Prefix has to be with no spaces in combination with value
    String input = "PDX : dxlarx";

    exception.expect(InvalidParameterException.class);
    exception.expectMessage("Invalid ATOM ':'. Only 'prefix:value' or 'value' are allowed.");
    DrgFormulaLexer.lex(input);
  }


}
