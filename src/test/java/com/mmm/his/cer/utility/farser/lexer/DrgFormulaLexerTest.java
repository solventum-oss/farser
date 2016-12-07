package com.mmm.his.cer.utility.farser.lexer;

import org.hamcrest.collection.IsIterableContainingInOrder;
import org.junit.Test;

import java.util.List;
import java.util.Optional;

import static com.mmm.his.cer.utility.farser.lexer.DrgFormulaLexer.Type.ATOM;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.*;

/**
 * farser
 *
 * @author a30w4zz
 */
public class DrgFormulaLexerTest {
    @Test
    public void lex() throws Exception {
        List<DrgFormulaLexer.Token> lex = DrgFormulaLexer.lex("larynx |(PDX:dxlarx & ~otlarynx)");
        // Rather than checking equality on lists, make sure values from Lex are what we expect
        assertThat(lex.get(3).type, is(ATOM));
        assertThat(lex.get(5).value, is("otlarynx"));
    }

    @Test
    public void getListNames() throws Exception {
        List<DrgFormulaLexer.Token> lex = DrgFormulaLexer.lex("larynx |(PDX:dxlarx & ~otlarynx)");
        String[] expected = new String[]{"larynx", "dxlarx", "otlarynx"};
        List<String> listNames = DrgFormulaLexer.getListNames(lex);
        assertThat(listNames, IsIterableContainingInOrder.contains(expected));
    }
}