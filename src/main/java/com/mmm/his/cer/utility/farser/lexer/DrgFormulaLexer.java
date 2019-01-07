package com.mmm.his.cer.utility.farser.lexer;

import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;

import java.util.List;

/**
 * The lexer for DRG formulas.
 * 
 * @author a5rn0zz
 *
 */
public class DrgFormulaLexer {

  private static final DrgFormulaTokenFactory factory = new DrgFormulaTokenFactory();

  private DrgFormulaLexer() {
    // Hide constructor. Only static methods.
  }

  /**
   * Method to perform our Lexical analysis.
   * 
   * @param input {@link String} to separate out into tokens.
   * @return List of {@link DrgLexerToken} that were created from the input string.
   */
  public static List<DrgLexerToken> lex(String input) {
    return Lexer.lex(DrgFormulaToken.class, input, factory);
  }

  /**
   * Get only the list names from a list of Tokens, this ignores all other types of tokens aside
   * from {@link DrgFormulaToken#ATOM.}
   * 
   * @param tokens the List of tokens to filter
   * @return List of strings that only contain values
   */
  public static List<String> getListNames(List<DrgLexerToken> tokens) {
    return Lexer.getTokens(tokens, DrgFormulaToken.ATOM);
  }

}
