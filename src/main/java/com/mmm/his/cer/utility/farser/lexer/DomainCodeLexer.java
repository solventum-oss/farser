package com.mmm.his.cer.utility.farser.lexer;

import com.mmm.his.cer.utility.farser.lexer.domain.DomainCodeLexerToken;
import com.mmm.his.cer.utility.farser.lexer.domain.DomainCodeToken;
import com.mmm.his.cer.utility.farser.lexer.domain.DomainCodeTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;

import java.util.List;

/**
 * The lexer for 3M domain code.
 * 
 * @author a5rn0zz
 *
 */
public class DomainCodeLexer {

  private static final DomainCodeTokenFactory factory = new DomainCodeTokenFactory();

  private DomainCodeLexer() {
    // Hide constructor. Only static methods.
  }

  /**
   * Method to perform our Lexical analysis.
   * 
   * @param input {@link String} to separate out into tokens.
   * @return List of {@link DrgLexerToken} that were created from the input string.
   */
  public static List<DomainCodeLexerToken> lex(String input) {
    return Lexer.lex(DomainCodeToken.class, input, factory);
  }

  /**
   * Get only the specific tokens from a list of Tokens.
   * 
   * @param tokens the List of tokens to filter
   * @param tokenType The type of tokens to filter for
   * @return List of strings that only contain values
   */
  public static List<String> getTokens(List<DrgLexerToken> tokens, DrgFormulaToken tokenType) {
    return Lexer.getTokens(tokens, tokenType);
  }

}
