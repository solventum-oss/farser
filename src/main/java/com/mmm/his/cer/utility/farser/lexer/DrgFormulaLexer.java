package com.mmm.his.cer.utility.farser.lexer;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Parses DRG formulas into {@link LexerToken}s.
 *
 * @author a30w4zz
 */
public class DrgFormulaLexer {

  private DrgFormulaLexer() {
    // Hide constructor. Only static methods in here.
  }

  /**
   * Looks for a token which matches the characters starting at the current start index.
   * 
   * @param str The string which may contain tokens
   * @param tokenStartIndex The start index within the provided string where the lookup of a token
   *        should start
   * @return The token if one was found, or <code>null</code> if no token was found
   */
  private static LexerToken lookForMatchingToken(String str, int tokenStartIndex) {

    String possibleToken = "";

    // Loop over list of all tokens. The list of all tokens should be ordered in increasing token
    // value length for optimization.
    for (TokenType tokenType : TokenType.values()) {
      Optional<String> value = tokenType.getValue();
      if (!value.isPresent()) {
        // Skip NULL values (no token to compare)
        continue;
      }
      String tokenTypeValue = value.get();

      if (possibleToken.length() != tokenTypeValue.length()) {
        int tokenEndIndex = tokenStartIndex + tokenTypeValue.length();
        if (tokenEndIndex > str.length()) {
          // End reached, no token found for current span of characters.
          // Continue with next token.
          continue;
        }
        possibleToken = str.substring(tokenStartIndex, tokenEndIndex);
      }

      if (possibleToken.equals(tokenTypeValue)) {
        return new LexerToken(tokenType);
      }

    }

    // We looked through all tokens. No match found.
    return null;
  }

  /**
   * Method to perform our Lexical analysis.
   * 
   * @param input {@link String} to separate out into tokens.
   * @return List of {@link LexerToken} that were created from the input string.
   */
  public static List<LexerToken> lex(String input) {
    List<LexerToken> result = new ArrayList<>();

    StringBuilder atomSubstring = new StringBuilder();

    for (int strCharIndex = 0; strCharIndex < input.length(); strCharIndex++) {
      char charAtIndex = input.charAt(strCharIndex);

      LexerToken token = lookForMatchingToken(input, strCharIndex);
      if (token == null) {
        // No matching token at the current start index.

        // Must be a new atom or we are still within an atom.
        // Keep adding characters to atom string.
        atomSubstring.append(charAtIndex);
      } else {
        // A token has been found at the current start index.

        // If previous characters were an atom string, process them first.
        String atom = atomSubstring.toString().trim();
        if (!atom.isEmpty()) {
          result.add(buildTokenFromAtom(atom));
          atomSubstring = new StringBuilder();
        }

        result.add(token);

        // Jump ahead to continue after the token we just found.
        // One less because the for-loop increases by 1.
        strCharIndex += token.getValue().length() - 1;
      }

    }

    // If the formula ended with an atom, add it now
    String atom = atomSubstring.toString().trim();
    if (!atom.isEmpty()) {
      result.add(buildTokenFromAtom(atom));
    }

    return result;

  }

  /**
   * Splits an atom string into its prefix and value if both are present, or just creates a token
   * with the value if no prefix is present.
   * 
   * @param atom the atom string
   * @return The token with value and with or without prefix
   */
  private static LexerToken buildTokenFromAtom(String atom) {
    String[] split = LexerToken.PREFIX_SEPARATOR_PATTERN.split(atom);
    if (split.length == 2) {
      // Prefix and value
      return new LexerToken(TokenType.ATOM, split[1].trim(), Optional.of(split[0].trim()));
    } else if (split.length == 1) {
      // Only value
      return new LexerToken(TokenType.ATOM, split[0].trim());
    } else {
      throw new InvalidParameterException("Invalid " + TokenType.ATOM.name() + ". Only 'prefix"
          + LexerToken.PREFIX_SEPARATOR_STRING + "value' or 'value' are allowed.");
    }
  }

  /**
   * Get only the values from a list of Tokens, this ignores all other types of tokens aside from
   * {@link TokenType#ATOM.}
   * 
   * @param tokens the List of tokens to filter
   * @return List of strings that only contain values
   */
  public static List<String> getValues(List<LexerToken> tokens) {
    return tokens.stream().filter(token -> token.getType() == TokenType.ATOM)
        .map(token -> token.getValue()).collect(Collectors.toList());
  }
}
