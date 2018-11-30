package com.mmm.his.cer.utility.farser.lexer;

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

  /**
   * Given a String and an Index, get the content of the string starting from that index. Stop at
   * the next known {@link TokenType} value.
   * 
   * @param str {@link String} from which to extract the substring.
   * @param index the index at which we will start the substring process.
   * @return the value of the substring
   */
  private static String getAtom(String str, int index) {
    int charIndex = index;
    for (; charIndex < str.length(); charIndex++) {
      char charAtIndex = str.charAt(charIndex);

      if (TokenType.TOKEN_TYPE_VALUES.contains(charAtIndex)) {
        // As soon as a token type is found, return what we have read so far.
        // Anything that is not a token type is an atom.
        return str.substring(index, charIndex);
      }
    }

    // Return all of it
    return str.substring(index, charIndex);
  }

  /**
   * Method to perform our Lexical analysis.
   * 
   * @param input {@link String} to separate out into tokens.
   * @return List of {@link LexerToken} that were created from the input string.
   */
  public static List<LexerToken> lex(String input) {
    List<LexerToken> result = new ArrayList<>();

    for (int i = 0; i < input.length(); i++) {

      char charAtIndex = input.charAt(i);
      // Skip all whitespaces in between tokens. The tokens itself (mainly the ATOMs) can contain
      // whitespaces but they are trimmed.
      if (!Character.isWhitespace(input.charAt(i))) {
        Optional<TokenType> tokenType = TokenType.getForValue(charAtIndex);

        if (tokenType.isPresent()) {
          result.add(new LexerToken(tokenType.get()));
        } else {
          // Read anything that is not a token type as atom
          String atom = getAtom(input, i);
          // Advance by the atom length (one character less because the loop increases the index)
          i += atom.length() - 1;
          Optional<String> value = parseValue(atom);
          if (value.isPresent()) {
            Optional<String> prefix = parsePrefix(atom);
            result.add(new LexerToken(TokenType.ATOM, value.get(), prefix));
          }
        }

      }
    }

    return result;

  }

  /**
   * Given an string, figure out and return the prefix.
   * 
   * @param atom {@link String} to analyze for prefix
   * @return Optional String value for the prefix
   */
  private static Optional<String> parsePrefix(String atom) {
    if (!atom.contains(LexerToken.PREFIX_SEPARATOR_STRING)) {
      return Optional.empty();
    }

    Optional<String> prefix = LexerToken.PREFIX_SEPARATOR_PATTERN.splitAsStream(atom).findFirst();
    if (prefix.isPresent()) {
      return Optional.of(prefix.get().trim());
    } else {
      return prefix;
    }
  }

  /**
   * Parse the value from a given atom. Ignores any prefix (separated by
   * {@link LexerToken#PREFIX_SEPARATOR_CHAR}) if present.
   * 
   * @param atom {@link String} to get the value from
   * @return Optional String value
   */
  private static Optional<String> parseValue(String atom) {
    if (!atom.contains(LexerToken.PREFIX_SEPARATOR_STRING)) {
      return Optional.of(atom.trim());
    }

    String withoutPrefix = LexerToken.PREFIX_SEPARATOR_PATTERN.splitAsStream(atom)
        .reduce((arg1, arg2) -> arg2).orElse(null);
    if (withoutPrefix != null) {
      withoutPrefix = withoutPrefix.trim();
    }
    return Optional.ofNullable(withoutPrefix);
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
