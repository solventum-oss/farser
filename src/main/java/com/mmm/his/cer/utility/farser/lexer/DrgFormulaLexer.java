package com.mmm.his.cer.utility.farser.lexer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Parses DRG formulas into {@link LexerToken}s.
 *
 * @author a30w4zz
 */
public class DrgFormulaLexer {

  private static ArrayList<String> operators = new ArrayList<>();
  private static ArrayList<Character> specialCaseChars = new ArrayList<>();

  static {
    Collections.addAll(operators, "~", "&", "|", "-", "*", ":", "(", ")", "$0", "$1");
    Collections.addAll(specialCaseChars, ':', '~', '-', '*', '_');
  }

  private static Pattern prefixDelimiter = Pattern.compile(LexerToken.PREFIX_SEPARATOR);



  /**
   * Given a String and an Index, get the value of the string. Skip known operator values.
   * 
   * @param str {@link String} from which to extract the substring.
   * @param index the index at which we will start the substring process.
   * @return the value of the substring
   */
  private static String getAtom(String str, int index) {
    int charIndex = index;
    for (; charIndex < str.length();) {
      if (Character.isLetterOrDigit(str.charAt(charIndex))
          || specialCaseChars.contains(str.charAt(charIndex))) {
        charIndex++;
      } else {
        return str.substring(index, charIndex);
      }
    }
    return str.substring(index, charIndex);
  }

  /**
   * Method to perform our Lexical analysis.
   * 
   * @param input {@link String} to seperate out into tokens.
   * @return List of {@link LexerToken} that were created from the input string.
   */
  public static List<LexerToken> lex(String input) {
    List<LexerToken> result = new ArrayList<>();
    for (int i = 0; i < input.length();) {
      switch (input.charAt(i)) {
        case '(':
          result.add(new LexerToken(TokenType.LPAREN, "("));
          i++;
          break;
        case ')':
          result.add(new LexerToken(TokenType.RPAREN, ")"));
          i++;
          break;
        case '|':
          result.add(new LexerToken(TokenType.OR, "|"));
          i++;
          break;
        case '&':
          result.add(new LexerToken(TokenType.AND, "&"));
          i++;
          break;
        case '~':
          result.add(new LexerToken(TokenType.NOT, "~"));
          i++;
          break;
        default:
          if (Character.isWhitespace(input.charAt(i))) {
            i++;
          } else {
            String atom = getAtom(input, i);
            i += atom.length();
            String listName = parseListName(atom).get();
            Optional<String> prefix = parsePrefix(atom);
            result.add(new LexerToken(TokenType.ATOM, listName, prefix));
          }
          break;
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
    if (!atom.contains(":")) {
      return Optional.empty();
    }
    return prefixDelimiter.splitAsStream(atom).findFirst();
  }

  /**
   * Parse the list name from a given atom. Ignores any prefix if present
   * 
   * @param atom {@link String} to get the list name from.
   * @return Optional String value for the list name
   */
  private static Optional<String> parseListName(String atom) {
    if (!atom.contains(":")) {
      return Optional.of(atom);
    }
    return Optional
        .ofNullable(prefixDelimiter.splitAsStream(atom).reduce((arg1, arg2) -> arg2).orElse(null));
  }

  /**
   * Get only the list names from a list of Tokens, this ignores all other types of tokens aside
   * from 'ATOM'
   * 
   * @param tokens the List of tokens to filter
   * @return List of strings that only contain list names.
   */
  public static List<String> getListNames(List<LexerToken> tokens) {
    return tokens.stream().filter(token -> token.type == TokenType.ATOM).map(token -> token.value)
        .collect(Collectors.toList());
  }
}
