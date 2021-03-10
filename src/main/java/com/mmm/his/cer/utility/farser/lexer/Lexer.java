package com.mmm.his.cer.utility.farser.lexer;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Parses a code string into {@link LexerToken}s.
 *
 * @author Mike Funaro
 */
public class Lexer {

  private Lexer() {
    // Hide constructor. Only static methods in here.
  }

  /**
   * Method to perform our Lexical analysis.
   *
   * @param tokenTypeEnumClass The enumeration class which defines all the tokens
   * @param input              {@link String} to separate out into tokens.
   * @param factory            The factory that creates the {@link LexerToken}s
   * @return List of {@link LexerToken} that were created from the input string.
   */
  public static <L extends LexerToken<T>, T extends TokenType<?>> List<L>
      lex(Class<T> tokenTypeEnumClass, String input, LexerTokenFactory<L, T> factory) {
    List<L> result = new ArrayList<>();
    Pattern delimiterPattern = TokenType.createTokenPattern(tokenTypeEnumClass);
    Matcher delimiterMatcher = delimiterPattern.matcher(input);

    // Handle optionals of the mandatory token types.
    // These token types being mandatory is already checked when it creates the token lookup in
    // TokenTypeLookup. However, that can not just be guaranteed here since that logic happens
    // somewhere completely different.
    T atomTokenType = TokenType.getForCommonTypeMandatory(tokenTypeEnumClass, CommonTokenType.ATOM);
    T spaceTokenType =
        TokenType.getForCommonTypeMandatory(tokenTypeEnumClass, CommonTokenType.SPACE);
    Optional<T> spaceTokenTypeTmp = Optional.of(spaceTokenType);

    int pos = 0;
    while (delimiterMatcher.find()) {
      // If there's something between the current and the previous delimiter
      // Add it to the tokens list.
      if (pos != delimiterMatcher.start()) {
        // Anything in between tokens is an ATOM
        String atom = input.substring(pos, delimiterMatcher.start()).trim();
        // Ignore all-space atom strings. No use for those.
        if (!atom.isEmpty()) {
          L atomToken = factory.create(atomTokenType, atom);
          if (atomToken != null) {
            result.add(atomToken);
          }
        }
      }

      String delimiter = delimiterMatcher.group();
      Optional<T> delimiterTokenType;

      if (delimiter.trim().isEmpty()) {
        // Handle special SPACE token
        delimiterTokenType = spaceTokenTypeTmp;
      } else {
        delimiterTokenType = TokenType.getForValue(tokenTypeEnumClass, delimiter);
      }

      if (delimiterTokenType.isPresent()) {
        L atomToken = factory.create(delimiterTokenType.get(), delimiter);
        if (atomToken != null) {
          result.add(atomToken);
        }
      } else {
        // This should never happen. The regex should hit all tokens which exist in the token type
        // enum.
        throw new FarserException("No match found for delimiter '"
            + delimiter
            + "'. No such token type seems to exist in "
            + tokenTypeEnumClass.getSimpleName());
      }

      // Remember end of delimiter
      pos = delimiterMatcher.end();
    }

    // If there are some characters remaining in the string after the last delimiter
    // it has to be an atom (all tokens have been hit before).
    if (pos != input.length()) {
      String atom = input.substring(pos).trim();
      // Ignore all-space atom strings. No use for those.
      if (!atom.isEmpty()) {
        L atomToken = factory.create(atomTokenType, atom);
        if (atomToken != null) {
          result.add(atomToken);
        }
      }
    }

    return result;
  }


  /**
   * Get only the values from a list of Tokens.
   *
   * @param tokens the List of tokens to filter
   * @return List of strings that only contain values
   */
  public static <L extends LexerToken<T>, T extends TokenType<?>> List<String>
      getTokens(List<L> tokens, T forTokenType) {
    return tokens.stream().filter(token -> token.getType() == forTokenType)
        .map(LexerToken::getValue).collect(Collectors.toList());
  }

}
