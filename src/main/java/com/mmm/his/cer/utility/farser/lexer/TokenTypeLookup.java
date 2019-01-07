package com.mmm.his.cer.utility.farser.lexer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.StringJoiner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A helper class which provides lookup maps for {@link TokenType}s.
 * 
 * @author a5rn0zz
 *
 */
public class TokenTypeLookup {


  /**
   * A space is always used as a token. This is needed so that it splits up ATOMs which are not
   * separated by token (e.g. "someAtom anotherAtom" should be split, like "someAtom AND
   * anotherAtom" would be split).
   */
  public static final String SPACE_AS_DUMMY_TOKEN = " ";

  private static final Map<Class<?>, Map<String, TokenType<?>>> valueLookupMap = new HashMap<>();

  private static final Map<Class<TokenType<?>>, //
      Map<CommonTokenType, TokenType<?>>> commonTypeLookupMap = new HashMap<>();

  private static final Map<Class<TokenType<?>>, Pattern> patternLookupMap = new HashMap<>();

  private static final Pattern wordCharPattern = Pattern.compile("\\w");


  private TokenTypeLookup() {
    // Constructor not needed. Only static methods.
  }

  /**
   * Creates a map with token values as keys and the tokens as values.<br />
   * This map can then be used for easier lookup of tokens via value.<br />
   * The map is only created once for the given enum class and is retrieved from a cache for
   * subsequent lookups.
   * 
   * @param enumClass The token enumeration class
   * @return An unmodifiable map with all the lookup entries
   * @throws IllegalArgumentException If the token type class is not an enumeration
   * @throws FarserException If there are duplicate keys based on the token values
   */
  protected static <T extends TokenType<?>> Map<String, T> getValueLookupMap(Class<T> enumClass) {

    if (!enumClass.isEnum()) {
      throw new IllegalArgumentException(enumClass.getName() + " has to be an enumeration");
    }

    // If lookup already exists just do the lookup.
    if (valueLookupMap.containsKey(enumClass)) {
      // Safe to suppress. Map is created in this method for the given enum class.
      @SuppressWarnings("unchecked")
      Map<String, T> lookupMap = (Map<String, T>) valueLookupMap.get(enumClass);
      return lookupMap;
    }

    // Build lookup map
    Map<String, T> lookupMap = new HashMap<>();
    for (T enumConst : enumClass.getEnumConstants()) {
      Optional<String> value = enumConst.getValue();
      if (value.isPresent()) {
        String key = value.get();
        if (lookupMap.containsKey(key)) {
          throw new FarserException(
              "Duplicate keys are not allowed. Key '" + key + "' alredy exists for "
                  + lookupMap.get(key).getClass().getName() + "." + enumConst.name());
        }
        lookupMap.put(key, enumConst);
      }
    }

    // Save to cast. Lookup will happen within this method.
    @SuppressWarnings("unchecked")
    Map<String, TokenType<?>> lookupMapTmp = (Map<String, TokenType<?>>) lookupMap;

    // Save to cast. Lookup will happen within this method.
    @SuppressWarnings("unchecked")
    Class<TokenType<?>> enumClassTmp = (Class<TokenType<?>>) enumClass;

    // Store as unmodifiable map because it should never change again.
    valueLookupMap.put(enumClassTmp, Collections.unmodifiableMap(lookupMapTmp));
    return lookupMap;
  }

  /**
   * Creates a map with {@link CommonTokenType} as keys and the tokens as values.<br />
   * This map can then be used for easier lookup of tokens via {@link CommonTokenType}.<br />
   * The map is only created once for the given enum class and is retrieved from a cache for
   * subsequent lookups.
   * 
   * @param enumClass The token enumeration class
   * @return An unmodifiable map with all the lookup entries
   * @throws IllegalArgumentException If the token type class is not an enumeration
   * @throws FarserException If there are duplicate keys based on the {@link CommonTokenType}s or if
   *         mandatory {@link CommonTokenType} do not exist.
   */
  protected static <T extends TokenType<?>> Map<CommonTokenType, T> getCommonTypeLookupMap(
      Class<T> enumClass) {

    if (!enumClass.isEnum()) {
      throw new IllegalArgumentException(enumClass.getName() + " has to be an enumeration");
    }

    // If lookup already exists just do the lookup.
    if (commonTypeLookupMap.containsKey(enumClass)) {
      // Safe to suppress. Map is created in this method for the given enum class.
      @SuppressWarnings("unchecked")
      Map<CommonTokenType, T> lookupMap =
          (Map<CommonTokenType, T>) commonTypeLookupMap.get(enumClass);
      return lookupMap;
    }

    // Build lookup map
    Map<CommonTokenType, T> lookupMap = new EnumMap<>(CommonTokenType.class);
    for (T enumConst : enumClass.getEnumConstants()) {
      Optional<CommonTokenType> commonType = enumConst.getCommonTokenType();
      if (commonType.isPresent()) {
        CommonTokenType key = commonType.get();
        if (lookupMap.containsKey(key)) {
          throw new FarserException(
              "Duplicate keys are not allowed. Key '" + key + "' alredy exists for "
                  + lookupMap.get(key).getClass().getName() + "." + enumConst.name());
        }
        lookupMap.put(key, enumConst);
      }
    }

    // Check that all mandatory common types exist
    for (CommonTokenType commonType : CommonTokenType.values()) {
      if (commonType.isMandatory() && !lookupMap.containsKey(commonType)) {
        throw new FarserException(
            commonType.getClass().getName() + " is mandatory. No token found in "
                + enumClass.getName() + " which is marked with this mandatory common type.");
      }
    }


    // Save to cast. Lookup will happen within this method.
    @SuppressWarnings("unchecked")
    Map<CommonTokenType, TokenType<?>> lookupMapTmp =
        (Map<CommonTokenType, TokenType<?>>) lookupMap;

    // Save to cast. Lookup will happen within this method.
    @SuppressWarnings("unchecked")
    Class<TokenType<?>> enumClassTmp = (Class<TokenType<?>>) enumClass;

    // Store as unmodifiable map because it should never change again.
    commonTypeLookupMap.put(enumClassTmp, Collections.unmodifiableMap(lookupMapTmp));
    return lookupMap;
  }


  /**
   * Creates a RegEx OR pattern ("A|B|C...") based on all the token values in the provided
   * {@link TokenType} class.<br />
   * Tokens with no value are ignored.<br />
   * The pattern is only created once for the given enum class and is retrieved from a cache for
   * subsequent lookups.
   * 
   * @param enumClass The token type enumeration class
   * @return The RegEx pattern
   * @throws IllegalArgumentException If the token type class is not an enumeration
   */
  public static Pattern getPattern(Class<? extends TokenType<?>> enumClass) {

    if (!enumClass.isEnum()) {
      throw new IllegalArgumentException(enumClass.getName() + " has to be an enumeration");
    }

    // If lookup already exists just do the lookup.
    if (patternLookupMap.containsKey(enumClass)) {
      return patternLookupMap.get(enumClass);
    }

    List<String> delimiters = new ArrayList<>();
    for (TokenType<?> enumConst : enumClass.getEnumConstants()) {
      Optional<CommonTokenType> commonType = enumConst.getCommonTokenType();
      // Only the ones which are not ATOMs
      if (!commonType.isPresent() || commonType.get() != CommonTokenType.ATOM) {
        Optional<String> value = enumConst.getValue();
        // Only all non-null values
        if (value.isPresent()) {
          delimiters.add(value.get());
        }
      }
    }

    // Sort by longest delimiter first, shortest last.
    Collections.sort(delimiters, (o1, o2) -> -o1.compareTo(o2));

    // Joiner for all tokens which are symbols
    StringJoiner sjSymbol = new StringJoiner("|", "(", ")");
    // Joiner for all tokens which contain characters. They need word boundaries.
    StringJoiner sjCharacters = new StringJoiner("|", "\\b(", ")\\b");
    int symbolCount = 0;
    int charCount = 0;

    for (String token : delimiters) {
      if (token.equals(SPACE_AS_DUMMY_TOKEN)) {
        // Avoid conflicts when space is used as defined token.
        // Adding this exception here was the "easy way out". If space is really needed in a token
        // enum it would have to be tracked in a lookup map like the other lookup implementations
        // here.
        throw new FarserException("A single space can not be used as token");
      }

      if (containsWordChar(token)) {
        // A token with word characters
        sjCharacters.add(Pattern.quote(token));
        charCount++;
      } else {
        // A token without word characters
        sjSymbol.add(Pattern.quote(token));
        symbolCount++;
      }
    }

    // Always use space as dummy token. It will not be used in the resulting tokens.
    sjSymbol.add(SPACE_AS_DUMMY_TOKEN);

    StringJoiner sjPattern = new StringJoiner("|");
    if (symbolCount > 0) {
      sjPattern.add(sjSymbol.toString());
    }
    if (charCount > 0) {
      sjPattern.add(sjCharacters.toString());
    }

    Pattern pattern = Pattern.compile(sjPattern.toString(), Pattern.CASE_INSENSITIVE);

    // Save to cast. Lookup will happen within this method.
    @SuppressWarnings("unchecked")
    Class<TokenType<?>> enumClassTmp = (Class<TokenType<?>>) enumClass;

    patternLookupMap.put(enumClassTmp, pattern);
    return pattern;
  }

  /**
   * Checks if the given string contains word characters.
   * 
   * @param str The string to check
   * @return Whether or not the string contains word characters
   */
  private static boolean containsWordChar(String str) {
    Matcher matcher = wordCharPattern.matcher(str);
    // Find the first occurrence of the pattern
    return matcher.find();
  }

}
