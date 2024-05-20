package com.mmm.his.cer.utility.farser.lexer;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
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
 * @author Thomas Naeff
 */
public class TokenTypeLookup {

  private static final Map<Class<?>, Map<String, TokenType<?>>> valueLookupMap = new HashMap<>();

  private static final Map<Class<TokenType<?>>, //
      Map<CommonTokenType, TokenType<?>>> commonTypeLookupMap = new HashMap<>();

  private static final Map<Class<TokenType<?>>, Pattern> patternLookupMap = new HashMap<>();

  private static final Pattern wordCharPattern = Pattern.compile("\\w");


  private TokenTypeLookup() {
    // Constructor not needed. Only static methods.
  }

  /**
   * Retrieves the map with token values as keys and the tokens as values.<br />
   * This map can be used for easier lookup of tokens via value.<br />
   * The map is only created once for the given enum class and is retrieved from a cache for
   * subsequent lookups.
   *
   * @param enumClass The token enumeration class
   * @return An unmodifiable map with all the lookup entries
   * @throws IllegalArgumentException If the token type class is not an enumeration
   * @throws FarserException          If there are duplicate keys based on the token values
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

    Map<String, T> lookupMap = buildValueLookupMap(enumClass);

    // Save to cast. Lookup happens above.
    @SuppressWarnings("unchecked")
    Map<String, TokenType<?>> lookupMapTmp = (Map<String, TokenType<?>>) lookupMap;

    // Save to cast. Lookup happens above.
    @SuppressWarnings("unchecked")
    Class<TokenType<?>> enumClassTmp = (Class<TokenType<?>>) enumClass;

    // Store as unmodifiable map because it should never change again.
    valueLookupMap.put(enumClassTmp, Collections.unmodifiableMap(lookupMapTmp));
    return lookupMap;
  }

  /**
   * Creates the map for token lookups via value.
   *
   * @param enumClass The {@link TokenType} enumeration class
   * @return The map with the values and tokens of the given enumeration class
   */
  private static <T extends TokenType<?>> Map<String, T> buildValueLookupMap(Class<T> enumClass) {

    // Build lookup map
    Map<String, T> lookupMap = new HashMap<>();
    for (T enumConst : enumClass.getEnumConstants()) {
      Optional<String> value = enumConst.getValue();
      if (value.isPresent()) {
        String key = value.get();
        if (lookupMap.containsKey(key)) {
          throw new FarserException(
              "Duplicate keys are not allowed. Key '"
                  + key
                  + "' alredy exists for "
                  + lookupMap.get(key).getClass().getName()
                  + "."
                  + enumConst.name());
        }
        lookupMap.put(key, enumConst);
      }
    }

    return lookupMap;
  }

  /**
   * Retrieves the map with {@link CommonTokenType} as keys and the tokens as values.<br />
   * This map can be used for easier lookup of tokens via {@link CommonTokenType}.<br />
   * The map is only created once for the given enum class and is retrieved from a cache for
   * subsequent lookups.
   *
   * @param enumClass The token enumeration class
   * @return An unmodifiable map with all the lookup entries
   * @throws IllegalArgumentException If the token type class is not an enumeration
   * @throws FarserException          If there are duplicate keys based on the
   *                                  {@link CommonTokenType}s or if mandatory
   *                                  {@link CommonTokenType} do not exist.
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
    Map<CommonTokenType, T> lookupMap = buildCommonTypeLookupMap(enumClass);

    // Check that all mandatory common types are used
    validateCommonTypeLookupMap(enumClass, lookupMap);

    // Save to cast. Lookup happens above.
    @SuppressWarnings("unchecked")
    Map<CommonTokenType, TokenType<?>> lookupMapTmp =
        (Map<CommonTokenType, TokenType<?>>) lookupMap;

    // Save to cast. Lookup happens above.
    @SuppressWarnings("unchecked")
    Class<TokenType<?>> enumClassTmp = (Class<TokenType<?>>) enumClass;

    // Store as unmodifiable map because it should never change again.
    commonTypeLookupMap.put(enumClassTmp, Collections.unmodifiableMap(lookupMapTmp));
    return lookupMap;
  }

  /**
   * Creates the map for token lookups via {@link CommonTokenType}.
   *
   * @param enumClass The {@link TokenType} enumeration class
   * @return The map with the values and tokens of the given enumeration class
   */
  private static <T extends TokenType<?>> Map<CommonTokenType, T> buildCommonTypeLookupMap(
      Class<T> enumClass) {
    // Build lookup map
    Map<CommonTokenType, T> lookupMap = new EnumMap<>(CommonTokenType.class);
    for (T enumConst : enumClass.getEnumConstants()) {
      Optional<CommonTokenFlag> commonType = enumConst.getCommonTokenType();
      if (commonType.isPresent() && commonType.get() instanceof CommonTokenType) {
        CommonTokenType key = (CommonTokenType) commonType.get();
        if (lookupMap.containsKey(key)) {
          throw new FarserException(
              "Duplicate keys are not allowed. Key '"
                  + key
                  + "' alredy exists for "
                  + lookupMap.get(key).getClass().getName()
                  + "."
                  + enumConst.name());
        }
        lookupMap.put(key, enumConst);
      }
    }

    return lookupMap;
  }


  /**
   * Creates the map for token lookups via {@link CommonTokenType}.
   *
   * @param enumClass The {@link TokenType} enumeration class for which the <code>lookupMap</code>
   *                  has been built
   * @param lookupMap The lookup map to validate
   * @throws FarserException If the enum class or lookup map have invalid content like mandatory
   *                         common token types missing etc.
   */
  private static <T extends TokenType<?>> void validateCommonTypeLookupMap(Class<T> enumClass,
      Map<CommonTokenType, T> lookupMap) {

    // Check that all mandatory common types exist
    for (CommonTokenType commonType : CommonTokenType.values()) {
      if (commonType.isMandatory() && !lookupMap.containsKey(commonType)) {
        throw new FarserException(commonType.getClass().getName()
            + "."
            + commonType.name()
            + " is mandatory. No token found in "
            + enumClass.getName()
            + " which is marked with this mandatory common type.");
      }
    }

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

    Pattern pattern = buildPattern(enumClass);

    // Save to cast. Lookup happens above.
    @SuppressWarnings("unchecked")
    Class<TokenType<?>> enumClassTmp = (Class<TokenType<?>>) enumClass;

    patternLookupMap.put(enumClassTmp, pattern);
    return pattern;
  }

  /**
   * Builds the pattern based off the given {@link TokenType} enumeration class.<br />
   * The pattern is build based on all token values in the enumeration excluding ones marked with
   * {@link CommonTokenType#ATOM} and {@link CommonTokenType#SPACE}.
   *
   * @param enumClass The enumeration class
   * @return The RegEx pattern
   */
  private static Pattern buildPattern(Class<? extends TokenType<?>> enumClass) {

    List<String> delimiters = new ArrayList<>();
    for (TokenType<?> enumConst : enumClass.getEnumConstants()) {
      Optional<CommonTokenFlag> commonType = enumConst.getCommonTokenType();
      // Only the ones which are not ATOMs or SPACE
      if (!commonType.isPresent()
          || commonType.get() != CommonTokenType.ATOM
              && commonType.get() != CommonTokenType.SPACE) {
        Optional<String> value = enumConst.getValue();
        // Only all non-null values
        value.ifPresent(delimiters::add);
      }
    }

    // Sort by longest delimiter first, shortest last.
    delimiters.sort((o1, o2) -> -o1.compareTo(o2));

    // Joiner for all tokens which are symbols
    StringJoiner sjSymbol = new StringJoiner("|", "(", ")");
    // Joiner for all tokens which contain characters. They need word boundaries.
    StringJoiner sjCharacters = new StringJoiner("|", "\\b(", ")\\b");
    int symbolCount = 0;
    int charCount = 0;

    // Handle CommonTokenType.SPACE
    // Set this as very first symbol token element.
    sjSymbol.add(CommonTokenType.SPACE_PATTERN);
    symbolCount++;

    for (String token : delimiters) {
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

    StringJoiner sjPattern = new StringJoiner("|");
    if (symbolCount > 0) {
      sjPattern.add(sjSymbol.toString());
    }
    if (charCount > 0) {
      sjPattern.add(sjCharacters.toString());
    }

    return Pattern.compile(sjPattern.toString(), Pattern.CASE_INSENSITIVE);
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
