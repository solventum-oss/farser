package com.mmm.his.cer.utility.farser.lexer;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * The interface to use for an enumeration which defines tokens that are recognized for this
 * specific type of code.<br />
 * <br />
 * Example:
 *
 * <pre>
 * public enum DrgFormulaToken implements
 *     TokenType&lt;DrgFormulaToken&gt; {
 *
 * ...
 *
 * }
 * </pre>
 *
 *
 * @param <T> The enumeration type which implements this interface.
 * @author Mike Funaro
 */
public interface TokenType<T extends Enum<T>> {

  /**
   * The name of the enumeration element.
   *
   * @return The enumeration name
   */
  String name();

  /**
   * The token value (e.g. the formula operator).<br />
   * Returns an empty optional for the {@link CommonTokenType#ATOM} token with the value.
   *
   * @return The token value.
   */
  Optional<String> getValue();

  /**
   * The marker for {@link CommonTokenType}s. Not all tokens need such a common token type.
   *
   * @return The common token type, or an empty optional if there is none.
   */
  Optional<CommonTokenFlag> getCommonTokenType();

  /**
   * Retrieves the {@link CommonTokenFlag} but fails with an exception if no such flag is available.
   *
   * @return The flag
   */
  default CommonTokenFlag getCommonTokenTypeOrThrow() {
    return getCommonTokenType().orElseThrow(
        () -> new UnsupportedOperationException(
            CommonTokenType.class.getSimpleName() + " required"));
  }

  /**
   * A simple equals-check against a {@link CommonTokenFlag} (if there is one set for this token).
   *
   * @param commonTokenType The common token type to check against. May be <code>null</code>.
   * @return Whether or not the common types are equal
   */
  default boolean isEqual(CommonTokenFlag commonTokenType) {
    return getCommonTokenType().orElse(null) == commonTokenType;
  }

  /**
   * Retrieves the token which is marked with the given {@link CommonTokenType}.
   *
   * @param tokenTypeClass The enumeration class with the tokens
   * @param commonType     The {@link CommonTokenType} to look for
   * @return The token
   * @throws IllegalArgumentException If the token type class is not an enumeration
   */
  static <T extends TokenType<?>> Optional<T> getForCommonType(Class<T> tokenTypeClass,
      CommonTokenType commonType) {

    // Build lookup map or retrieve cached lookup map
    Map<CommonTokenType, T> lookup = TokenTypeLookup.getCommonTypeLookupMap(tokenTypeClass);

    if (!lookup.containsKey(commonType)) {
      return Optional.empty();
    } else {
      return Optional.ofNullable(lookup.get(commonType));
    }
  }

  /**
   * Retrieves the token which is marked with the given {@link CommonTokenType}. Throws an exception
   * if no such entry with that common token type exists.
   *
   * @param tokenTypeClass The enumeration class with the tokens
   * @param commonType     The {@link CommonTokenType} to look for
   * @return The token
   * @throws IllegalArgumentException If the token type class is not an enumeration, or if the token
   *                                  type class does not contain the provided common token type
   *                                  entry.
   */
  static <T extends TokenType<?>> T getForCommonTypeMandatory(Class<T> tokenTypeClass,
      CommonTokenType commonType) {
    Optional<T> tokenTypeTmp = getForCommonType(tokenTypeClass, commonType);
    if (!tokenTypeTmp.isPresent()) {
      // Having the provided token type is mandatory
      throw new IllegalArgumentException("No implementation with "
          + commonType.getClass().getSimpleName()
          + "."
          + commonType.name()
          + " found in "
          + tokenTypeClass.getSimpleName());
    }
    return tokenTypeTmp.get();
  }

  /**
   * Retrieves the token which has the given value.
   *
   * @param tokenTypeClass The enumeration class with the tokens
   * @param value          The value to look for
   * @return The token
   * @throws IllegalArgumentException If the token type class is not an enumeration
   */
  static <T extends TokenType<?>> Optional<T> getForValue(Class<T> tokenTypeClass,
      String value) {

    // Build lookup map or retrieve cached lookup map
    Map<String, T> lookup = TokenTypeLookup.getValueLookupMap(tokenTypeClass);

    if (!lookup.containsKey(value)) {
      return Optional.empty();
    } else {
      return Optional.ofNullable(lookup.get(value));
    }
  }

  /**
   * Retrieves all token values from the enumeration class.
   *
   * @param tokenTypeClass The enumeration class
   * @return All tokens
   * @throws IllegalArgumentException If the token type class is not an enumeration
   */
  static <T extends TokenType<?>> T[] values(Class<T> tokenTypeClass) {

    if (!tokenTypeClass.isEnum()) {
      throw new IllegalArgumentException(tokenTypeClass.getName()
          + " has to be an enumeration");
    }

    return tokenTypeClass.getEnumConstants();
  }


  /**
   * Creates a RegEx OR pattern ("A|B|C...") based on all the token values in the provided
   * {@link TokenType} class.<br />
   * Tokens with no value are ignored.
   *
   * @param enumClass The token type enumeration class
   * @return The RegEx pattern
   * @throws IllegalArgumentException If the token type class is not an enumeration
   */
  static Pattern createTokenPattern(Class<? extends TokenType<?>> enumClass) {
    // Build pattern or retrieve cached pattern
    return TokenTypeLookup.getPattern(enumClass);
  }


}
