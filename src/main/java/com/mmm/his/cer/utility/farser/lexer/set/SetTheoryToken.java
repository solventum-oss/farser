package com.mmm.his.cer.utility.farser.lexer.set;

import com.mmm.his.cer.utility.farser.lexer.LexerToken;
import com.mmm.his.cer.utility.farser.lexer.TokenType;
import java.util.Optional;
import java.util.regex.Pattern;

/**
 * Set theory token implementation.
 *
 * @author Mike Funaro
 */
public class SetTheoryToken implements LexerToken<SetTheoryTokenType> {

  public static final String PREFIX_SEPARATOR_STRING = ":";
  /**
   * The regex pattern for {@link #PREFIX_SEPARATOR_STRING}.
   */
  public static final Pattern PREFIX_SEPARATOR_PATTERN =
      Pattern.compile(SetTheoryToken.PREFIX_SEPARATOR_STRING);
  public final SetTheoryTokenType type;
  public final String value;
  public final String prefix;

  /**
   * A new token with the value from the {@link TokenType}.
   *
   * @param type The token type
   */
  public SetTheoryToken(SetTheoryTokenType type) {
    this.type = type;
    this.value = type.getValue().orElse(null);
    this.prefix = null;
  }

  /**
   * A new token with a provided value.
   *
   * @param type  The token type
   * @param value The token value
   */
  public SetTheoryToken(SetTheoryTokenType type, String value) {
    this.type = type;
    this.value = value;
    this.prefix = null;
  }

  /**
   * A new token.
   *
   * @param type   The token type
   * @param value  The token value
   * @param prefix The prefix (if there is one)
   */
  public SetTheoryToken(SetTheoryTokenType type, String value, String prefix) {
    this.type = type;
    this.value = value;
    this.prefix = prefix;
  }


  @Override
  public SetTheoryTokenType getType() {
    return type;
  }

  @Override
  public String getValue() {
    return value;
  }

  public Optional<String> getPrefix() {
    return Optional.ofNullable(prefix);
  }


  @Override
  public String toString() {
    if (type == SetTheoryTokenType.ATOM) {
      String prefixString = "";
      if (prefix != null && !prefix.isEmpty()) {
        prefixString = " with prefix " + prefix;
      }
      return type.name() + "<" + value + ">" + prefixString;
    }
    return type.toString();
  }


}
