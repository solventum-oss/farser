package com.mmm.his.cer.utility.farser.lexer.set;

import com.mmm.his.cer.utility.farser.lexer.LexerTokenFactory;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.security.InvalidParameterException;

/**
 * The factory which creates a {@link SetTheoryToken}s.
 *
 * @author Mike Funaro
 */
public class SetFormulaTokenFactory implements
    LexerTokenFactory<SetTheoryToken, SetTheoryTokenType> {

  @Override
  public SetTheoryToken create(SetTheoryTokenType tokenType, String value) {
    if (tokenType == SetTheoryTokenType.SPACE) {
      // Ignore spaces
      return null;
    } else if (tokenType == SetTheoryTokenType.ATOM) {
      return buildTokenFromAtom(value);
    } else {
      return new SetTheoryToken(tokenType, value);
    }
  }


  /**
   * Splits an atom string into its prefix and value if both are present, or just creates a token
   * with the value if no prefix is present.
   *
   * @param atom the atom string
   * @return The token with value and with or without prefix
   */
  private static SetTheoryToken buildTokenFromAtom(String atom) {
    String[] split = DrgLexerToken.PREFIX_SEPARATOR_PATTERN.split(atom);
    if (split.length == 2) {
      // Prefix and value
      return new SetTheoryToken(SetTheoryTokenType.ATOM, split[1].trim(), split[0].trim());
    } else if (split.length == 1) {
      // Only value
      return new SetTheoryToken(SetTheoryTokenType.ATOM, split[0].trim());
    } else {
      throw new InvalidParameterException("Invalid "
          + DrgFormulaToken.ATOM.name()
          + " '"
          + atom
          + "'. Only 'prefix"
          + DrgLexerToken.PREFIX_SEPARATOR_STRING
          + "value' or 'value' are allowed.");
    }
  }


}
