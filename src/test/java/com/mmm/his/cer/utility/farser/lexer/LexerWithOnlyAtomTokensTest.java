package com.mmm.his.cer.utility.farser.lexer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.collection.IsIterableContainingInOrder.contains;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.junit.Test;

/**
 * This test demonstrates that only the minimally required token types (all the ones in
 * {@link CommonTokenType}) are needed to lex any kind of string.
 *
 * @author Thomas Naeff
 *
 */
public class LexerWithOnlyAtomTokensTest {

  private static final MyOwnTokenFactory factory = new MyOwnTokenFactory();

  @Test
  public void allAtomTokensWithNoSpecificTypes() throws Exception {

    String toLex = "a b / ( c this works well";

    List<MyOwnToken> lexed = Lexer.lex(EmptyTokenType.class, toLex, factory);

    List<String> toTest = lexed.stream()
        .map(token -> token.getType() + ":" + token.getValue())
        .collect(Collectors.toList());
    assertThat(toTest, contains("ATOM:a", "ATOM:b", "ATOM:/", "ATOM:(", "ATOM:c", "ATOM:this",
        "ATOM:works", "ATOM:well"));

  }



  /*******************************************************************************************
   *
   * @author Thomas Naeff
   *
   */
  private enum EmptyTokenType implements TokenType<EmptyTokenType> {

    SPACE(CommonTokenType.SPACE),
    ATOM(CommonTokenType.ATOM);

    // No further enum elements here aside from the mandatory ones.

    private final CommonTokenType commonType;

    EmptyTokenType(CommonTokenType commonType) {
      this.commonType = commonType;
    }

    @Override
    public Optional<String> getValue() {
      return Optional.empty();
    }

    @Override
    public Optional<CommonTokenFlag> getCommonTokenType() {
      return Optional.of(commonType);
    }

  }

  /*******************************************************************************************
   *
   * @author Thomas Naeff
   *
   */
  private static class MyOwnToken implements LexerToken<EmptyTokenType> {

    public final EmptyTokenType type;
    public final String value;

    public MyOwnToken(EmptyTokenType type, String value) {
      this.type = type;
      this.value = value;
    }

    @Override
    public EmptyTokenType getType() {
      return type;
    }

    @Override
    public String getValue() {
      return value;
    }

    @Override
    public String toString() {
      return value;
    }

  }

  /*******************************************************************************************
   *
   * @author Thomas Naeff
   *
   */
  private static class MyOwnTokenFactory implements LexerTokenFactory<MyOwnToken, EmptyTokenType> {

    @Override
    public MyOwnToken create(EmptyTokenType tokenType, String value) {
      if (tokenType == EmptyTokenType.SPACE) {
        // Ignore spaces
        return null;
      }

      return new MyOwnToken(tokenType, value);
    }
  }

}
