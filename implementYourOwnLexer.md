# Implement your own Lexer

These 4 steps show how to implement custom tokens to lex formulas in any format (e.g. different operators).


## 1. Implement a TokenType

The `TokenType` specifies tokens of a formula which are recognized as an "important" type 
(a symbol for example which later needs to get accessed to further process the formula). 
The `ATOM` token is a special type: Anything that is not recognized will result in this type.

The `SPACE` type is a "convenience type". It simply allows filtering out spaces from the 
tokenized list if they are not needed (see the `LexerTokenFactory` example for a use case).

Where available, the `CommonTokenType` should be set for a token. The `CommonTokenType`
is what is utilized in further processing logic (for example AST functionality) to 
recognize specific custom tokens.


```java
public enum MyOwnTokenType implements TokenType<MyOwnTokenType> {

  ATOM(null, CommonTokenType.ATOM),
  SPACE(" ", CommonTokenType.SPACE),
  LPAREN("("),
  RPAREN(")"),

  AND("&&"),
  OR("||");

  private final String value;
  private final CommonTokenFlag commonType;

  MyOwnTokenType(String value, CommonTokenFlag commonType) {
    this.value = value;
    this.commonType = commonType;
  }

  MyOwnTokenType(String value) {
    this(value, null);
  }

  @Override
  public Optional<String> getValue() {
    return Optional.ofNullable(value);
  }

  @Override
  public Optional<CommonTokenFlag> getCommonTokenType() {
    return Optional.ofNullable(commonType);
  }
}
```

Defined tokens can contain multiple words (e.g. "IN TABLE", as two words separated by a space character) 
and they can also be mixed with the same words as individual token (e.g. "IN" and "TABLE" as additional 
separate tokens). The lexer takes care of favoring the "best match" (longest substring match), the 
order of the elements in the token enum does not matter.

```java
public enum MyOwnTokenType implements TokenType<MyOwnTokenType> {
  ...
  IN("IN"),
  IN_TABLE("IN TABLE")
  TABLE("TABLE");
  ...
}
```


## 2. Implement a LexerToken

This class represents an instance (a container) of each token - with *type* and *value* of the token. 
A list of `LexerToken` is produced by the lexer.

The *value* in this token class is generally the same as the value of the `TokenType` enum. Only 
for `ATOM` tokens the value will be something different - the ATOM *value* will be whatever the value 
from the formula is.

Depending on the `LexerTokenFactory` implementation, this token class can also contain other 
information. The `LexerTokenFactory` can produce any `LexerToken` needed for your purpose.

It is recommended that this implementation is immutable.


```java
public class MyOwnToken implements LexerToken<MyOwnTokenType> {

  public final MyOwnTokenType type;
  public final String value;

  public MyOwnToken(MyOwnTokenType type, String value) {
    this.type = type;
    this.value = value;
  }

  @Override
  public MyOwnTokenType getType() {
    return type;
  }

  @Override
  public String getValue() {
    return value;
  }
}
```


## 3. Implement a LexerTokenFactory

This factory provides a chance to bring some logic into the token creation. The example below is 
very basic and most likely the standard use case.

The `TokenFactory` also gives you a chance to can decide which tokens to actually use and which 
ones to skip.

It is recommended that this implementation is state-less and immutable.

```java
public class MyOwnTokenFactory implements LexerTokenFactory<MyOwnToken, MyOwnTokenType> {

  @Override
  public MyOwnToken create(MyOwnTokenType tokenType, String value) {
    if (tokenType == MyOwnTokenType.SPACE) {
      // Ignore spaces
      return null;
    }
    
    return new MyOwnToken(tokenType, value);
  }
}
```


## 4. Use it


```java
MyOwnTokenFactory factory = new MyOwnTokenFactory();

String input = "A || B && C";

List<MyOwnToken> tokens = Lexer.lex(MyOwnTokenType.class, input, factory);
```


