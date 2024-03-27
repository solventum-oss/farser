# Implement your own Abstract Syntax Tree

This documentation shows the implementation needed to produce an AST with your custom tokens 
(operands, operators, ...). The basis for this is a list of lexer tokens 
(minimally an implementation of `TokenType`  and `LexerToken` from 
["Implement your own Lexer"](implementYourOwnLexer.md)). A list of such tokens can then 
be used to produce an AST with the additions documented here.


## 1. Implement an AstTokenType

The `AstTokenType` interface is an extension to the lexer `TokenType`. These two
interfaces are separated to clearly separate the AST vs. the lexer library implementation.

With this truncated example from the [lexer documentation](implementYourOwnLexer.md), 
we can now also add the `AstTokenType` interface and configure it with the additional 
data needed for building the AST:

 - Flag specific tokens with common types available in `AstCommonTokenType`
 - Define the operator precedence


```java
public enum MyOwnTokenType implements TokenType<MyOwnTokenType>, AstTokenType<MyOwnTokenType> {

  ATOM(null, CommonTokenType.ATOM),
  SPACE(" ", CommonTokenType.SPACE),
  LPAREN("(", AstCommonTokenType.LPAREN),
  RPAREN(")", AstCommonTokenType.RPAREN),

  AND("&&", 2), // Lower value = higher precedence (stronger bond).
  OR("||", 3),  // See javadoc on 'getOperatorPrecedence()'.
  GT(">", 1),   // Multiple operators may have the same precedence.
  LT("<", 1),

  ...
  
  private final int operatorPrecedence;
  
  MyOwnTokenType(String value, CommonTokenFlag commonType) {
    this.value = value;
    this.commonType = commonType;
    this.operatorPrecedence = AstTokenType.NOT_AN_OPERATOR;
  }

  MyOwnTokenType(String value, int operatorPrecedence) {
    this.value = value;
    this.commonType = null;
    this.operatorPrecedence = operatorPrecedence;
  }
  
  ...

  @Override
  public int getOperatorPrecedence() {
    return operatorPrecedence;
  }
  
}
```


## 2. Implement Terminal Nodes (variables/values)

Terminal nodes are the ones which do not have to perform an operation based on an operator. 
A terminal node can be a scalar value, some string, a variable or some other keyword or 
special function which needs a dedicated programmatic implementation.

The formula `A > 5 & MY_FUNC`  for example will produce terminal nodes for `A`, `5` 
and `MY_FUNC`.

This example formula means that we need 3 types of terminal nodes:

 - One that substitutes the variable `A` with a number to be able to use it in the greater-than evaluation
 - One that simply returns the `5`
 - One that implements some special logic for `MY_FUNC` and returns a boolean result

The implementations are done by implementing `[Boolean]Expression`. The following example is the 
implementation which returns an integer for `5`.

```java
public class MyTerminalNode implements Expression<MyAstContext, Integer> {

  private final MyToken token;

  public MyTerminalNode(MyToken token) {
    this.token = token;
  }

  @Override
  public Integer evaluate(MyAstContext context) {
    return Integer.valueOf(token.getValue());
  }

  @Override
  public String print() {
    return token.value;
  }

  @Override
  public String toString() {
    return token.value;
  }

}
```



## 3. Implement Non-Terminal Nodes (Operators)

All the operators (the `[Ast]TokenType` enum elements which have an operator precedence set) 
will need an implementation to program the logic on how to evaluate them. For example the 
operator `>` (greater than) will need an implementation to evaluate *5 > 2*.

The implementations are done by implementing `[Boolean]NonTerminal`. Each node has a 
"left" and a "right" value (which may be the result of a nested child node evaluation) which 
it then has to evaluate based on its operator.

```java
public class MyGreaterThanOperator<C> extends NonTerminal<C, Integer> {

  @Override
  public Boolean evaluate(C context) {
    return left.evaluate(context) > right.evaluate(context);
  }

  @Override
  public String print() {
    return "GREATER-THAN";
  }

  @Override
  public String toString() {
    return "GreaterThan{" + "left=" + left + ", right=" + right + '}';
  }
}
```

## 4. Create a Context Class

A context object can be passed in to the evaluation. This context object is then passed 
to all the node evaluation (`Boolean evaluate(C context)`) calls. The context 
object can be used to record evaluation steps, provide data needed for the evaluation etc.

```java
public class MyAstContext {

  List<String> listOfValuesWhichAreTrue = ...;
  List<String> listOfNodesWhichGotVisitedDuringEvaluation = ...;
  
  ...

}
```

If no evaluation context is needed, then an empty class implementation can be provided.



## 5. Implement a NodeSupplier

Now that each operator (non-terminal node) has an implementation and we have a value 
(terminal node) implementation, we need to tie the token (enum) type together with 
its implementation. An implementation of a `NodeSupplier` can contain 
any needed logic to create those instances.

```java
public class MyAstNodeSupplier implements NodeSupplier<MyOwnToken, MyAstContext> {

  @Override
  public Expression<MyAstContext, ?> createNode(MyOwnToken inToken) {
      // The node can be created based on some keywords
      if ("MY_FUNCTION".equals(token.getValue())) {
        return new MySpecialFunctionNode(inToken);
      } else {
        return new MyTerminalNode<MyAstContext>(inToken);
      }
  }


  @Override
  public NonTerminal<MyAstContext, ?> createNonTerminalNode(MyOwnToken token) {

    switch (token.type) {
      case GT:
        return new MyGreaterThanOperator<>();
      case LT:
        return new MyLessThanOperator<>();
        
      // A few node types which are already implemented in this libary.
      // Use them or create your own.
      case AND:
        return new And<>();
      case OR:
        return new Or<>();
      case NOT:
        return new Not<>();
        
      default:
        throw new FarserException("Operator type " + token + " not implemented");
    }

  }

}
```



## 6. Use it

Build the AST:

```java
List<MyOwnToken> tokens = ...;

NodeSupplier<MyToken, MyAstContext> nodeSupplier = new MyAstNodeSupplier();

AstDescentParser<MyToken, MyTokenType, MyAstContext> parser =
        new AstDescentParser<>(tokens.iterator(), nodeSupplier);

AbstractSyntaxTree<MyAstContext> ast = parser.buildTree();
```

Now evaluate it, print it, ...

```java
MyAstContext context = ...;
// Evaluate the whole tree with...
ExpressionResult<MyAstContext> result = ast.evaluateExpression(context);
// ...or
Boolean result = ast.evaluate(context);

// Print the AST
String printed = AbstractSyntaxTreePrinter.printTree(ast);
System.out.println(printed);
```



