package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * Interface for calling applications to implement so that they can provide custom terminal nodes
 * for special logic to be evaluated within the AST.
 *
 * @param <T> the token type that will be used to create the node
 * @param <R> the parametric type on {@link BooleanExpression}
 *
 * @author Mike Funaro
 */
public interface NodeSupplier<T, R> {

  /**
   * Create a terminal node. This is type defined on the class. The input will be a token of a
   * particular type which is used in the body of the method to create an instance of
   * {@link BooleanExpression} that is type defined again using the types on the class.
   *
   * @return BooleanExpression that was instantiated in this method.
   */
  BooleanExpression<R> createNode(T token);
}
