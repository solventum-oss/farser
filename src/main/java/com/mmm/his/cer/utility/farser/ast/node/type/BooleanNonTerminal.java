package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * This class represents a non-terminal node in the AST. These types of nodes will have a left and a
 * right child, each of child individually with a boolean return type and with a combined evaluation
 * result of a boolean type.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @author Mike Funaro
 */
public abstract class BooleanNonTerminal<C> extends NonTerminal<C, Boolean> {

  @Override
  public String toString() {
    return "NonTerminal{" + "left=" + left + ", right=" + right + '}';
  }
}
