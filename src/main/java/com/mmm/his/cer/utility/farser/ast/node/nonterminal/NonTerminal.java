package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;

/**
 * Interface to provide more flexibility in the
 * {@link com.mmm.his.cer.utility.farser.ast.parser.AstDescentParser}. The descent parser will
 * create
 * node of this interface allowing any non-terminal type nodes to be used in the AST and not just
 * boolean return type nodes.
 *
 * @author Mike Funaro
 */
public interface NonTerminal<C, E> {

  /**
   * Set the left node.
   *
   * @param left the node to set.
   */
  void setLeft(Expression<C, E> left);

  /**
   * Set the right node.
   *
   * @param right the node to set.
   */
  void setRight(Expression<C, E> right);
}
