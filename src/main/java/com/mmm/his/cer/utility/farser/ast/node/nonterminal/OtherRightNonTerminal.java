package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

/**
 * For non-terminal expressions that have the same left and this return type, but a different 
 * right return type.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <R> The result type of the right expression.
 * @param <E> The result type of the left expression and this expression.
 */
public abstract class OtherRightNonTerminal<C, R, E> extends BaseNonTerminalExpression<C, E, R, E> {

}
