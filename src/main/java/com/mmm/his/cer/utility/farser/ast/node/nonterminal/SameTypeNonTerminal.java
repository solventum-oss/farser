package com.mmm.his.cer.utility.farser.ast.node.nonterminal;

/**
 * For non-terminal expressions that have the same left, right, and this return types.
 *
 * @param <C> The node context type used in the terminal nodes.
 * @param <E> The result type of the left, right, and this expression.
 */
public abstract class SameTypeNonTerminal<C, E> extends BaseNonTerminalExpression<C, E, E, E> {

}
