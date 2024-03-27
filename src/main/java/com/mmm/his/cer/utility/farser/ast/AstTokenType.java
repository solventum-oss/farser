package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.lexer.TokenType;

/**
 * The interface to use for an enumeration which defines tokens that are recognized for AST
 * generation this specific type of code (operand, operator, ...).<br />
 * <br />
 * Example:
 *
 * <pre>
 * public enum DrgFormulaToken implements
 *     TokenType&lt;DrgFormulaToken&gt;, AstTokenType&lt;DrgFormulaToken&gt; {
 *
 * ...
 *
 * }
 * </pre>
 *
 *
 * @param <T> The enumeration type which implements this interface.
 * @author Mike Funaro
 */
public interface AstTokenType<T extends Enum<T>> extends TokenType<T> {
  /**
   * The value which is used by default for tokens which are not operators (and which therefore do
   * not have an operator precedence set).
   */
  public static final int NOT_AN_OPERATOR = -1;

  /**
   * The precedence - or importance - of an operator which defines the order of operations (when no
   * parenthesis define the order).<br>
   * <br>
   * A lower value means higher precedence (higher up in the hierarchy of importance, performed
   * before an operation with a lower precedence).<br>
   * A higher value means lower precedence (lower down in the hierarchy of importance, performed
   * after an operation with a higher precedence).<br>
   * <br>
   * Operator precedence values must be > {@value #NOT_AN_OPERATOR}.<br>
   * Multiple operators may have the same precedence value.<br>
   * For tokens which are not an operator, return {@link #NOT_AN_OPERATOR}.
   *
   * @return The operator precedence, or {@link #NOT_AN_OPERATOR}.
   */
  int getOperatorPrecedence();

  /**
   * Checks if this token is an operator. It is considered an operator if an operator precedence is
   * set.
   *
   * @return Whether or not this token is an operator.
   */
  default boolean isOperator() {
    return getOperatorPrecedence() != NOT_AN_OPERATOR;
  }

  /**
   * Checks whether the token is an operator (has a precedence set) and its precedence value is
   * greater/equal the provided other precedence value.
   *
   * @param otherOperatorPrecedence The precedence value to test against
   * @return <code>true</code> if lower/equal precedence (less importance/weight) compare to the
   *         other
   */
  default boolean isLowerOrSamePrecedence(int otherOperatorPrecedence) {
    // Higher value means lower precedence.
    // Also handle equal precedence here.
    int prec = getOperatorPrecedence();
    return (prec != NOT_AN_OPERATOR) && (prec >= otherOperatorPrecedence);
  }

  /**
   * Checks whether the token is an operator (has a precedence set) and its precedence value is less
   * than the provided other precedence value.
   *
   * @param otherOperatorPrecedence The precedence value to test against
   * @return <code>true</code> if higher precedence (more importance/weight) compare to the other
   */
  default boolean isHigherPrecedence(int otherOperatorPrecedence) {
    // Lower value means higher precedence.
    // Do not handle equal precedence here to get proper left-to-right evaluation when multiple
    // operators with equal precedence follow each other.
    int prec = getOperatorPrecedence();
    return (prec != NOT_AN_OPERATOR) && (prec < otherOperatorPrecedence);
  }

}
