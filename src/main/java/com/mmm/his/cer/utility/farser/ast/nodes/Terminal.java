package com.mmm.his.cer.utility.farser.ast.nodes;

/**
 * Terminal node that is used in the AST. Terminal nodes will contain a value and can be considered
 * a "leaf" in the AST.
 *
 * @author Mike Funaro
 */
public abstract class Terminal<T> implements BooleanExpression<T> {

  protected T operandValue;


  public Terminal(T operandValue) {
    this.operandValue = operandValue;
  }

  public T getOperandValue() {
    return operandValue;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Terminal{");
    sb.append("operandValue='").append(operandValue).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
