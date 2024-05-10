package com.mmm.his.cer.utility.farser.ast.parser;

/**
 * Class that contains all information about a expression match from {@link DescentParser}.
 *
 * @param <C> the type of context which was used for the terminal nodes to evaluate the AST
 * @author Mike Funaro
 */
public class ExpressionResult<C, R> {

  private final R result;
  private final C context;

  /**
   * Ctor.
   *
   * @param result boolean true if the expression evaluated to true, false otherwise. When there is
   *                true in the matched field the contents of the matches field will have the
   *                terminal objects that were matched.
   * @param context the context used in evaluation.
   */
  public ExpressionResult(R result, C context) {
    this.result = result;
    this.context = context;
  }

  public R getResult() {
    return result;
  }

  public C getContext() {
    return context;
  }
}
