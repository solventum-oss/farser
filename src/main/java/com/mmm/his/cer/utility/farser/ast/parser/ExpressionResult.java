package com.mmm.his.cer.utility.farser.ast.parser;

import java.util.Set;

/**
 * Class that contains all information about a expression match from {@link DescentParser}.
 *
 * @param <T> the type of object that will be contains in the matches set.
 * @author Mike Funaro
 */
public class ExpressionResult<T> {

  private final boolean matched;
  private final Set<T> matches;

  /**
   * Ctor.
   *
   * @param matched boolean true if the expression evaluated to true, false otherwise. When there is
   *                true in the matched field the contents of the matches field will have the
   *                terminal objects that were matched.
   * @param matches set of terminal objects that were matched during the expression evaluation.
   */
  public ExpressionResult(boolean matched, Set<T> matches) {
    this.matched = matched;
    this.matches = matches;
  }

  public boolean isMatched() {
    return matched;
  }

  public Set<T> getMatches() {
    return matches;
  }
}
