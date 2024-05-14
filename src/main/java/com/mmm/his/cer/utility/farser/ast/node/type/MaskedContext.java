package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.List;
import java.util.Set;

/**
 * farser.
 *
 * @author Mike Funaro
 */
public interface MaskedContext<T> {
  Set<T> getMask();

  void accumulate(T value);

  void evaluating(T value);

  Set<T> getMatches();

  List<T> getEvaluatedValues();
  
  void setPassed(boolean passed);
}
