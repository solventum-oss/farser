package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.List;

/**
 * Context interface for Set logic. Any context that will be used in a Set logic AST will need to
 * implement this interface.
 *
 * @author Mike Funaro
 */
public interface LookupContext<T> {

  /**
   * Fetch data from a source.
   *
   * @param key the key of the daya.
   * @return a list of data.
   */
  List<T> lookupData(String key);
}
