package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.List;

/**
 * Context interface which provides a way to lookup data based on a key.
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
