package com.mmm.his.cer.utility.farser.ast.node.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import java.util.Collection;

/**
 * A terminal node to be used in Set theory. This node, requires a SetContext that will either 
 * contain the data that can be looked up using a key or contain a handle to something that can 
 * be used to fetch the data from an outside source using the key.
 *
 * @author Mike Funaro
 */
public class DataLookupNode<C extends LookupContext<T>, T> implements Expression<C, Collection<T>> {

  private final String key;

  public DataLookupNode(String key) {
    this.key = key;
  }

  @Override
  public Collection<T> evaluate(C context) {
    return context.lookupData(key);
  }

  @Override
  public String print() {
    return "Data Lookup key: " + key;
  }
}
