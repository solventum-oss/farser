package com.mmm.his.cer.utility.farser.ast.node.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.AhrqContext;
import com.mmm.his.cer.utility.farser.ast.node.type.Attribute;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import com.mmm.his.cer.utility.farser.ast.node.type.MaskedContext;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * A terminal node to be used in Set logic. This node, requires a SetContext that will either 
 * contain the data that can be looked up using a key or contain a handle to something that can 
 * be used to fetch the data from an outside source using the key.
 *
 * @author Mike Funaro
 */
public class MatchingNode<C extends AhrqContext, Attribute> implements Expression<C, List<Attribute>> {

  private final Attribute value;

  public MatchingNode(Attribute value) {
    this.value = value;
  }

  @Override
  public List<Attribute> evaluate(C context) {
    Set<Attribute> mask = context.getMask();
    boolean contains = mask.contains(value);
    
    if(contains){
      return Arrays.asList(value);
    }
    return new ArrayList<>();
  }

  @Override
  public String print() {
    return "Data Lookup key: " + value;
  }
}
