package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.Set;

/**
 * Wrapper class for a set of Attributes which implements the necessary interface to work with
 * our terminal nodes.
 *
 * @author Mike Funaro
 */
public class AhrqContext implements MaskedContext<Attribute> {

  private final Set<Attribute> mask;


  public AhrqContext(Set<Attribute> mask) {
    this.mask = mask;
  }

  @Override
  public Set<Attribute> getMask() {
    return mask;
  }

}
