package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.Objects;

/**
 * farser.
 *
 * @author Mike Funaro
 */
public class Attribute {

  private final AttributeType prefix;
  private final String value;


  public Attribute(AttributeType prefix, String value) {
    this.prefix = prefix;
    this.value = value;
  }

  @Override
  public String toString() {
    return "Attribute{" 
        + "prefix=" + prefix 
        + ", listName='" + value + '\'' 
        + '}';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Attribute attribute = (Attribute) o;
    return prefix == attribute.prefix && Objects.equals(value, attribute.value);
  }

  @Override
  public int hashCode() {
    return Objects.hash(prefix, value);
  }

  public AttributeType getPrefix() {
    return prefix;
  }

  public String getValue() {
    return value;
  }
}
