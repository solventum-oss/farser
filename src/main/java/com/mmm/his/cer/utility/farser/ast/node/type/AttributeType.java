package com.mmm.his.cer.utility.farser.ast.node.type;

/**
 * Temp class to get something working.
 *
 * @author Mike Funaro
 */
public enum AttributeType {
  PDX("PDX"),
  SDX("SDX"),
  DX("DX"),
  PR("PR"),
  NONE(null);

  private final String prefix;

  AttributeType(String prefix) {
    this.prefix = prefix;
  }

  /**
   * Comment.
   */
  public static AttributeType getOperandType(String str) {
    if (str != null) {
      for (AttributeType v : values()) {
        if (str.equalsIgnoreCase(v.prefix)) {
          return v;
        }
      }
    }
    return NONE;
  }

  /**
   * Comment.
   */
  public static boolean isMatchedAttributeType(AttributeType first, AttributeType second) {
    boolean match = false;
    if (first == second) {
      match = true;
    } else if (first == AttributeType.DX) {
      match = second == AttributeType.PDX || second == AttributeType.SDX;
    } else if (second == AttributeType.DX) {
      match = first == AttributeType.PDX || first == AttributeType.SDX;
    }
    return match;
  }
}
