package com.mmm.his.cer.utility.farser.ast;

import com.mmm.his.cer.utility.farser.CommonTokenFlag;

public enum AstCommonTokenType implements CommonTokenFlag {

  /**
   * A left parenthesis "(".
   */
  LPAREN,

  /**
   * A right parenthesis ")".
   */
  RPAREN,

  /**
   * A negation/not.
   */
  NOT,

  /**
   * A left-side assignment (e.g. <code>OR</code> operator).
   */
  LEFT,

  /**
   * A right-side assignment (e.g. <code>AND</code> operator).
   */
  RIGHT;

}
