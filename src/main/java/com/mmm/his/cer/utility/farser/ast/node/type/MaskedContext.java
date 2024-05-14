package com.mmm.his.cer.utility.farser.ast.node.type;

import java.util.Set;

/**
 * farser.
 *
 * @author Mike Funaro
 */
public interface MaskedContext<T> {
  Set<T> getMask();
}
