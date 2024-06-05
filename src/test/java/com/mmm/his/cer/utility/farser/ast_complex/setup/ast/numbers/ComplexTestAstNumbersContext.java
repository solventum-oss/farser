package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.numbers;

import java.util.Map;
import java.util.Optional;

public class ComplexTestAstNumbersContext {

  private final Map<String, Integer> intMapping;

  public ComplexTestAstNumbersContext(Map<String, Integer> intMapping) {
    this.intMapping = intMapping;
  }
  
  public Optional<Integer> getIntMapping(String key) {
    return Optional.ofNullable(intMapping.get(key));
  }

}
