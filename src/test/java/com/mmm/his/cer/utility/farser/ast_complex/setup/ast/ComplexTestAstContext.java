package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class ComplexTestAstContext {

  private final Map<String, Integer> mappingData;
  private final List<String> listData;

  public ComplexTestAstContext() {
    this(null, null);
  }

  public ComplexTestAstContext(Map<String, Integer> mappingData, List<String> listData) {
    this.mappingData = mappingData != null ? mappingData : Collections.emptyMap();
    this.listData = listData != null ? listData : Collections.emptyList();
  }

  public ComplexTestAstContext(Map<String, Integer> mappingData) {
    this(mappingData, null);
  }

  public ComplexTestAstContext(List<String> listData) {
    this(null, listData);
  }

  public Optional<Integer> getIntegerMapping(String key) {
    return Optional.ofNullable(mappingData.get(key));
  }

  public boolean contains(String value) {
    return listData.contains(value);
  }

  public boolean contains(List<String> values) {
    return listData.containsAll(values);
  }
}
