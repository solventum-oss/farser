package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class ComplexTestAstContext {

  private final Map<String, Integer> mappingData;
  private final List<String> listData;

  public ComplexTestAstContext() {
    this.mappingData = new HashMap<>();
    this.listData = new ArrayList<>();
  }

  public ComplexTestAstContext(Map<String, Integer> mappingData, List<String> listData) {
    this.mappingData = mappingData;
    this.listData = listData;
  }

  public ComplexTestAstContext(Map<String, Integer> mappingData) {
    this.mappingData = mappingData;
    this.listData = new ArrayList<>();
  }

  public ComplexTestAstContext(List<String> listData) {
    this.listData = listData;
    this.mappingData = new HashMap<>();
  }

  public Optional<Integer> getIntegerMapping(String key) {
    return Optional.ofNullable(mappingData.get(key));
  }

  public boolean contains(String value) {
    return listData.contains(value);
  }

}
