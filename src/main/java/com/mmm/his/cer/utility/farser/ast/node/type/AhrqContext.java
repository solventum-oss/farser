package com.mmm.his.cer.utility.farser.ast.node.type;

import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * farser.
 *
 * @author Mike Funaro
 */
public class AhrqContext implements MaskedContext<Attribute> {
  private final Set<Attribute> mask;
  private final Set<Attribute> accumulator;
  private final List<Attribute> evaluatedValues;
  private boolean passed;


  public AhrqContext(Set<Attribute> mask) {
    this.mask = mask;
    this.accumulator = new HashSet<>();
    this.evaluatedValues = new ArrayList<>();
  }

  @Override
  public Set<Attribute> getMask() {
    return mask;
  }

  public boolean hasMatch(Attribute other) {
    for (Attribute attr : mask) {
      if (Objects.equals(attr.getValue(), other.getValue())) {
        if (AttributeType.isMatchedAttributeType(attr.getPrefix(), other.getPrefix())) {
          return true;
        }
      }
    }
    return false;
  }

  @Override
  public void accumulate(Attribute value) {
    accumulator.add(value);
  }

  @Override
  public void evaluating(Attribute value) {
    this.evaluatedValues.add(value);
  }

  @Override
  public Set<Attribute> getMatches() {
    return accumulator;
  }

  @Override
  public List<Attribute> getEvaluatedValues() {
    return evaluatedValues;
  }

  @Override
  public void setPassed(boolean passed) {
    this.passed = passed;
  }

  public void resetAccumulator() {
    this.accumulator.clear();
    this.evaluatedValues.clear();
  }

  public List<Attribute> getNotEvaluated(List<DrgLexerToken> lexerTokens) {
    List<Attribute> list = new ArrayList<>();
    lexerTokens.forEach(token -> {
      if (token.prefix != null && token.type == DrgFormulaToken.ATOM) {
        if (evaluatedValues.stream().noneMatch(ev -> Objects.equals(ev.getValue(), token.value)
            && Objects.equals(ev.getPrefix(), token.prefix))) {
          list.add(new Attribute(AttributeType.getOperandType(token.prefix), token.value));
        }
      }
    });
    return list;
  }
}
