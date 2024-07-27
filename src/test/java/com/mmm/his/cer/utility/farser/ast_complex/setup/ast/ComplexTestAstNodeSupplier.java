package com.mmm.his.cer.utility.farser.ast_complex.setup.ast;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.And;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.Not;
import com.mmm.his.cer.utility.farser.ast.node.operator.bool.Or;
import com.mmm.his.cer.utility.farser.ast.node.supplier.NodeSupplier;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal.ComplexTestEqualOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal.ComplexTestGreaterThanOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal.ComplexTestInOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal.ComplexTestInTableOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.non_terminal.ComplexTestLessThanOperator;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.terminal.*;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;
import com.mmm.his.cer.utility.farser.lexer.FarserException;

import java.util.List;
import java.util.stream.Collectors;


public class ComplexTestAstNodeSupplier implements NodeSupplier<ComplexTestToken, ComplexTestAstContext> {
  
  private boolean isInteger(String value) {
    boolean result = true;
    try {
      Integer.parseInt(value);
    } catch (NumberFormatException exc) {
      result = false;
    }
    return result;
  }
  
  private boolean isLastHalfOfAlphabet(String value) {
    if (value.length() != 1) {
      return false;
    }
    int a = 'a';
    int position = (String.valueOf(value.charAt(0)).toLowerCase().codePointAt(0)) - a + 1;
    return position >= 13;
  }

  @Override
  public Expression<ComplexTestAstContext, ?> createNode(final ComplexTestToken inToken) {
    Expression<ComplexTestAstContext, ?> expression;

    if (isInteger(inToken.value)) {
      expression = new ComplexTestTerminalNumberNode(inToken);
    } else if (inToken.value.toLowerCase().equals(inToken.value)) {
      expression = new ComplexTestTerminalStringNode(inToken);
    } else if (isLastHalfOfAlphabet(inToken.value)) {
      expression = new ComplexTestTerminalContainsNode(inToken);
    } else {
      expression = new ComplexTestTerminalLookupNode(inToken);
    }
    return expression;
  }

  @Override
  public Expression<ComplexTestAstContext, ?> createNode(ComplexTestToken functionToken, List<ComplexTestToken> args) {
    Expression<ComplexTestAstContext, ?> expression;

    if (functionToken.value.equals("containsStrings")) {
      expression = new ComplexTestContainsStringsNode(args);
    } else if (functionToken.value.equals("maxNumber")) {
      int exclusion = Integer.parseInt(args.get(0).value);
      List<String> keys = args.stream().map(v -> v.value).collect(Collectors.toList());
      expression = new ComplexTestMaxNumberNode(keys, exclusion);
    } else {
      throw new FarserException(functionToken + " not implemented");
    }
    return expression;
  }

  @Override
  public BaseNonTerminal<ComplexTestAstContext, ?> createNonTerminalNode(ComplexTestToken token) {

    switch (token.type) {
      case GT:
        return new ComplexTestGreaterThanOperator<>();
      case LT:
        return new ComplexTestLessThanOperator<>();
      case EQUAL:
        return new ComplexTestEqualOperator<>();
      case AND:
        return new And<>();
      case OR:
        return new Or<>();
      case NOT:
        return new Not<>();
      case IN:
        return new ComplexTestInOperator<>();
      case IN_TABLE:
        return new ComplexTestInTableOperator<>();
      default:
        throw new FarserException("Operator type " + token + " not implemented");
    }
  }
}

