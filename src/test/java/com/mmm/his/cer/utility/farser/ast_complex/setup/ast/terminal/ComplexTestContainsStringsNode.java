package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;
import com.mmm.his.cer.utility.farser.ast_complex.setup.lex.ComplexTestToken;

import java.util.List;
import java.util.stream.Collectors;

public class ComplexTestContainsStringsNode implements Expression<ComplexTestAstContext, Boolean> {

    private final List<String> items;

    public ComplexTestContainsStringsNode(List<ComplexTestToken> items) {
        this.items = items.stream().map(v -> v.value).collect(Collectors.toList());
    }

    @Override
    public Boolean evaluate(ComplexTestAstContext context) {
        return context.contains(items);
    }
}

