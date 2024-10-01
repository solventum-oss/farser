package com.mmm.his.cer.utility.farser.ast_complex.setup.ast.terminal;

import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast_complex.setup.ast.ComplexTestAstContext;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class ComplexTestMaxNumberNode implements Expression<ComplexTestAstContext, Integer> {

    private final List<String> items;
    private final int excluding;

    public ComplexTestMaxNumberNode(List<String> items, int excluding) {
        this.items = items;
        this.excluding = excluding;
    }

    @Override
    public Integer evaluate(ComplexTestAstContext context) {
        Set<Integer> numbers = new HashSet<>();
        items.forEach(key -> {
            Optional<Integer> number = context.getIntegerMapping(key);
            if (number.isPresent() && !number.get().equals(excluding)) {
                numbers.add(number.get());
            }
        });
        return !numbers.isEmpty() ? Collections.max(numbers) : null;
    }
}
