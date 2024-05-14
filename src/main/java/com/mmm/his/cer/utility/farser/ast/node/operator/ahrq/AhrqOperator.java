package com.mmm.his.cer.utility.farser.ast.node.operator.ahrq;

import com.mmm.his.cer.utility.farser.ast.node.nonterminal.BaseNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.nonterminal.SetNonTerminal;
import com.mmm.his.cer.utility.farser.ast.node.type.Expression;
import com.mmm.his.cer.utility.farser.ast.node.type.LookupContext;
import com.mmm.his.cer.utility.farser.ast.node.type.MaskedContext;
import java.util.ArrayList;
import java.util.List;

/**
 * Base node for performing Set logic. This will ensure that the left and right nodes are processed
 * so that data is fetched, and then perform the "math" as defined in the subclass.
 *
 * @author Mike Funaro
 */
public abstract class AhrqOperator<C extends MaskedContext<T>, T> extends
    BaseNonTerminal<C, EvalResult<T>> implements Expression<C, EvalResult<T>> {

}
