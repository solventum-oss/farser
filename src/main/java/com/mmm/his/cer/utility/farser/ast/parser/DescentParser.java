package com.mmm.his.cer.utility.farser.ast.parser;

import com.mmm.his.cer.utility.farser.ast.node.type.NodeSupplier;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgFormulaToken;
import com.mmm.his.cer.utility.farser.lexer.drg.DrgLexerToken;
import java.util.Iterator;
import java.util.Map;

/**
 * Recursive descent parser that will build an Abstract syntax tree from <b>a DRG formula</b> (list
 * of {@link DrgLexerToken} formula tokens).<br>
 * This implementation simply extends {@link AstDescentParser}, with DRG-specific token types
 * defined.
 *
 * @author Mike Funaro
 *
 * @param <C> The type of the context used in terminal nodes when evaluating the AST
 */
public class DescentParser<C> extends AstDescentParser<DrgLexerToken, DrgFormulaToken, C> {

  public DescentParser(Iterator<DrgLexerToken> tokenIterator,
      NodeSupplier<DrgLexerToken, C> defaultSupplier,
      Map<String, NodeSupplier<DrgLexerToken, C>> suppliers) {
    super(tokenIterator, defaultSupplier, suppliers);
  }

}
