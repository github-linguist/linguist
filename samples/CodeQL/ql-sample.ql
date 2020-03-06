/**
 * Source: https://github.com/Semmle/ql/blob/bd9a2d71bacfa94cc93ce6ab33dd4c7f8ce71050/csharp/ql/src/Linq/RedundantSelect.ql
 * License: Apache License 2.0
 */

import csharp
import Helpers

predicate isIdentityFunction(AnonymousFunctionExpr afe) {
  afe.getNumberOfParameters() = 1 and
  afe.getExpressionBody() = afe.getParameter(0).getAnAccess()
}

from SelectCall sc
where isIdentityFunction(sc.getFunctionExpr())
select sc, "This LINQ selection is redundant and can be removed."
