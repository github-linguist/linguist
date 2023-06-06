/**
 * Source: https://github.com/krisds/cobol-codeql/blob/9373a45da534889cb5d3ae1841a5233a38abceee/queries/semmlecode-cobol-queries/SQL/SpecifyWhereClauseOnSelect.ql
 * Lisence: MIT
 */

import cobol

from SqlSelectStmt stmt
where not exists (stmt.getWhere())
  // Make sure we're selecting from an actual table.
  and exists ( SqlTableReference ref |
    ref = stmt.getFrom().getATarget() and
    not ref.(SqlTableName).getName().toUpperCase() = "DUAL"
  )
select stmt, "Unconditional selection of data."
