implicit def toSucc(s: String) = new { def succ = BigDecimal(s) + 1 toString }
