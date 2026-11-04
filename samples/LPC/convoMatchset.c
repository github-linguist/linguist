/*
 * LDMud mob dialog system
 * Copyright 2022-2024 John Chmura
 *
 */
#include "../../jgambit.h"

private mixed *responses = ({});
private int ttl = 0;
private mixed ttl_op;

public match(string pattern, mixed response) {
  responses += ({
    ({ pattern, response })
  });

  return TO;
}

public varargs timeout(int n, mixed op) {
  ttl = n;
  ttl_op = op;
  return TO;  
}

public query_ttl() { return ttl; }
public query_ttl_op() { return ttl_op; }
public query_matchset() { return responses; }
