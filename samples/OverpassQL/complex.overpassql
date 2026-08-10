[out:csv(
  ::"type",
  ::"id",
  name,
  network,
  "network:wikidata"
)];

( node[noname=yes]; ) -> .var1;

(
  node.var1[!name];
  rel(bn.var1:"outer");
) -> ._;

(._; >;); // recurse

out skel qt;