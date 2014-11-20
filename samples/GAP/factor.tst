gap> START_TEST("Test of factor groups and natural homomorphisms");

gap> G:=HeisenbergPcpGroup(2);
Pcp-group with orders [ 0, 0, 0, 0, 0 ]

gap> H:=Subgroup(G,[G.2,G.3,G.4,G.5]);
gap> K:=G/H;
gap> NaturalHomomorphism(K);

gap> A:=Subgroup(H, [G.3]);
Pcp-group with orders [ 0 ]
gap> B:=Subgroup(Subgroup(G,[G.1,G.4,G.5]), [G.4]);
Pcp-group with orders [ 0 ]
gap> Normalizer(A,B);
Pcp-group with orders [ 0 ]
gap> # The following used to trigger the error "arguments must have a common parent group"
gap> Normalizer(B,A);
Pcp-group with orders [ 0 ]


gap> STOP_TEST( "factor.tst", 10000000);
