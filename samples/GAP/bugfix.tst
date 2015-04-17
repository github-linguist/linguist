gap> START_TEST("Test for various former bugs");


gap> # The following used to trigger an error starting with:
gap> # "SolutionMat: matrix and vector incompatible called from"
gap> K:=AbelianPcpGroup([3,3,3]);;
gap> A:=Subgroup(K,[K.1]);;
gap> cr:=CRRecordBySubgroup(K,A);;
gap> ExtensionsCR(cr);;


# Comparing homomorphisms used to be broken
gap> K:=AbelianPcpGroup(1,[3]);;
gap> hom1:=GroupHomomorphismByImages(K,K,[K.1],[K.1]);;
gap> hom2:=GroupHomomorphismByImages(K,K,[K.1^2],[K.1^2]);;
gap> hom1=hom2;
true
gap> hom1=IdentityMapping(K);
true
gap> hom2=IdentityMapping(K);
true


gap> # The following incorrectly triggered an error at some point
gap> IsTorsionFree(ExamplesOfSomePcpGroups(5));
true


gap> # Verify IsGeneratorsOfMagmaWithInverses warnings are silenced
gap> IsGeneratorsOfMagmaWithInverses(GeneratorsOfGroup(ExamplesOfSomePcpGroups(5)));
true


gap> # Check for a bug reported 2012-01-19 by Robert Morse
gap> g := PcGroupToPcpGroup(SmallGroup(48,1));
Pcp-group with orders [ 2, 2, 2, 2, 3 ]
gap> # The next two commands used to trigger errors
gap> NonAbelianTensorSquare(Centre(g));
Pcp-group with orders [ 8 ]
gap> NonAbelianExteriorSquare(Centre(g));
Pcp-group with orders [  ]


gap> # Check for a bug reported 2012-01-19 by Robert Morse
gap> F := FreeGroup("x","y");
<free group on the generators [ x, y ]>
gap> x := F.1;; y := F.2;;
gap> G := F/[x^2/y^24, y^24, y^x/y^23];
<fp group on the generators [ x, y ]>
gap> iso := IsomorphismPcGroup(G);
[ x, y ] -> [ f1, f2*f5 ]
gap> iso1 := IsomorphismPcpGroup(Image(iso));
[ f1, f2, f3, f4, f5 ] -> [ g1, g2, g3, g4, g5 ]
gap> G := Image(iso*iso1);
Pcp-group with orders [ 2, 2, 2, 2, 3 ]
gap> # The next command used to trigger an error
gap> NonAbelianTensorSquare(Image(iso*iso1));
Pcp-group with orders [ 2, 2, 3, 2, 2, 2, 2 ]


gap> # The problem with the previous example is/was that Igs(G)
gap> # is set to a non-standard value:
gap> Igs(G);
[ g1, g2*g5, g3*g4*g5^2, g4*g5, g5 ]
gap> # Unfortunately, it seems that a lot of code that
gap> # really should be using Ngs or Cgs is using Igs incorrectly.
gap> # For example, direct products could return *invalid* embeddings:
gap> D := DirectProduct(G, G);
Pcp-group with orders [ 2, 2, 2, 2, 3, 2, 2, 2, 2, 3 ]
gap> hom:=Embedding(D,1);;
gap> mapi:=MappingGeneratorsImages(hom);;
gap> GroupHomomorphismByImages(Source(hom),Range(hom),mapi[1],mapi[2]) <> fail;
true
gap> hom:=Projection(D,1);;
gap> mapi:=MappingGeneratorsImages(hom);;
gap> GroupHomomorphismByImages(Source(hom),Range(hom),mapi[1],mapi[2]) <> fail;
true


gap> # Check for bug computing Schur extension of infinite cyclic groups,
gap> # found by Max Horn 2012-05-25
gap> G:=AbelianPcpGroup(1,[0]);
Pcp-group with orders [ 0 ]
gap> # The next command used to trigger an error
gap> SchurExtension(G);
Pcp-group with orders [ 0 ]


gap> # Check for bug computing Schur extensions of subgroups, found by MH 2012-05-25.
gap> G:=HeisenbergPcpGroup(2);
Pcp-group with orders [ 0, 0, 0, 0, 0 ]
gap> H:=Subgroup(G,[G.2^3*G.3^2, G.1^9]);
Pcp-group with orders [ 0, 0, 0 ]
gap> # The next command used to trigger an error
gap> SchurExtension(H);
Pcp-group with orders [ 0, 0, 0, 0, 0, 0 ]


gap> # Check for bug computing Schur extensions of subgroups, found by MH 2012-05-25.
gap> G:=HeisenbergPcpGroup(2);
Pcp-group with orders [ 0, 0, 0, 0, 0 ]
gap> H:=Subgroup(G,[G.1, G.2]);
Pcp-group with orders [ 0, 0 ]
gap> # The next command used to trigger an error
gap> SchurExtension(H);
Pcp-group with orders [ 0, 0, 0 ]


gap> # Check for bug computing normalizer of two subgroups, found by MH 2012-05-30.
gap> # The problem was caused by incorrect resp. overly restrictive use of Parent().
gap> G:=HeisenbergPcpGroup(2);
Pcp-group with orders [ 0, 0, 0, 0, 0 ]
gap> A:=Subgroup(Subgroup(G,[G.2,G.3,G.4,G.5]), [G.3]);
Pcp-group with orders [ 0 ]
gap> B:=Subgroup(Subgroup(G,[G.1,G.4,G.5]), [G.4]);
Pcp-group with orders [ 0 ]
gap> Normalizer(A,B);
Pcp-group with orders [ 0 ]
gap> # The following used to trigger the error "arguments must have a common parent group"
gap> Normalizer(B,A);
Pcp-group with orders [ 0 ]


gap> # In polycyclic 2.9 and 2.10, the code for 2-cohomology computations was broken.
gap> G := UnitriangularPcpGroup(3,0);
Pcp-group with orders [ 0, 0, 0 ]
gap> mats := G!.mats;
[ [ [ 1, 1, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ], 
  [ [ 1, 0, 0 ], [ 0, 1, 1 ], [ 0, 0, 1 ] ], 
  [ [ 1, 0, 1 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] ]
gap> C := CRRecordByMats(G,mats);;
gap> cc := TwoCohomologyCR(C);;
gap> cc.factor.rels;
[ 2, 0, 0 ]
gap> c := cc.factor.prei[2];
[ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 1 ]
gap> cc.gcb;
[ [ 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, -1, 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0 ], 
  [ 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1 ], 
  [ -1, 0, 1, 1, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 1 ] ]
gap> cc.gcc;
[ [ 1, 0, 0, 0, 0, -2, -1, 0, 1, 1, -1, -1, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 1, 0, 0, -1, -1, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 1, 0, 0, -2, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 1, 0, 0, -1, -1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1, 1 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, -1 ] ]


gap> # LowerCentralSeriesOfGroup for non-nilpotent pcp-groups used to trigger
gap> # an infinite recursion
gap> G := PcGroupToPcpGroup(SmallGroup(6,1));
Pcp-group with orders [ 2, 3 ]
gap> LowerCentralSeriesOfGroup(G);
[ Pcp-group with orders [ 2, 3 ], Pcp-group with orders [ 3 ] ]


gap> STOP_TEST( "bugfix.tst", 10000000);
