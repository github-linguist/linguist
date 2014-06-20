#############################################################################
##
#W  vspc.gd                     GAP library                     Thomas Breuer
##
##
#Y  Copyright (C)  1997,  Lehrstuhl D für Mathematik,  RWTH Aachen,  Germany
#Y  (C) 1998 School Math and Comp. Sci., University of St Andrews, Scotland
#Y  Copyright (C) 2002 The GAP Group
##
##  This file declares the operations for vector spaces.
##
##  The operations for bases of free left modules can be found in the file
##  <F>lib/basis.gd<F>.
##


#############################################################################
##
#C  IsLeftOperatorRing(<R>)
##
##  <ManSection>
##  <Filt Name="IsLeftOperatorRing" Arg='R' Type='Category'/>
##
##  <Description>
##  </Description>
##  </ManSection>
##
DeclareSynonym( "IsLeftOperatorRing",
    IsLeftOperatorAdditiveGroup and IsRing and IsAssociativeLOpDProd );
#T really?


#############################################################################
##
#C  IsLeftOperatorRingWithOne(<R>)
##
##  <ManSection>
##  <Filt Name="IsLeftOperatorRingWithOne" Arg='R' Type='Category'/>
##
##  <Description>
##  </Description>
##  </ManSection>
##
DeclareSynonym( "IsLeftOperatorRingWithOne",
    IsLeftOperatorAdditiveGroup and IsRingWithOne
    and IsAssociativeLOpDProd );
#T really?


#############################################################################
##
#C  IsLeftVectorSpace( <V> )
#C  IsVectorSpace( <V> )
##
##  <#GAPDoc Label="IsLeftVectorSpace">
##  <ManSection>
##  <Filt Name="IsLeftVectorSpace" Arg='V' Type='Category'/>
##  <Filt Name="IsVectorSpace" Arg='V' Type='Category'/>
##
##  <Description>
##  A <E>vector space</E> in &GAP; is a free left module
##  (see&nbsp;<Ref Func="IsFreeLeftModule"/>) over a division ring
##  (see Chapter&nbsp;<Ref Chap="Fields and Division Rings"/>).
##  <P/>
##  Whenever we talk about an <M>F</M>-vector space <A>V</A> then <A>V</A> is
##  an additive group (see&nbsp;<Ref Func="IsAdditiveGroup"/>) on which the
##  division ring <M>F</M> acts via multiplication from the left such that
##  this action and the addition in <A>V</A> are left and right distributive.
##  The division ring <M>F</M> can be accessed as value of the attribute
##  <Ref Func="LeftActingDomain"/>.
##  <P/>
##  Vector spaces in &GAP; are always <E>left</E> vector spaces,
##  <Ref Filt="IsLeftVectorSpace"/> and <Ref Filt="IsVectorSpace"/> are
##  synonyms.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "IsLeftVectorSpace",
    IsLeftModule and IsLeftActedOnByDivisionRing );

DeclareSynonym( "IsVectorSpace", IsLeftVectorSpace );

InstallTrueMethod( IsFreeLeftModule,
    IsLeftModule and IsLeftActedOnByDivisionRing );


#############################################################################
##
#F  IsGaussianSpace( <V> )
##
##  <#GAPDoc Label="IsGaussianSpace">
##  <ManSection>
##  <Func Name="IsGaussianSpace" Arg='V'/>
##
##  <Description>
##  The filter <Ref Filt="IsGaussianSpace"/> (see&nbsp;<Ref Sect="Filters"/>)
##  for the row space (see&nbsp;<Ref Func="IsRowSpace"/>)
##  or matrix space (see&nbsp;<Ref Func="IsMatrixSpace"/>) <A>V</A>
##  over the field <M>F</M>, say,
##  indicates that the entries of all row vectors or matrices in <A>V</A>,
##  respectively, are all contained in <M>F</M>.
##  In this case, <A>V</A> is called a <E>Gaussian</E> vector space.
##  Bases for Gaussian spaces can be computed using Gaussian elimination for
##  a given list of vector space generators.
##  <Example><![CDATA[
##  gap> mats:= [ [[1,1],[2,2]], [[3,4],[0,1]] ];;
##  gap> V:= VectorSpace( Rationals, mats );;
##  gap> IsGaussianSpace( V );
##  true
##  gap> mats[1][1][1]:= E(4);;   # an element in an extension field
##  gap> V:= VectorSpace( Rationals, mats );;
##  gap> IsGaussianSpace( V );
##  false
##  gap> V:= VectorSpace( Field( Rationals, [ E(4) ] ), mats );;
##  gap> IsGaussianSpace( V );
##  true
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareFilter( "IsGaussianSpace", IsVectorSpace );

InstallTrueMethod( IsGaussianSpace,
    IsVectorSpace and IsFullMatrixModule );

InstallTrueMethod( IsGaussianSpace,
    IsVectorSpace and IsFullRowModule );


#############################################################################
##
#C  IsDivisionRing( <D> )
##
##  <#GAPDoc Label="IsDivisionRing">
##  <ManSection>
##  <Filt Name="IsDivisionRing" Arg='D' Type='Category'/>
##
##  <Description>
##  A <E>division ring</E> in &GAP; is a nontrivial associative algebra
##  <A>D</A> with a multiplicative inverse for each nonzero element.
##  In &GAP; every division ring is a vector space over a division ring
##  (possibly over itself).
##  Note that being a division ring is thus not a property that a ring can
##  get, because a ring is usually not represented as a vector space.
##  <P/>
##  The field of coefficients is stored as the value of the attribute
##  <Ref Func="LeftActingDomain"/> of <A>D</A>.
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonymAttr( "IsDivisionRing",
        IsMagmaWithInversesIfNonzero
    and IsLeftOperatorRingWithOne
    and IsLeftVectorSpace
    and IsNonTrivial
    and IsAssociative
    and IsEuclideanRing );


#############################################################################
##
#A  GeneratorsOfLeftVectorSpace( <V> )
#A  GeneratorsOfVectorSpace( <V> )
##
##  <#GAPDoc Label="GeneratorsOfLeftVectorSpace">
##  <ManSection>
##  <Attr Name="GeneratorsOfLeftVectorSpace" Arg='V'/>
##  <Attr Name="GeneratorsOfVectorSpace" Arg='V'/>
##
##  <Description>
##  For an <M>F</M>-vector space <A>V</A>,
##  <Ref Attr="GeneratorsOfLeftVectorSpace"/> returns a list of vectors in
##  <A>V</A> that generate <A>V</A> as an <M>F</M>-vector space.
##  <Example><![CDATA[
##  gap> GeneratorsOfVectorSpace( FullRowSpace( Rationals, 3 ) );
##  [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonymAttr( "GeneratorsOfLeftVectorSpace",
    GeneratorsOfLeftOperatorAdditiveGroup );

DeclareSynonymAttr( "GeneratorsOfVectorSpace",
    GeneratorsOfLeftOperatorAdditiveGroup );


#############################################################################
##
#A  CanonicalBasis( <V> )
##
##  <#GAPDoc Label="CanonicalBasis">
##  <ManSection>
##  <Attr Name="CanonicalBasis" Arg='V'/>
##
##  <Description>
##  If the vector space <A>V</A> supports a <E>canonical basis</E> then
##  <Ref Attr="CanonicalBasis"/> returns this basis,
##  otherwise <K>fail</K> is returned.
##  <P/>
##  The defining property of a canonical basis is that its vectors are
##  uniquely determined by the vector space.
##  If canonical bases exist for two vector spaces over the same left acting
##  domain (see&nbsp;<Ref Func="LeftActingDomain"/>) then the equality of
##  these vector spaces can be decided by comparing the canonical bases.
##  <P/>
##  The exact meaning of a canonical basis depends on the type of <A>V</A>.
##  Canonical bases are defined for example for Gaussian row and matrix
##  spaces (see&nbsp;<Ref Sect="Row and Matrix Spaces"/>).
##  <P/>
##  If one designs a new kind of vector spaces
##  (see&nbsp;<Ref Sect="How to Implement New Kinds of Vector Spaces"/>) and
##  defines a canonical basis for these spaces then the
##  <Ref Attr="CanonicalBasis"/> method one installs
##  (see&nbsp;<Ref Func="InstallMethod"/>)
##  must <E>not</E> call <Ref Func="Basis"/>.
##  On the other hand, one probably should install a <Ref Func="Basis"/>
##  method that simply calls <Ref Attr="CanonicalBasis"/>,
##  the value of the method
##  (see&nbsp;<Ref Sect="Method Installation"/> and
##  <Ref Sect="Applicable Methods and Method Selection"/>)
##  being <C>CANONICAL_BASIS_FLAGS</C>.
##  <Example><![CDATA[
##  gap> vecs:= [ [ 1, 2, 3 ], [ 1, 1, 1 ], [ 1, 1, 1 ] ];;
##  gap> V:= VectorSpace( Rationals, vecs );;
##  gap> B:= CanonicalBasis( V );
##  CanonicalBasis( <vector space over Rationals, with 3 generators> )
##  gap> BasisVectors( B );
##  [ [ 1, 0, -1 ], [ 0, 1, 2 ] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "CanonicalBasis", IsFreeLeftModule );


#############################################################################
##
#F  IsRowSpace( <V> )
##
##  <#GAPDoc Label="IsRowSpace">
##  <ManSection>
##  <Func Name="IsRowSpace" Arg='V'/>
##
##  <Description>
##  A <E>row space</E> in &GAP; is a vector space that consists of
##  row vectors (see Chapter&nbsp;<Ref Chap="Row Vectors"/>).
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "IsRowSpace", IsRowModule and IsVectorSpace );


#############################################################################
##
#F  IsGaussianRowSpace( <V> )
##
##  <ManSection>
##  <Func Name="IsGaussianRowSpace" Arg='V'/>
##
##  <Description>
##  A row space is <E>Gaussian</E> if the left acting domain contains all
##  scalars that occur in the vectors.
##  Thus one can use Gaussian elimination in the calculations.
##  <P/>
##  (Otherwise the space is non-Gaussian.
##  We will need a flag for this to write down methods that delegate from
##  non-Gaussian spaces to Gaussian ones.)
##  <!-- reformulate this when it becomes documented -->
##  </Description>
##  </ManSection>
##
DeclareSynonym( "IsGaussianRowSpace", IsGaussianSpace and IsRowSpace );


#############################################################################
##
#F  IsNonGaussianRowSpace( <V> )
##
##  <ManSection>
##  <Func Name="IsNonGaussianRowSpace" Arg='V'/>
##
##  <Description>
##  If an <M>F</M>-vector space <A>V</A> is in the filter
##  <Ref Func="IsNonGaussianRowSpace"/> then this expresses that <A>V</A>
##  consists of row vectors (see&nbsp;<Ref Func="IsRowVector"/>) such
##  that not all entries in these row vectors are contained in <M>F</M>
##  (so Gaussian elimination cannot be used to compute an <M>F</M>-basis
##  from a list of vector space generators),
##  and that <A>V</A> is handled via the mechanism of nice bases
##  (see&nbsp;<Ref ???="..."/>) in the following way.
##  Let <M>K</M> be the field spanned by the entries of all vectors in
##  <A>V</A>.
##  Then the <Ref Attr="NiceFreeLeftModuleInfo"/> value of <A>V</A> is
##  a basis <M>B</M> of the field extension <M>K / ( K \cap F )</M>,
##  and the <Ref Func="NiceVector"/> value of <M>v \in <A>V</A></M>
##  is defined by replacing each entry of <M>v</M> by the list of its
##  <M>B</M>-coefficients, and then forming the concatenation.
##  <P/>
##  So the associated nice vector space is a Gaussian row space
##  (see&nbsp;<Ref Func="IsGaussianRowSpace"/>).
##  </Description>
##  </ManSection>
##
DeclareHandlingByNiceBasis( "IsNonGaussianRowSpace",
    "for non-Gaussian row spaces" );


#############################################################################
##
#F  IsMatrixSpace( <V> )
##
##  <#GAPDoc Label="IsMatrixSpace">
##  <ManSection>
##  <Func Name="IsMatrixSpace" Arg='V'/>
##
##  <Description>
##  A <E>matrix space</E> in &GAP; is a vector space that consists of matrices
##  (see Chapter&nbsp;<Ref Chap="Matrices"/>).
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "IsMatrixSpace", IsMatrixModule and IsVectorSpace );


#############################################################################
##
#F  IsGaussianMatrixSpace( <V> )
##
##  <ManSection>
##  <Func Name="IsGaussianMatrixSpace" Arg='V'/>
##
##  <Description>
##  A matrix space is Gaussian if the left acting domain contains all
##  scalars that occur in the vectors.
##  Thus one can use Gaussian elimination in the calculations.
##  <P/>
##  (Otherwise the space is non-Gaussian.
##  We will need a flag for this to write down methods that delegate from
##  non-Gaussian spaces to Gaussian ones.)
##  </Description>
##  </ManSection>
##
DeclareSynonym( "IsGaussianMatrixSpace", IsGaussianSpace and IsMatrixSpace );


#############################################################################
##
#F  IsNonGaussianMatrixSpace( <V> )
##
##  <ManSection>
##  <Func Name="IsNonGaussianMatrixSpace" Arg='V'/>
##
##  <Description>
##  If an <M>F</M>-vector space <A>V</A> is in the filter
##  <Ref Func="IsNonGaussianMatrixSpace"/>
##  then this expresses that <A>V</A> consists of matrices
##  (see&nbsp;<Ref Func="IsMatrix"/>)
##  such that not all entries in these matrices are contained in <M>F</M>
##  (so Gaussian elimination cannot be used to compute an <M>F</M>-basis
##  from a list of vector space generators),
##  and that <A>V</A> is handled via the mechanism of nice bases
##  (see&nbsp;<Ref ???="..."/>) in the following way.
##  Let <M>K</M> be the field spanned by the entries of all vectors in <A>V</A>.
##  The <Ref Attr="NiceFreeLeftModuleInfo"/> value of <A>V</A> is irrelevant,
##  and the <Ref Func="NiceVector"/> value of <M>v \in <A>V</A></M>
##  is defined as the concatenation of the rows of <M>v</M>.
##  <P/>
##  So the associated nice vector space is a (not necessarily Gaussian)
##  row space (see&nbsp;<Ref Func="IsRowSpace"/>).
##  </Description>
##  </ManSection>
##
DeclareHandlingByNiceBasis( "IsNonGaussianMatrixSpace",
    "for non-Gaussian matrix spaces" );


#############################################################################
##
#A  NormedRowVectors( <V> ) . . .  normed vectors in a Gaussian row space <V>
##
##  <#GAPDoc Label="NormedRowVectors">
##  <ManSection>
##  <Attr Name="NormedRowVectors" Arg='V'/>
##
##  <Description>
##  For a finite Gaussian row space <A>V</A>
##  (see&nbsp;<Ref Func="IsRowSpace"/>, <Ref Func="IsGaussianSpace"/>),
##  <Ref Attr="NormedRowVectors"/> returns a list of those nonzero
##  vectors in <A>V</A> that have a one in the first nonzero component.
##  <P/>
##  The result list can be used as action domain for the action of a matrix
##  group via <Ref Func="OnLines"/>, which yields the natural action on
##  one-dimensional subspaces of <A>V</A>
##  (see also&nbsp;<Ref Func="Subspaces"/>).
##  <Example><![CDATA[
##  gap> vecs:= NormedRowVectors( GF(3)^2 );
##  [ [ 0*Z(3), Z(3)^0 ], [ Z(3)^0, 0*Z(3) ], [ Z(3)^0, Z(3)^0 ], 
##    [ Z(3)^0, Z(3) ] ]
##  gap> Action( GL(2,3), vecs, OnLines );
##  Group([ (3,4), (1,2,4) ])
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "NormedRowVectors", IsGaussianSpace );


#############################################################################
##
#A  TrivialSubspace( <V> )
##
##  <#GAPDoc Label="TrivialSubspace">
##  <ManSection>
##  <Attr Name="TrivialSubspace" Arg='V'/>
##
##  <Description>
##  For a vector space <A>V</A>, <Ref Attr="TrivialSubspace"/> returns the
##  subspace of <A>V</A> that consists of the zero vector in <A>V</A>.
##  <Example><![CDATA[
##  gap> V:= GF(3)^3;;
##  gap> triv:= TrivialSubspace( V );
##  <vector space over GF(3), with 0 generators>
##  gap> AsSet( triv );
##  [ [ 0*Z(3), 0*Z(3), 0*Z(3) ] ]
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonymAttr( "TrivialSubspace", TrivialSubmodule );


#############################################################################
##
#F  VectorSpace( <F>, <gens>[, <zero>][, "basis"] )
##
##  <#GAPDoc Label="VectorSpace">
##  <ManSection>
##  <Func Name="VectorSpace" Arg='F, gens[, zero][, "basis"]'/>
##
##  <Description>
##  For a field <A>F</A> and a collection <A>gens</A> of vectors,
##  <Ref Func="VectorSpace"/> returns the <A>F</A>-vector space spanned by
##  the elements in <A>gens</A>.
##  <P/>
##  The optional argument <A>zero</A> can be used to specify the zero element
##  of the space; <A>zero</A> <E>must</E> be given if <A>gens</A> is empty.
##  The optional string <C>"basis"</C> indicates that <A>gens</A> is known to
##  be linearly independent over <A>F</A>, in particular the dimension of the
##  vector space is immediately set;
##  note that <Ref Func="Basis"/> need <E>not</E> return the basis formed by
##  <A>gens</A> if the string <C>"basis"</C> is given as an argument.
##  <!-- crossref. to <C>FreeLeftModule</C> as soon as the modules chapter
##       is reliable!-->
##  <Example><![CDATA[
##  gap> V:= VectorSpace( Rationals, [ [ 1, 2, 3 ], [ 1, 1, 1 ] ] );
##  <vector space over Rationals, with 2 generators>
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareGlobalFunction( "VectorSpace" );


#############################################################################
##
#F  Subspace( <V>, <gens>[, "basis"] )  . subspace of <V> generated by <gens>
#F  SubspaceNC( <V>, <gens>[, "basis"] )
##
##  <#GAPDoc Label="Subspace">
##  <ManSection>
##  <Func Name="Subspace" Arg='V, gens[, "basis"]'/>
##  <Func Name="SubspaceNC" Arg='V, gens[, "basis"]'/>
##
##  <Description>
##  For an <M>F</M>-vector space <A>V</A> and a list or collection
##  <A>gens</A> that is a subset of <A>V</A>,
##  <Ref Func="Subspace"/> returns the <M>F</M>-vector space spanned by
##  <A>gens</A>; if <A>gens</A> is empty then the trivial subspace
##  (see&nbsp;<Ref Func="TrivialSubspace"/>) of <A>V</A> is returned.
##  The parent (see&nbsp;<Ref Sect="Parents"/>) of the returned vector space
##  is set to <A>V</A>.
##  <P/>
##  <Ref Func="SubspaceNC"/> does the same as <Ref Func="Subspace"/>,
##  except that it omits the check whether <A>gens</A> is a subset of
##  <A>V</A>.
##  <P/>
##  The optional string <A>"basis"</A> indicates that <A>gens</A> is known to
##  be linearly independent over <M>F</M>.
##  In this case the dimension of the subspace is immediately set,
##  and both <Ref Func="Subspace"/> and <Ref Func="SubspaceNC"/> do
##  <E>not</E> check whether <A>gens</A> really is linearly independent and
##  whether <A>gens</A> is a subset of <A>V</A>.
##  <!-- crossref. to <C>Submodule</C> as soon as the modules chapter
##       is reliable!-->
##  <Example><![CDATA[
##  gap> V:= VectorSpace( Rationals, [ [ 1, 2, 3 ], [ 1, 1, 1 ] ] );;
##  gap> W:= Subspace( V, [ [ 0, 1, 2 ] ] );
##  <vector space over Rationals, with 1 generators>
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "Subspace", Submodule );

DeclareSynonym( "SubspaceNC", SubmoduleNC );


#############################################################################
##
#O  AsVectorSpace( <F>, <D> ) . . . . . . . . .  view <D> as <F>-vector space
##
##  <#GAPDoc Label="AsVectorSpace">
##  <ManSection>
##  <Oper Name="AsVectorSpace" Arg='F, D'/>
##
##  <Description>
##  Let <A>F</A> be a division ring and <A>D</A> a domain.
##  If the elements in <A>D</A> form an <A>F</A>-vector space then
##  <Ref Oper="AsVectorSpace"/> returns this <A>F</A>-vector space,
##  otherwise <K>fail</K> is returned.
##  <P/>
##  <Ref Oper="AsVectorSpace"/> can be used for example to view a given
##  vector space as a vector space over a smaller or larger division ring.
##  <Example><![CDATA[
##  gap> V:= FullRowSpace( GF( 27 ), 3 );
##  ( GF(3^3)^3 )
##  gap> Dimension( V );  LeftActingDomain( V );
##  3
##  GF(3^3)
##  gap> W:= AsVectorSpace( GF( 3 ), V );
##  <vector space over GF(3), with 9 generators>
##  gap> Dimension( W );  LeftActingDomain( W );
##  9
##  GF(3)
##  gap> AsVectorSpace( GF( 9 ), V );
##  fail
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "AsVectorSpace", AsLeftModule );


#############################################################################
##
#O  AsSubspace( <V>, <U> )  . . . . . . . . . . . view <U> as subspace of <V>
##
##  <#GAPDoc Label="AsSubspace">
##  <ManSection>
##  <Oper Name="AsSubspace" Arg='V, U'/>
##
##  <Description>
##  Let <A>V</A> be an <M>F</M>-vector space, and <A>U</A> a collection.
##  If <A>U</A> is a subset of <A>V</A> such that the elements of <A>U</A>
##  form an <M>F</M>-vector space then <Ref Oper="AsSubspace"/> returns this
##  vector space, with parent set to <A>V</A>
##  (see&nbsp;<Ref Func="AsVectorSpace"/>).
##  Otherwise <K>fail</K> is returned.
##  <Example><![CDATA[
##  gap> V:= VectorSpace( Rationals, [ [ 1, 2, 3 ], [ 1, 1, 1 ] ] );;
##  gap> W:= VectorSpace( Rationals, [ [ 1/2, 1/2, 1/2 ] ] );;
##  gap> U:= AsSubspace( V, W );
##  <vector space over Rationals, with 1 generators>
##  gap> Parent( U ) = V;
##  true
##  gap> AsSubspace( V, [ [ 1, 1, 1 ] ] );
##  fail
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareOperation( "AsSubspace", [ IsVectorSpace, IsCollection ] );


#############################################################################
##
#F  Intersection2Spaces( <AsStruct>, <Substruct>, <Struct> )
##
##  <ManSection>
##  <Func Name="Intersection2Spaces" Arg='AsStruct, Substruct, Struct'/>
##
##  <Description>
##  is a function that takes two arguments <A>V</A> and <A>W</A> which must
##  be finite dimensional vector spaces,
##  and returns the intersection of <A>V</A> and <A>W</A>.
##  <P/>
##  If the left acting domains are different then let <M>F</M> be their
##  intersection.
##  The intersection of <A>V</A> and <A>W</A> is computed as intersection of
##  <C><A>AsStruct</A>( <A>F</A>, <A>V</A> )</C> and
##  <C><A>AsStruct</A>( <A>F</A>, <A>V</A> )</C>.
##  <P/>
##  If the left acting domains are equal to <M>F</M> then the intersection of
##  <A>V</A> and <A>W</A> is returned either as <M>F</M>-<A>Substruct</A>
##  with the common parent of <A>V</A> and <A>W</A> or as
##  <M>F</M>-<A>Struct</A>, in both cases with known basis.
##  <P/>
##  This function is used to handle the intersections of two vector spaces,
##  two algebras, two algebras-with-one, two left ideals, two right ideals,
##  two two-sided ideals.
##  </Description>
##  </ManSection>
##
DeclareGlobalFunction( "Intersection2Spaces" );


#############################################################################
##
#F  FullRowSpace( <F>, <n> )
##
##  <#GAPDoc Label="FullRowSpace">
##  <ManSection>
##  <Func Name="FullRowSpace" Arg='F, n'/>
##  <Meth Name="\^" Arg='F, n' Label="for a field and an integer"/>
##
##  <Description>
##  For a field <A>F</A> and a nonnegative integer <A>n</A>,
##  <Ref Func="FullRowSpace"/> returns the <A>F</A>-vector space that
##  consists of all row vectors (see&nbsp;<Ref Func="IsRowVector"/>) of
##  length <A>n</A> with entries in <A>F</A>.
##  <P/>
##  An alternative to construct this vector space is via
##  <A>F</A><C>^</C><A>n</A>.
##  <Example><![CDATA[
##  gap> FullRowSpace( GF( 9 ), 3 );
##  ( GF(3^2)^3 )
##  gap> GF(9)^3;           # the same as above
##  ( GF(3^2)^3 )
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "FullRowSpace", FullRowModule );
DeclareSynonym( "RowSpace", FullRowModule );


#############################################################################
##
#F  FullMatrixSpace( <F>, <m>, <n> )
##
##  <#GAPDoc Label="FullMatrixSpace">
##  <ManSection>
##  <Func Name="FullMatrixSpace" Arg='F, m, n'/>
##  <Meth Name="\^" Arg='F, dims'
##   Label="for a field and a pair of integers"/>
##
##  <Description>
##  For a field <A>F</A> and two positive integers <A>m</A> and <A>n</A>,
##  <Ref Func="FullMatrixSpace"/> returns the <A>F</A>-vector space that
##  consists of all <A>m</A> by <A>n</A> matrices
##  (see&nbsp;<Ref Func="IsMatrix"/>) with entries in <A>F</A>.
##  <P/>
##  If <A>m</A><C> = </C><A>n</A> then the result is in fact an algebra
##  (see&nbsp;<Ref Func="FullMatrixAlgebra"/>).
##  <P/>
##  An alternative to construct this vector space is via
##  <A>F</A><C>^[</C><A>m</A>,<A>n</A><C>]</C>.
##  <Example><![CDATA[
##  gap> FullMatrixSpace( GF(2), 4, 5 );
##  ( GF(2)^[ 4, 5 ] )
##  gap> GF(2)^[ 4, 5 ];    # the same as above
##  ( GF(2)^[ 4, 5 ] )
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareSynonym( "FullMatrixSpace", FullMatrixModule );
DeclareSynonym( "MatrixSpace", FullMatrixModule );
DeclareSynonym( "MatSpace", FullMatrixModule );


#############################################################################
##
#C  IsSubspacesVectorSpace( <D> )
##
##  <#GAPDoc Label="IsSubspacesVectorSpace">
##  <ManSection>
##  <Filt Name="IsSubspacesVectorSpace" Arg='D' Type='Category'/>
##
##  <Description>
##  The domain of all subspaces of a (finite) vector space or of all
##  subspaces of fixed dimension, as returned by <Ref Func="Subspaces"/>
##  (see&nbsp;<Ref Func="Subspaces"/>) lies in the category
##  <Ref Filt="IsSubspacesVectorSpace"/>.
##  <Example><![CDATA[
##  gap> D:= Subspaces( GF(3)^3 );
##  Subspaces( ( GF(3)^3 ) )
##  gap> Size( D );
##  28
##  gap> iter:= Iterator( D );;
##  gap> NextIterator( iter );
##  <vector space over GF(3), with 0 generators>
##  gap> NextIterator( iter );
##  <vector space of dimension 1 over GF(3)>
##  gap> IsSubspacesVectorSpace( D );
##  true
##  ]]></Example>
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareCategory( "IsSubspacesVectorSpace", IsDomain );


#############################################################################
##
#M  IsFinite( <D> ) . . . . . . . . . . . . . . . . .  for a subspaces domain
##
##  Returns `true' if <D> is finite.
##  We allow subspaces domains in `IsSubspacesVectorSpace' only for finite
##  vector spaces.
##
InstallTrueMethod( IsFinite, IsSubspacesVectorSpace );


#############################################################################
##
#A  Subspaces( <V>[, <k>] )
##
##  <#GAPDoc Label="Subspaces">
##  <ManSection>
##  <Attr Name="Subspaces" Arg='V[, k]'/>
##
##  <Description>
##  Called with a finite vector space <A>v</A>,
##  <Ref Oper="Subspaces"/> returns the domain of all subspaces of <A>V</A>.
##  <P/>
##  Called with <A>V</A> and a nonnegative integer <A>k</A>,
##  <Ref Oper="Subspaces"/> returns the domain of all <A>k</A>-dimensional
##  subspaces of <A>V</A>.
##  <P/>
##  Special <Ref Attr="Size"/> and <Ref Oper="Iterator"/> methods are
##  provided for these domains.
##  <!-- <C>Enumerator</C> would also be good ...
##       (special treatment for full row spaces,
##       other spaces delegate to this)-->
##  </Description>
##  </ManSection>
##  <#/GAPDoc>
##
DeclareAttribute( "Subspaces", IsLeftModule );
DeclareOperation( "Subspaces", [ IsLeftModule, IsInt ] );


#############################################################################
##
#F  IsSubspace( <V>, <U> )
##
##  <ManSection>
##  <Func Name="IsSubspace" Arg='V, U'/>
##
##  <Description>
##  check that <A>U</A> is a vector space that is contained in <A>V</A>
##  <!-- Must also <A>V</A> be a vector space?
##       If yes then must <A>V</A> and <A>U</A> have same left acting domain?
##       (Is this function useful at all?) -->
##  </Description>
##  </ManSection>
##
DeclareGlobalFunction( "IsSubspace" );


#############################################################################
##
#A  OrthogonalSpaceInFullRowSpace( <U> )
##
##  <ManSection>
##  <Attr Name="OrthogonalSpaceInFullRowSpace" Arg='U'/>
##
##  <Description>
##  For a Gaussian row space <A>U</A> over <M>F</M>,
##  <Ref Attr="OrthogonalSpaceInFullRowSpace"/>
##  returns a complement of <A>U</A> in the full row space of same vector
##  dimension as <A>U</A> over <M>F</M>.
##  </Description>
##  </ManSection>
##
DeclareAttribute( "OrthogonalSpaceInFullRowSpace", IsGaussianSpace );


#############################################################################
##
#P  IsVectorSpaceHomomorphism( <map> )
##
##  <ManSection>
##  <Prop Name="IsVectorSpaceHomomorphism" Arg='map'/>
##
##  <Description>
##  A mapping <M>f</M> is a vector space homomorphism (or linear mapping)
##  if the source and range are vector spaces
##  (see&nbsp;<Ref Func="IsVectorSpace"/>)
##  over the same division ring <M>D</M>
##  (see&nbsp;<Ref Func="LeftActingDomain"/>),
##  and if <M>f( a + b ) = f(a) + f(b)</M> and <M>f( s * a ) = s * f(a)</M>
##  hold for all elements <M>a</M>, <M>b</M> in the source of <M>f</M> and
##  <M>s \in D</M>.
##  </Description>
##  </ManSection>
##
DeclareProperty( "IsVectorSpaceHomomorphism", IsGeneralMapping );


#############################################################################
##
#E

