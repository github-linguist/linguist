#############################################################################
##
#W  example.gd
##
##  This file contains a sample of a GAP declaration file.
##
DeclareProperty( "SomeProperty", IsLeftModule );
DeclareGlobalFunction( "SomeGlobalFunction" );


#############################################################################
##
#C  IsQuuxFrobnicator(<R>)
##
##  <ManSection>
##  <Filt Name="IsQuuxFrobnicator" Arg='R' Type='Category'/>
##
##  <Description>
##  Tests whether R is a quux frobnicator.
##  </Description>
##  </ManSection>
##
DeclareSynonym( "IsQuuxFrobnicator", IsField and IsGroup );
