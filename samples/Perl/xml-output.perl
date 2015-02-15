#! /usr/bin/perl
use strict;
use XML::Mini::Document;

my @students = ( [ "April", "Bubbly: I'm > Tam and <= Emily" ],
                 [ "Tam O'Shanter", "Burns: \"When chapman billies leave the street ...\"" ],
		 [ "Emily", "Short & shrift" ]
                );

my $doc = XML::Mini::Document->new();
my $root = $doc->getRoot();
my $studs = $root->createChild("CharacterRemarks");
foreach my $s (@students)
{
    my $stud = $studs->createChild("Character");
    $stud->attribute("name", $s->[0]);
    $stud->text($s->[1]);
}
print $doc->toString();
