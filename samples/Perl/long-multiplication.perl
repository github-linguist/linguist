#!/usr/bin/perl -w
use strict;

# This should probably be done in a loop rather than be recursive.
sub add_with_carry
{
  my $resultref = shift;
  my $addend = shift;
  my $addendpos = shift;

  push @$resultref, (0) while (scalar @$resultref < $addendpos + 1);
  my $addend_result = $addend + $resultref->[$addendpos];
  my @addend_digits = reverse split //, $addend_result;
  $resultref->[$addendpos] = shift @addend_digits;

  my $carry_digit = shift @addend_digits;
  &add_with_carry($resultref, $carry_digit, $addendpos + 1)
    if( defined $carry_digit )
}

sub longhand_multiplication
{
  my @multiplicand = reverse split //, shift;
  my @multiplier = reverse split //, shift;
  my @result = ();
  my $multiplicand_offset = 0;
  foreach my $multiplicand_digit (@multiplicand)
  {
    my $multiplier_offset = $multiplicand_offset;
    foreach my $multiplier_digit (@multiplier)
    {
      my $multiplication_result = $multiplicand_digit * $multiplier_digit;
      my @result_digit_addend_list = reverse split //, $multiplication_result;

      my $addend_offset = $multiplier_offset;
      foreach my $result_digit_addend (@result_digit_addend_list)
      {
        &add_with_carry(\@result, $result_digit_addend, $addend_offset++)
      }

      ++$multiplier_offset;
    }

    ++$multiplicand_offset;
  }

  @result = reverse @result;

  return join '', @result;
}

my $sixtyfour = "18446744073709551616";

my $onetwentyeight = &longhand_multiplication($sixtyfour, $sixtyfour);
print "$onetwentyeight\n";
