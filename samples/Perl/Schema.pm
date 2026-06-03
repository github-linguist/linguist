package Color::Palette::Schema 0.100004;
use Moose;
# ABSTRACT: requirements for a palette

use Color::Palette;
use Color::Palette::Types qw(ColorName);
use MooseX::Types::Moose qw(ArrayRef);

#pod =head1 DESCRIPTION
#pod
#pod Most of this is documented in L<Color::Palette>.  Below is just a bit more
#pod documentation.
#pod
#pod =attr required_colors
#pod
#pod This is an arrayref of color names that must be present in any palette checked
#pod against this schema.
#pod
#pod =cut

has required_colors => (
  is  => 'ro',
  isa => ArrayRef[ ColorName ],
  required => 1,
);

#pod =method check
#pod
#pod   $schema->check($palette);
#pod
#pod This method will throw an exception if the given palette doesn't meet the
#pod requirements of the schema.
#pod
#pod =cut

sub check {
  my ($self, $palette) = @_;

  # ->color will throw an exception on unknown colors, doing our job for us.
  # -- rjbs, 2009-05-19
  $palette->color($_) for @{ $self->required_colors };
}

1;

__END__

=pod

=encoding UTF-8

=head1 NAME

Color::Palette::Schema - requirements for a palette

=head1 VERSION

version 0.100004

=head1 DESCRIPTION

Most of this is documented in L<Color::Palette>.  Below is just a bit more
documentation.

=head1 PERL VERSION

This library should run on perls released even a long time ago.  It should work
on any version of perl released in the last five years.

Although it may work on older versions of perl, no guarantee is made that the
minimum required version will not be increased.  The version may be increased
for any reason, and there is no promise that patches will be accepted to lower
the minimum required perl.

=head1 ATTRIBUTES

=head2 required_colors

This is an arrayref of color names that must be present in any palette checked
against this schema.

=head1 METHODS

=head2 check

  $schema->check($palette);

This method will throw an exception if the given palette doesn't meet the
requirements of the schema.

=head1 AUTHOR

Ricardo SIGNES <cpan@semiotic.systems>

=head1 COPYRIGHT AND LICENSE

This software is copyright (c) 2022 by Ricardo SIGNES.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=cut
