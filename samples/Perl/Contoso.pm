# Comment up front.
# Fake copyright notice, etc.
# ...
package Contoso::Widget;
use Moose;

has name => (
    is => 'ro',
);

__PACKAGE__->meta->make_immutable;
1;
