#!perl
use Test::More;
use Test::Exception;

use_ok 'Music::ScaleNote';

my $msn = Music::ScaleNote->new(
    scale_note => 'C',
    scale_name => 'pminor',
#    verbose    => 1,
);
isa_ok $msn, 'Music::ScaleNote';

my $x;

throws_ok { $x = $msn->get_offset() }
    qr/note_name, note_format or offset not provided/, 'invalid get_offset';

my $format = 'midinum';
$x = $msn->get_offset(
    note_name   => 60,
    note_format => $format,
    offset      => 1,
);
is $x->format($format), 63, 'get_offset';

$format = 'ISO';
$x = $msn->get_offset(
    note_name   => 'D#4',
    note_format => $format,
    offset      => -1,
);
is $x->format($format), 'C4', 'get_offset';

throws_ok {
    $x = $msn->get_offset(
        note_name   => 'C0',
        note_format => $format,
        offset      => -1,
    )
} qr/Octave: -1 out of bounds/, 'out of bounds';

throws_ok {
    $x = $msn->get_offset(
        note_name   => 'A#127',
        note_format => $format,
        offset      => 1,
    )
} qr/Octave: 128 out of bounds/, 'out of bounds';

done_testing();
