#! /usr/bin/perl

use strict;
use Image::Imlib2;

sub tograyscale
{
    my $img = shift;
    my $gimg = Image::Imlib2->new($img->width, $img->height);
    for ( my $x = 0; $x < $gimg->width; $x++ ) {
	for ( my $y = 0; $y < $gimg->height; $y++ ) {
	    my ( $r, $g, $b, $a ) = $img->query_pixel($x, $y);
	    my $gray = int(0.2126 * $r + 0.7152 * $g + 0.0722 * $b);
	    # discard alpha info...
	    $gimg->set_color($gray, $gray, $gray, 255);
	    $gimg->draw_point($x, $y);
	}
    }
    return $gimg;
}

my $animage = Image::Imlib2->load("Lenna100.jpg");
my $gscale = tograyscale($animage);
$gscale->set_quality(80);
$gscale->save("Lennagray.jpg");

exit 0;
