#! /usr/bin/perl

use strict;

use Image::Imlib2;

# create the "canvas"
my $img = Image::Imlib2->new(200,200);

# fill with a plain RGB(A) color
$img->set_color(255, 0, 0, 255);
$img->fill_rectangle(0,0, 200, 200);

# set a pixel to green (at 40,40)
$img->set_color(0, 255, 0, 255);
$img->draw_point(40,40);

# "get" pixel rgb(a)
my ($red, $green, $blue, $alpha) = $img->query_pixel(40,40);
undef $img;

# another way of creating a canvas with a bg colour (or from
# an existing "raw" data)
my $col = pack("CCCC", 255, 255, 0, 0); # a, r, g, b
my $img = Image::Imlib2->new_using_data(200, 200, $col x (200 * 200));

exit 0;
