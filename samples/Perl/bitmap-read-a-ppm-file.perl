#! /usr/bin/perl

use strict;
use Image::Imlib2;

my $img = Image::Imlib2->load("out0.ppm");

# let's do something with it now
$img->set_color(255, 255, 255, 255);
$img->draw_line(0,0, $img->width,$img->height);
$img->image_set_format("png");
$img->save("out1.png");

exit 0;
