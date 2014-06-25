''***************************************
''*  Graphics Driver v1.0               *
''*  Author: Chip Gracey                *
''*  Copyright (c) 2005 Parallax, Inc.  *               
''*  See end of file for terms of use.  *               
''***************************************

''
'' Theory of Operation:
''
'' A cog is launched which processes commands via the PUB routines.
''
'' Points, lines, arcs, sprites, text, and polygons are rasterized into
'' a specified stretch of memory which serves as a generic bitmap buffer.
''
'' The bitmap can be displayed by the TV.SRC or VGA.SRC driver.
''
'' See GRAPHICS_DEMO.SRC for usage example.
''

CON

  #1, _setup, _color, _width, _plot, _line, _arc, _vec, _vecarc, _pix, _pixarc, _text, _textarc, _textmode, _fill, _loop

VAR

  long  cog

  long  command

  long  bitmap_base                                     'bitmap data
  long  bitmap_longs
  word  bases[32]

  long  pixel_width                                     'pixel data
  long  slices[8]

  long  text_xs, text_ys, text_sp, text_just            'text data (these 4 must be contiguous)


PUB start : okay

'' Start graphics driver - starts a cog
'' returns false if no cog available

  fontptr := @font                                      'set font pointer (same for all instances)

  stop
  okay := cog := cognew(@loop, @command) + 1


PUB stop

'' Stop graphics driver - frees a cog

  if cog
    cogstop(cog~ - 1)

  command~


PUB setup(x_tiles, y_tiles, x_origin, y_origin, base_ptr) | bases_ptr, slices_ptr

'' Set bitmap parameters
''
''   x_tiles        - number of x tiles (tiles are 16x16 pixels each)
''   y_tiles        - number of y tiles
''   x_origin       - relative-x center pixel
''   y_origin       - relative-y center pixel
''   base_ptr       - base address of bitmap

  setcommand(_loop, 0)                                  'make sure last command finished

  repeat bases_ptr from 0 to x_tiles - 1 <# 31          'write bases
    bases[bases_ptr] := base_ptr + bases_ptr * y_tiles << 6

  y_tiles <<= 4                                         'adjust arguments and do setup command
  y_origin := y_tiles - y_origin - 1
  bases_ptr := @bases
  slices_ptr := @slices
  setcommand(_setup, @x_tiles)

  bitmap_base := base_ptr                               'retain high-level bitmap data
  bitmap_longs := x_tiles * y_tiles


PUB clear

'' Clear bitmap

  setcommand(_loop, 0)                                  'make sure last command finished

  longfill(bitmap_base, 0, bitmap_longs)                'clear bitmap


PUB copy(dest_ptr)

'' Copy bitmap
'' use for double-buffered display (flicker-free)
''
''   dest_ptr       - base address of destination bitmap

  setcommand(_loop, 0)                                  'make sure last command finished

  longmove(dest_ptr, bitmap_base, bitmap_longs)         'copy bitmap


PUB color(c)

'' Set pixel color to two-bit pattern
''
''   c              - color code in bits[1..0]

  setcommand(_color, @colors[c & 3])                    'set color


PUB width(w) | pixel_passes, r, i, p

'' Set pixel width
'' actual width is w[3..0] + 1
''
''   w              - 0..15 for round pixels, 16..31 for square pixels

  r := not w & $10                                      'determine pixel shape/width
  w &= $F
  pixel_width := w
  pixel_passes := w >> 1 + 1

  setcommand(_width, @w)                                'do width command now to avoid updating slices when busy

  p := w ^ $F                                           'update slices to new shape/width
  repeat i from 0 to w >> 1
    slices[i] := true >> (p << 1) << (p & $E)
    if r and pixels[w] & |< i
      p += 2
    if r and i == pixel_passes - 2
      p += 2


PUB colorwidth(c, w)

'' Set pixel color and width

  color(c)
  width(w)


PUB plot(x, y)

'' Plot point
''
''   x,y            - point

  setcommand(_plot, @x)


PUB line(x, y)

'' Draw a line to point
''
''   x,y            - endpoint

  setcommand(_line, @x)


PUB arc(x, y, xr, yr, angle, anglestep, steps, arcmode)

'' Draw an arc
''
''   x,y            - center of arc
''   xr,yr          - radii of arc
''   angle          - initial angle in bits[12..0] (0..$1FFF = 0°..359.956°)
''   anglestep      - angle step in bits[12..0]
''   steps          - number of steps (0 just leaves (x,y) at initial arc position)
''   arcmode        - 0: plot point(s)
''                    1: line to point(s)
''                    2: line between points
''                    3: line from point(s) to center

  setcommand(_arc, @x)


PUB vec(x, y, vecscale, vecangle, vecdef_ptr)

'' Draw a vector sprite
''
''   x,y            - center of vector sprite
''   vecscale       - scale of vector sprite ($100 = 1x)
''   vecangle       - rotation angle of vector sprite in bits[12..0]
''   vecdef_ptr     - address of vector sprite definition
''
''
'' Vector sprite definition:
''
''    word    $8000|$4000+angle       'vector mode + 13-bit angle (mode: $4000=plot, $8000=line)
''    word    length                  'vector length
''    ...                             'more vectors
''    ...
''    word    0                       'end of definition

  setcommand(_vec, @x)


PUB vecarc(x, y, xr, yr, angle, vecscale, vecangle, vecdef_ptr)

'' Draw a vector sprite at an arc position
''
''   x,y            - center of arc
''   xr,yr          - radii of arc
''   angle          - angle in bits[12..0] (0..$1FFF = 0°..359.956°)
''   vecscale       - scale of vector sprite ($100 = 1x)
''   vecangle       - rotation angle of vector sprite in bits[12..0]
''   vecdef_ptr     - address of vector sprite definition

  setcommand(_vecarc, @x)


PUB pix(x, y, pixrot, pixdef_ptr)

'' Draw a pixel sprite
''
''   x,y            - center of vector sprite
''   pixrot         - 0: 0°, 1: 90°, 2: 180°, 3: 270°, +4: mirror
''   pixdef_ptr     - address of pixel sprite definition
''
''
'' Pixel sprite definition:
''
''    word                            'word align, express dimensions and center, define pixels
''    byte    xwords, ywords, xorigin, yorigin
''    word    %%xxxxxxxx,%%xxxxxxxx
''    word    %%xxxxxxxx,%%xxxxxxxx
''    word    %%xxxxxxxx,%%xxxxxxxx
''    ...

  setcommand(_pix, @x)


PUB pixarc(x, y, xr, yr, angle, pixrot, pixdef_ptr)

'' Draw a pixel sprite at an arc position
''
''   x,y            - center of arc
''   xr,yr          - radii of arc
''   angle          - angle in bits[12..0] (0..$1FFF = 0°..359.956°)
''   pixrot         - 0: 0°, 1: 90°, 2: 180°, 3: 270°, +4: mirror
''   pixdef_ptr     - address of pixel sprite definition

  setcommand(_pixarc, @x)


PUB text(x, y, string_ptr) | justx, justy

'' Draw text
''
''   x,y            - text position (see textmode for sizing and justification)
''   string_ptr     - address of zero-terminated string (it may be necessary to call .finish
''                    immediately afterwards to prevent subsequent code from clobbering the
''                    string as it is being drawn

  justify(string_ptr, @justx)                           'justify string and draw text
  setcommand(_text, @x)


PUB textarc(x, y, xr, yr, angle, string_ptr) | justx, justy

'' Draw text at an arc position
''
''   x,y            - center of arc
''   xr,yr          - radii of arc
''   angle          - angle in bits[12..0] (0..$1FFF = 0°..359.956°)
''   string_ptr     - address of zero-terminated string (it may be necessary to call .finish
''                    immediately afterwards to prevent subsequent code from clobbering the
''                    string as it is being drawn

  justify(string_ptr, @justx)                           'justify string and draw text
  setcommand(_textarc, @x)


PUB textmode(x_scale, y_scale, spacing, justification)

'' Set text size and justification
''
''   x_scale        - x character scale, should be 1+
''   y_scale        - y character scale, should be 1+
''   spacing        - character spacing, 6 is normal
''   justification  - bits[1..0]: 0..3 = left, center, right, left
''                    bits[3..2]: 0..3 = bottom, center, top, bottom

  longmove(@text_xs, @x_scale, 4)                       'retain high-level text data
 
  setcommand(_textmode, @x_scale)                       'set text mode


PUB box(x, y, box_width, box_height) | x2, y2, pmin, pmax 

'' Draw a box with round/square corners, according to pixel width
''
''   x,y            - box left, box bottom

  if box_width > pixel_width and box_height > pixel_width

    pmax := pixel_width - (pmin := pixel_width >> 1)    'get pixel-half-min and pixel-half-max

    x += pmin                                           'adjust coordinates to accomodate width
    y += pmin
    x2 := x + box_width - 1 - pixel_width
    y2 := y + box_height - 1 - pixel_width

    plot(x, y)                                          'plot round/square corners
    plot(x, y2)
    plot(x2, y)
    plot(x2, y2)

    fill(x, y2 + pmax, 0, (x2 - x) << 16, 0, 0, pmax)   'fill gaps
    fill(x, y, 0, (x2 - x) << 16, 0, 0, pmin)
    fill(x - pmin, y2, 0, (x2 - x + pixel_width) << 16, 0, 0, y2 - y)


PUB quad(x1, y1, x2, y2, x3, y3, x4, y4)

'' Draw a solid quadrilateral
'' vertices must be ordered clockwise or counter-clockwise

  tri(x1, y1, x2, y2, x3, y3)                           'draw two triangle to make 4-sides polygon
  tri(x3, y3, x4, y4, x1, y1)


PUB tri(x1, y1, x2, y2, x3, y3) | xy[2]

'' Draw a solid triangle

' reorder vertices by descending y

  case (y1 => y2) & %100 | (y2 => y3) & %010 | (y1 => y3) & %001
    %000:
      longmove(@xy, @x1, 2)
      longmove(@x1, @x3, 2)
      longmove(@x3, @xy, 2)
    %010:
      longmove(@xy, @x1, 2)
      longmove(@x1, @x2, 4)
      longmove(@x3, @xy, 2)
    %011:
      longmove(@xy, @x1, 2)
      longmove(@x1, @x2, 2)
      longmove(@x2, @xy, 2)
    %100:
      longmove(@xy, @x3, 2)
      longmove(@x2, @x1, 4)
      longmove(@x1, @xy, 2)
    %101:
      longmove(@xy, @x2, 2)
      longmove(@x2, @x3, 2)
      longmove(@x3, @xy, 2)

' draw triangle

  fill(x1, y1, (x3 - x1) << 16 / (y1 - y3 + 1), (x2 - x1) << 16 / (y1 - y2 + 1), (x3 - x2) << 16 / (y2 - y3 + 1), y1 - y2, y1 - y3)


PUB finish

'' Wait for any current graphics command to finish
'' use this to insure that it is safe to manually manipulate the bitmap

  setcommand(_loop, 0)                                  'make sure last command finished


PRI fill(x, y, da, db, db2, linechange, lines_minus_1)

  setcommand(_fill, @x)


PRI justify(string_ptr, justptr) | x

  x := (strsize(string_ptr) - 1) * text_xs * text_sp + text_xs * 5 - 1
  long[justptr] := -lookupz(text_just >> 2 & 3: 0, x >> 1, x, 0)
  long[justptr][1] := -lookupz(text_just & 3: 0, text_ys << 3, text_ys << 4, 0)


PRI setcommand(cmd, argptr)

  command := cmd << 16 + argptr                         'write command and pointer
  repeat while command                                  'wait for command to be cleared, signifying receipt


CON

  ' Vector font primitives

  xa0   = %000 << 0             'x line start / arc center
  xa1   = %001 << 0
  xa2   = %010 << 0
  xa3   = %011 << 0
  xa4   = %100 << 0
  xa5   = %101 << 0
  xa6   = %110 << 0
  xa7   = %111 << 0

  ya0   = %0000 << 3            'y line start / arc center
  ya1   = %0001 << 3
  ya2   = %0010 << 3
  ya3   = %0011 << 3
  ya4   = %0100 << 3
  ya5   = %0101 << 3
  ya6   = %0110 << 3
  ya7   = %0111 << 3
  ya8   = %1000 << 3
  ya9   = %1001 << 3
  yaA   = %1010 << 3
  yaB   = %1011 << 3
  yaC   = %1100 << 3
  yaD   = %1101 << 3
  yaE   = %1110 << 3
  yaF   = %1111 << 3

  xb0   = %000 << 7             'x line end
  xb1   = %001 << 7
  xb2   = %010 << 7
  xb3   = %011 << 7
  xb4   = %100 << 7
  xb5   = %101 << 7
  xb6   = %110 << 7
  xb7   = %111 << 7

  yb0   = %0000 << 10           'y line end
  yb1   = %0001 << 10
  yb2   = %0010 << 10
  yb3   = %0011 << 10
  yb4   = %0100 << 10
  yb5   = %0101 << 10
  yb6   = %0110 << 10
  yb7   = %0111 << 10
  yb8   = %1000 << 10
  yb9   = %1001 << 10
  ybA   = %1010 << 10
  ybB   = %1011 << 10
  ybC   = %1100 << 10
  ybD   = %1101 << 10
  ybE   = %1110 << 10
  ybF   = %1111 << 10

  ax1   = %0 << 7               'x arc radius
  ax2   = %1 << 7

  ay1   = %00 << 8              'y arc radius
  ay2   = %01 << 8
  ay3   = %10 << 8
  ay4   = %11 << 8

  a0    = %0000 << 10           'arc start/length
  a1    = %0001 << 10           'bits[1..0] = start (0..3 = 0°, 90°, 180°, 270°)
  a2    = %0010 << 10           'bits[3..2] = length (0..3 = 360°, 270°, 180°, 90°)
  a3    = %0011 << 10
  a4    = %0100 << 10
  a5    = %0101 << 10
  a6    = %0110 << 10
  a7    = %0111 << 10
  a8    = %1000 << 10
  a9    = %1001 << 10
  aA    = %1010 << 10
  aB    = %1011 << 10
  aC    = %1100 << 10
  aD    = %1101 << 10
  aE    = %1110 << 10
  aF    = %1111 << 10

  fline = %0 << 14              'line command
  farc  = %1 << 14              'arc command

  more  = %1 << 15              'another arc/line


DAT

' Color codes

colors  long    %%0000000000000000
        long    %%1111111111111111
        long    %%2222222222222222
        long    %%3333333333333333

' Round pixel recipes

pixels  byte    %00000000,%00000000,%00000000,%00000000         '0,1,2,3
        byte    %00000000,%00000000,%00000010,%00000101         '4,5,6,7
        byte    %00001010,%00001010,%00011010,%00011010         '8,9,A,B
        byte    %00110100,%00111010,%01110100,%01110100         'C,D,E,F

' Vector font - standard ascii characters ($21-$7E)

font    word    fline + xa2 + yaC + xb2 + yb7 + more            '!
        word    fline + xa2 + ya5 + xb2 + yb4

        word    fline + xa1 + yaD + xb1 + ybC + more            '"
        word    fline + xa3 + yaD + xb3 + ybC

        word    fline + xa1 + yaA + xb1 + yb6 + more            '#
        word    fline + xa3 + yaA + xb3 + yb6 + more
        word    fline + xa0 + ya9 + xb4 + yb9 + more
        word    fline + xa0 + ya7 + xb4 + yb7

        word    farc + xa2 + ya9 + a9 + ax2 + ay1 + more        '$
        word    farc + xa2 + ya7 + aB + ax2 + ay1 + more
        word    fline + xa0 + ya6 + xb2 + yb6 + more
        word    fline + xa2 + yaA + xb4 + ybA + more
        word    fline + xa2 + yaA + xb2 + ybB + more
        word    fline + xa2 + ya6 + xb2 + yb5

        word    farc + xa1 + yaA + a0 + ax1 + ay1 + more        '%
        word    farc + xa3 + ya6 + a0 + ax1 + ay1 + more
        word    fline + xa0 + ya6 + xb4 + ybA

        word    farc + xa2 + yaA + a7 + ax1 + ay1 + more        '&
        word    farc + xa2 + ya7 + a5 + ax2 + ay2 + more
        word    fline + xa1 + yaA + xb4 + yb5

        word    fline + xa2 + yaD + xb2 + ybC                   ' '

        word    farc + xa3 + ya9 + aD + ax1 + ay4 + more        '(
        word    farc + xa3 + ya7 + aE + ax1 + ay4 + more
        word    fline + xa2 + ya7 + xb2 + yb9

        word    farc + xa1 + ya9 + aC + ax1 + ay4 + more        ')
        word    farc + xa1 + ya7 + aF + ax1 + ay4 + more
        word    fline + xa2 + ya7 + xb2 + yb9

        word    fline + xa4 + ya6 + xb0 + ybA + more            '*
        word    fline + xa0 + ya6 + xb4 + ybA + more
        word    fline + xa2 + yaB + xb2 + yb5

        word    fline + xa0 + ya8 + xb4 + yb8 + more            '+
        word    fline + xa2 + yaA + xb2 + yb6

        word    fline + xa2 + ya4 + xb1 + yb3                   ',

        word    fline + xa0 + ya8 + xb4 + yb8                   '-

        word    fline + xa2 + ya5 + xb2 + yb4                   '.

        word    fline + xa0 + ya4 + xb4 + ybC                   '/

        word    farc + xa2 + ya8 + a0 + ax2 + ay4               '0

        word    fline + xa0 + ya4 + xb4 + yb4 + more            '1
        word    fline + xa2 + ya4 + xb2 + ybC + more
        word    fline + xa0 + yaA + xb2 + ybC

        word    farc + xa2 + yaA + a8 + ax2 + ay2 + more        '2
        word    farc + xa2 + yaA + aF + ax2 + ay3 + more
        word    farc + xa2 + ya4 + aD + ax2 + ay3 + more
        word    fline + xa0 + ya4 + xb4 + yb4

        word    farc + xa2 + yaA + a7 + ax2 + ay2 + more        '3
        word    farc + xa2 + ya6 + a6 + ax2 + ay2

        word    fline + xa2 + yaC + xb0 + yb7 + more            '4
        word    fline + xa0 + ya7 + xb4 + yb7 + more
        word    fline + xa3 + ya4 + xb3 + yb8

        word    farc + xa2 + ya6 + aB + ax2 + ay2 + more        '5
        word    fline + xa4 + yaC + xb0 + ybC + more
        word    fline + xa0 + yaC + xb0 + yb8 + more
        word    fline + xa0 + ya8 + xb2 + yb8 + more
        word    fline + xa0 + ya4 + xb2 + yb4

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        '6
        word    farc + xa2 + ya8 + aD + ax2 + ay4 + more
        word    fline + xa0 + ya6 + xb0 + yb8 + more
        word    fline + xa2 + yaC + xb3 + ybC

        word    fline + xa0 + yaC + xb4 + ybC + more            '7
        word    fline + xa1 + ya4 + xb4 + ybC

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        '8
        word    farc + xa2 + yaA + a0 + ax2 + ay2

        word    farc + xa2 + yaA + a0 + ax2 + ay2 + more        '9
        word    farc + xa2 + ya8 + aF + ax2 + ay4 + more
        word    fline + xa4 + ya8 + xb4 + ybA + more
        word    fline + xa1 + ya4 + xb2 + yb4

        word    fline + xa2 + ya6 + xb2 + yb7 + more            ':
        word    fline + xa2 + yaA + xb2 + yb9

        word    fline + xa2 + ya4 + xb1 + yb3 + more            ';
        word    fline + xa2 + ya8 + xb2 + yb7

        word    fline + xa0 + ya8 + xb4 + ybA + more            '<
        word    fline + xa0 + ya8 + xb4 + yb6

        word    fline + xa0 + yaA + xb4 + ybA + more            '=
        word    fline + xa0 + ya6 + xb4 + yb6

        word    fline + xa4 + ya8 + xb0 + ybA + more            '>
        word    fline + xa4 + ya8 + xb0 + yb6

        word    farc + xa2 + yaB + a8 + ax2 + ay1 + more        '?
        word    farc + xa3 + yaB + aF + ax1 + ay2 + more
        word    farc + xa3 + ya7 + aD + ax1 + ay2 + more
        word    fline + xa2 + ya5 + xb2 + yb4

        word    farc + xa2 + ya8 + a0 + ax1 + ay1 + more        '@
        word    farc + xa2 + ya8 + a4 + ax2 + ay3 + more
        word    farc + xa3 + ya8 + aF + ax1 + ay1 + more
        word    farc + xa2 + ya6 + aF + ax2 + ay1 + more
        word    fline + xa3 + ya7 + xb3 + yb9

        word    farc + xa2 + yaA + a8 + ax2 + ay2 + more        'A
        word    fline + xa0 + ya4 + xb0 + ybA + more
        word    fline + xa4 + ya4 + xb4 + ybA + more
        word    fline + xa0 + ya8 + xb4 + yb8

        word    farc + xa2 + yaA + aB + ax2 + ay2 + more        'B
        word    farc + xa2 + ya6 + aB + ax2 + ay2 + more
        word    fline + xa0 + ya4 + xb0 + ybC + more
        word    fline + xa0 + ya4 + xb2 + yb4 + more
        word    fline + xa0 + ya8 + xb2 + yb8 + more
        word    fline + xa0 + yaC + xb2 + ybC

        word    farc + xa2 + yaA + a8 + ax2 + ay2 + more        'C
        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more
        word    fline + xa0 + ya6 + xb0 + ybA

        word    farc + xa2 + yaA + aC + ax2 + ay2 + more        'D
        word    farc + xa2 + ya6 + aF + ax2 + ay2 + more
        word    fline + xa0 + ya4 + xb0 + ybC + more
        word    fline + xa4 + ya6 + xb4 + ybA + more
        word    fline + xa0 + ya4 + xb2 + yb4 + more
        word    fline + xa0 + yaC + xb2 + ybC

        word    fline + xa0 + ya4 + xb0 + ybC + more            'E
        word    fline + xa0 + ya4 + xb4 + yb4 + more
        word    fline + xa0 + ya8 + xb3 + yb8 + more
        word    fline + xa0 + yaC + xb4 + ybC

        word    fline + xa0 + ya4 + xb0 + ybC + more            'F
        word    fline + xa0 + ya8 + xb3 + yb8 + more
        word    fline + xa0 + yaC + xb4 + ybC

        word    farc + xa2 + yaA + a8 + ax2 + ay2 + more        'G
        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more
        word    fline + xa0 + ya6 + xb0 + ybA + more
        word    fline + xa4 + ya4 + xb4 + yb7 + more
        word    fline + xa3 + ya7 + xb4 + yb7

        word    fline + xa0 + ya4 + xb0 + ybC + more            'H
        word    fline + xa4 + ya4 + xb4 + ybC + more
        word    fline + xa0 + ya8 + xb4 + yb8

        word    fline + xa2 + ya4 + xb2 + ybC + more            'I
        word    fline + xa0 + ya4 + xb4 + yb4 + more
        word    fline + xa0 + yaC + xb4 + ybC

        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more        'J
        word    fline + xa4 + ya6 + xb4 + ybC

        word    fline + xa0 + ya4 + xb0 + ybC + more            'K
        word    fline + xa4 + yaC + xb0 + yb8 + more
        word    fline + xa4 + ya4 + xb0 + yb8

        word    fline + xa0 + ya4 + xb0 + ybC + more            'L
        word    fline + xa0 + ya4 + xb4 + yb4

        word    fline + xa0 + ya4 + xb0 + ybC + more            'M
        word    fline + xa4 + ya4 + xb4 + ybC + more
        word    fline + xa2 + ya8 + xb0 + ybC + more
        word    fline + xa2 + ya8 + xb4 + ybC

        word    fline + xa0 + ya4 + xb0 + ybC + more            'N
        word    fline + xa4 + ya4 + xb4 + ybC + more
        word    fline + xa4 + ya4 + xb0 + ybC

        word    farc + xa2 + yaA + a8 + ax2 + ay2 + more        '0
        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more
        word    fline + xa0 + ya6 + xb0 + ybA + more
        word    fline + xa4 + ya6 + xb4 + ybA

        word    farc + xa2 + yaA + aB + ax2 + ay2 + more        'P
        word    fline + xa0 + ya4 + xb0 + ybC + more
        word    fline + xa0 + ya8 + xb2 + yb8 + more
        word    fline + xa0 + yaC + xb2 + ybC

        word    farc + xa2 + yaA + a8 + ax2 + ay2 + more        'Q
        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more
        word    fline + xa0 + ya6 + xb0 + ybA + more
        word    fline + xa4 + ya6 + xb4 + ybA + more
        word    fline + xa2 + ya6 + xb4 + yb3

        word    farc + xa2 + yaA + aB + ax2 + ay2 + more        'R
        word    fline + xa0 + ya4 + xb0 + ybC + more
        word    fline + xa0 + ya8 + xb2 + yb8 + more
        word    fline + xa0 + yaC + xb2 + ybC + more
        word    fline + xa4 + ya4 + xb2 + yb8

        word    farc + xa2 + yaA + a4 + ax2 + ay2 + more        'S
        word    farc + xa2 + ya6 + a6 + ax2 + ay2

        word    fline + xa2 + ya4 + xb2 + ybC + more            'T
        word    fline + xa0 + yaC + xb4 + ybC

        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more        'U
        word    fline + xa0 + ya6 + xb0 + ybC + more
        word    fline + xa4 + ya6 + xb4 + ybC

        word    fline + xa2 + ya4 + xb0 + ybC + more            'V
        word    fline + xa2 + ya4 + xb4 + ybC

        word    fline + xa0 + yaC + xb0 + yb4 + more            'W
        word    fline + xa4 + yaC + xb4 + yb4 + more
        word    fline + xa2 + ya8 + xb0 + yb4 + more
        word    fline + xa2 + ya8 + xb4 + yb4

        word    fline + xa4 + ya4 + xb0 + ybC + more            'X
        word    fline + xa0 + ya4 + xb4 + ybC

        word    fline + xa0 + yaC + xb2 + yb8 + more            'Y
        word    fline + xa4 + yaC + xb2 + yb8 + more
        word    fline + xa2 + ya4 + xb2 + yb8

        word    fline + xa0 + yaC + xb4 + ybC + more            'Z
        word    fline + xa0 + ya4 + xb4 + ybC + more
        word    fline + xa0 + ya4 + xb4 + yb4

        word    fline + xa2 + yaD + xb2 + yb3 + more            '[
        word    fline + xa2 + yaD + xb4 + ybD + more
        word    fline + xa2 + ya3 + xb4 + yb3

        word    fline + xa4 + ya4 + xb0 + ybC                   '\

        word    fline + xa2 + yaD + xb2 + yb3 + more            '[
        word    fline + xa2 + yaD + xb0 + ybD + more
        word    fline + xa2 + ya3 + xb0 + yb3

        word    fline + xa2 + yaA + xb0 + yb6 + more            '^
        word    fline + xa2 + yaA + xb4 + yb6

        word    fline + xa0 + ya1 + xa4 + yb1                   '_

        word    fline + xa1 + ya9 + xb3 + yb7                   '`

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        'a
        word    fline + xa4 + ya4 + xb4 + yb8

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        'b
        word    fline + xa0 + ya4 + xb0 + ybC

        word    farc + xa2 + ya6 + a9 + ax2 + ay2 + more        'c
        word    fline + xa2 + ya4 + xb4 + yb4 + more
        word    fline + xa2 + ya8 + xb4 + yb8

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        'd
        word    fline + xa4 + ya4 + xb4 + ybC

        word    farc + xa2 + ya6 + a4 + ax2 + ay2 + more        'e
        word    fline + xa0 + ya6 + xb4 + yb6 + more
        word    fline + xa2 + ya4 + xb4 + yb4

        word    farc + xa4 + yaA + aD + ax2 + ay2 + more        'f
        word    fline + xa0 + ya8 + xb4 + yb8 + more
        word    fline + xa2 + ya4 + xb2 + ybA

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        'g
        word    farc + xa2 + ya3 + aF + ax2 + ay2 + more
        word    fline + xa4 + ya3 + xb4 + yb8 + more
        word    fline + xa1 + ya1 + xb2 + yb1

        word    farc + xa2 + ya6 + a8 + ax2 + ay2 + more        'h
        word    fline + xa0 + ya4 + xb0 + ybC + more
        word    fline + xa4 + ya4 + xb4 + yb6

        word    fline + xa1 + ya4 + xb3 + yb4 + more            'i
        word    fline + xa2 + ya4 + xb2 + yb8 + more
        word    fline + xa1 + ya8 + xb2 + yb8 + more
        word    fline + xa2 + yaB + xb2 + ybA

        word    farc + xa0 + ya3 + aF + ax2 + ay2 + more        'j
        word    fline + xa2 + ya3 + xb2 + yb8 + more
        word    fline + xa1 + ya8 + xb2 + yb8 + more
        word    fline + xa2 + yaB + xb2 + ybA

        word    fline + xa0 + ya4 + xb0 + ybC + more            'k
        word    fline + xa0 + ya6 + xb2 + yb6 + more
        word    fline + xa2 + ya6 + xb4 + yb8 + more
        word    fline + xa2 + ya6 + xb4 + yb4

        word    fline + xa1 + ya4 + xb3 + yb4 + more            'l
        word    fline + xa2 + ya4 + xb2 + ybC + more
        word    fline + xa1 + yaC + xb2 + ybC

        word    farc + xa1 + ya7 + a8 + ax1 + ay1 + more        'm
        word    farc + xa3 + ya7 + a8 + ax1 + ay1 + more
        word    fline + xa0 + ya4 + xb0 + yb8 + more
        word    fline + xa2 + ya4 + xb2 + yb7 + more
        word    fline + xa4 + ya4 + xb4 + yb7

        word    farc + xa2 + ya6 + a8 + ax2 + ay2 + more        'n
        word    fline + xa0 + ya4 + xb0 + yb8 + more
        word    fline + xa4 + ya4 + xb4 + yb6

        word    farc + xa2 + ya6 + a0 + ax2 + ay2               'o

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        'p
        word    fline + xa0 + ya1 + xb0 + yb8

        word    farc + xa2 + ya6 + a0 + ax2 + ay2 + more        'q
        word    fline + xa4 + ya1 + xb4 + yb8

        word    farc + xa2 + ya7 + a8 + ax2 + ay1 + more        'r
        word    fline + xa0 + ya4 + xb0 + yb8

        word    farc + xa2 + ya7 + a9 + ax2 + ay1 + more        's
        word    farc + xa2 + ya5 + aB + ax2 + ay1 + more
        word    fline + xa0 + ya4 + xb2 + yb4 + more
        word    fline + xa2 + ya8 + xb4 + yb8

        word    farc + xa4 + ya6 + aE + ax2 + ay2 + more        't
        word    fline + xa0 + ya8 + xb4 + yb8 + more
        word    fline + xa2 + ya6 + xb2 + ybA

        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more        'u
        word    fline + xa0 + ya6 + xb0 + yb8 + more
        word    fline + xa4 + ya4 + xb4 + yb8

        word    fline + xa0 + ya8 + xb2 + yb4 + more            'v
        word    fline + xa4 + ya8 + xb2 + yb4

        word    farc + xa1 + ya5 + aA + ax1 + ay1 + more        'w
        word    farc + xa3 + ya5 + aA + ax1 + ay1 + more
        word    fline + xa0 + ya5 + xb0 + yb8 + more
        word    fline + xa2 + ya5 + xb2 + yb6 + more
        word    fline + xa4 + ya5 + xb4 + yb8

        word    fline + xa0 + ya8 + xb4 + yb4 + more            'x
        word    fline + xa0 + ya4 + xb4 + yb8

        word    farc + xa2 + ya6 + aA + ax2 + ay2 + more        'y
        word    farc + xa2 + ya3 + aF + ax2 + ay2 + more
        word    fline + xa4 + ya3 + xb4 + yb8 + more
        word    fline + xa0 + ya6 + xb0 + yb8 + more
        word    fline + xa1 + ya1 + xb2 + yb1

        word    fline + xa0 + ya8 + xb4 + yb8 + more            'z
        word    fline + xa4 + ya8 + xb0 + yb4 + more
        word    fline + xa0 + ya4 + xb4 + yb4

        word    farc + xa3 + yaA + aD + ax1 + ay3 + more        '{
        word    farc + xa1 + ya6 + aC + ax1 + ay2 + more
        word    farc + xa1 + yaA + aF + ax1 + ay2 + more
        word    farc + xa3 + ya6 + aE + ax1 + ay3

        word    fline + xa2 + ya3 + xb2 + ybD                   '|

        word    farc + xa1 + yaA + aC + ax1 + ay3 + more        '}
        word    farc + xa3 + ya6 + aD + ax1 + ay2 + more
        word    farc + xa3 + yaA + aE + ax1 + ay2 + more
        word    farc + xa1 + ya6 + aF + ax1 + ay3

        word    farc + xa1 + ya8 + a8 + ax1 + ay1 + more        '~
        word    farc + xa3 + ya8 + aA + ax1 + ay1

' Vector font - custom characters ($7F+)

        word    fline + xa2 + ya9 + xb0 + yb4 + more            'delta
        word    fline + xa2 + ya9 + xb4 + yb4 + more
        word    fline + xa0 + ya4 + xb4 + yb4

        word    farc + xa2 + ya7 + a8 + ax2 + ay2 + more        'omega
        word    farc + xa1 + ya7 + aE + ax1 + ay2 + more
        word    farc + xa3 + ya7 + aF + ax1 + ay2 + more
        word    fline + xa1 + ya5 + xb1 + yb4 + more
        word    fline + xa3 + ya5 + xb3 + yb4 + more
        word    fline + xa0 + ya4 + xb1 + yb4 + more
        word    fline + xa4 + ya4 + xb3 + yb4

        word    farc + xa2 + ya8 + a0 + ax1 + ay1               'bullet

CON     fx = 3  'number of custom characters

DAT

'*************************************
'* Assembly language graphics driver *
'*************************************

                        org
'
'
' Graphics driver - main loop
'
loop                    rdlong  t1,par          wz      'wait for command
        if_z            jmp     #loop

                        movd    :arg,#arg0              'get 8 arguments
                        mov     t2,t1
                        mov     t3,#8
:arg                    rdlong  arg0,t2
                        add     :arg,d0
                        add     t2,#4
                        djnz    t3,#:arg

                        wrlong  zero,par                'zero command to signify received

                        call    #setd                   'set dx,dy from arg0,arg1

                        ror     t1,#16+2                'lookup command address
                        add     t1,#jumps
                        movs    :table,t1
                        rol     t1,#2
                        shl     t1,#3
:table                  mov     t2,0
                        shr     t2,t1
                        and     t2,#$FF
                        jmp     t2                      'jump to command


jumps                   byte    0                       '0
                        byte    setup_                  '1
                        byte    color_                  '2
                        byte    width_                  '3
                        byte    plot_                   '4
                        byte    line_                   '5
                        byte    arc_                    '6
                        byte    vec_                    '7
                        byte    vecarc_                 '8
                        byte    pix_                    '9
                        byte    pixarc_                 'A
                        byte    text_                   'B
                        byte    textarc_                'C
                        byte    textmode_               'D
                        byte    fill_                   'E
                        byte    loop                    'F
'
'
' setup(x_tiles, y_tiles*16, x_origin, y_origin, base_ptr)  bases_ptr, slices_ptr
'
setup_                  mov     xlongs,arg0             'set xlongs, ylongs
                        mov     ylongs,arg1
                        mov     xorigin,arg2            'set xorigin, yorigin
                        mov     yorigin,arg3
                        mov     basesptr,arg5           'set pointers
                        mov     slicesptr,arg6

                        jmp     #loop
'
'
' color(c)
'
color_                  mov     pcolor,arg0             'set pixel color

                        jmp     #loop
'
'
' width(w)  pixel_passes
'
width_                  mov     pwidth,arg0             'set pixel width
                        mov     passes,arg1             'set pixel passes

                        jmp     #loop
'
'
' plot(x, y)
'
plot_                   call    #plotd

                        jmp     #loop
'
'
' line(x, y)
'
line_                   call    #linepd

                        jmp     #loop
'
'
' arc(x, y, xr, yr, angle, anglestep, iterations, mode)
'
arc_                    and     arg7,#3                 'limit mode

:loop                   call    #arca                   'get arc dx,dy

                        cmp     arg7,#1         wz      'if not mode 1, set px,py
        if_nz           mov     px,dx
        if_nz           mov     py,dy

                        tjz     arg6,#loop              'if no points exit with new px,py

                        cmp     arg7,#3         wz      'if mode 3, set center
        if_z            call    #setd

                        test    arg7,#1         wz      'if mode 0 or 2, plot point
        if_z            call    #plotp

                        test    arg7,#1         wz      'if mode 1 or 3, plot line
        if_nz           call    #linepd

                        cmp     arg7,#2         wz      'if mode 2, set mode 1
        if_z            mov     arg7,#1

                        add     arg4,arg5               'step angle
                        djnz    arg6,#:loop             'loop if more iterations

                        jmp     #loop
'
'
' vec(x, y, vecscale, vecangle, vecdef_ptr)
' vecarc(x, y, xr, yr, angle, vecscale, vecangle, vecdef_ptr)
'
' vecdef:       word    $8000/$4000+angle       'vector mode + 13-bit angle (mode: $4000=plot, $8000=line)
'               word    length                  'vector length
'               ...                             'more vectors
'               ...
'               word    0                       'end of definition
'
vecarc_                 call    #arcmod

vec_                    tjz     arg2,#loop              'if scale 0, exit

:loop                   rdword  t7,arg4         wz      'get vector mode+angle
                        add     arg4,#2

        if_z            jmp     #loop                   'if mode+angle 0, exit

                        rdword  t1,arg4                 'get vector length
                        add     arg4,#2

                        abs     t2,arg2         wc      'add/sub vector angle to/from angle
                        mov     t6,arg3
                        sumc    t6,t7

                        call    #multiply               'multiply length by scale
                        add     t1,#$80                 'round up 1/2 lsb
                        shr     t1,#8

                        mov     t4,t1                   'get arc dx,dy
                        mov     t5,t1
                        call    #arcd

                        test    t7,h8000        wc      'plot pixel or draw line?
        if_nc           call    #plotd
                        test    t7,h8000        wc
        if_c            call    #linepd

                        jmp     #:loop                  'get next vector
'
'
' pix(x, y, pixrot, pixdef_ptr)
' pixarc(x, y, xr, yr, angle, pixrot, pixdef_ptr)
'
' pixdef:       word
'               byte    xwords, ywords, xorigin, yorigin
'               word    %%xxxxxxxx,%%xxxxxxxx
'               word    %%xxxxxxxx,%%xxxxxxxx
'               word    %%xxxxxxxx,%%xxxxxxxx
'               ...
'
pixarc_                 call    #arcmod

pix_                    mov     t6,pcolor               'save color

                        mov     px,dx                   'get center into px,py
                        mov     py,dy

                        mov     sy,pwidth               'get actual pixel width
                        add     sy,#1

                        rdbyte  dx,arg3                 'get dimensions into dx,dy
                        add     arg3,#1
                        rdbyte  dy,arg3
                        add     arg3,#1

                        rdbyte  t1,arg3                 'get origin and adjust px,py
                        add     arg3,#1
                        rdbyte  t2,arg3
                        add     arg3,#1
                        neg     t2,t2
                        sub     t2,#1
                        add     t2,dy
                        mov     t3,sy
:adjust                 test    arg2,#%001      wz
                        test    arg2,#%110      wc
        if_z            sumnc   px,t1
        if_nz           sumc    py,t1
                        test    arg2,#%010      wc
        if_nz           sumnc   px,t2
        if_z            sumnc   py,t2
                        djnz    t3,#:adjust

:yline                  mov     sx,#0                   'plot entire pix
                        mov     t3,dx
:xword                  rdword  t4,arg3                 'read next pix word
                        add     arg3,#2
                        shl     t4,#16
                        mov     t5,#8
:xpixel                 rol     t4,#2                   'plot pixel within word
                        test    t4,#1           wc      'set color
                        muxc    pcolor,color1
                        test    t4,#2           wc
                        muxc    pcolor,color2   wz      '(z=1 if color=0)
        if_nz           call    #plotp
                        test    arg2,#%001      wz      'update px,py for next x
                        test    arg2,#%110      wc
        if_z            sumc    px,sy
        if_nz           sumnc   py,sy
                        add     sx,sy
                        djnz    t5,#:xpixel             'another x pixel?
                        djnz    t3,#:xword              'another x word?
        if_z            sumnc   px,sx                   'update px,py for next y
        if_nz           sumc    py,sx
                        test    arg2,#%010      wc
        if_nz           sumc    px,sy
        if_z            sumc    py,sy
                        djnz    dy,#:yline              'another y line?

                        mov     pcolor,t6               'restore color

                        jmp     #loop
'
'
' text(x, y, @string) justx, justy
' textarc(x, y, xr, yr, angle, @string) justx, justy
'
textarc_                call    #arcmod

text_                   add     arg3,arg0               'add x into justx
                        add     arg4,arg1               'add y into justy

:chr                    rdbyte  t1,arg2         wz      'get chr
                        add     arg2,#1

        if_z            jmp     #loop                   'if 0, done

                        sub     t1,#$21                 'if chr out of range, skip
                        cmp     t1,#$7F-$21+fx  wc
        if_nc           jmp     #:skip

                        mov     arg5,fontptr            'scan font for chr definition
:scan                   tjz     t1,#:def
                        rdword  t2,arg5
                        add     arg5,#2
                        test    t2,h8000        wc
        if_nc           sub     t1,#1
                        jmp     #:scan

:def                    rdword  t7,arg5                 'get font definition word
                        add     arg5,#2

                        call    #fontxy                 'extract initial x,y

                        test    t7,#$80         wc      'arc or line?
        if_nc           jmp     #:line


                        mov     t2,textsx               'arc, extract x radius
                        mov     t3,#%0001_0001_1
                        call    #fontb
                        mov     t4,t1

                        mov     t2,textsy               'extract y radius
                        mov     t3,#%0010_0011_1
                        call    #fontb
                        mov     t5,t1

                        mov     t2,#1                   'extract starting angle
                        mov     t3,#%0010_0011_0
                        call    #fontb
                        shl     t1,#11

                        mov     t6,t1                   'extract angle sweep
                        mov     t3,#%0010_0011_0
                        call    #fontb
                        neg     arg6,t1
                        shl     arg6,#4
                        add     arg6,#65

                        call    #arcd                   'plot initial arc point
                        call    #plotd

:arc                    call    #arcd                   'connect subsequent arc points with lines
                        call    #linepd
                        add     t6,#$80
                        djnz    arg6,#:arc

                        jmp     #:more


:line                   call    #plotd                  'line, plot initial x,y

                        call    #fontxy                 'extract terminal x,y

                        call    #linepd                 'draw line


:more                   test    t7,#$02         wc      'more font definition?
        if_c            jmp     #:def

:skip                   mov     t1,textsp               'advance x to next chr position
                        mov     t2,textsx
                        call    #multiply
                        add     arg3,t1

                        jmp     #:chr                   'get next chr


fontxy                  mov     t2,textsx               'extract x
                        mov     t3,#%0011_0111_0
                        call    #fontb
                        mov     arg0,t1
                        add     arg0,arg3

                        mov     t2,textsy               'extract y
                        mov     t3,#%0100_1111_0
                        call    #fontb
                        mov     arg1,t1
                        add     arg1,arg4

setd                    mov     dx,xorigin              'set dx,dy from arg0,arg1
                        add     dx,arg0
                        mov     dy,yorigin
                        sub     dy,arg1
setd_ret
fontxy_ret              ret


fontb                   mov     t1,t7                   'extract bitrange from font word
                        shr     t3,#1           wc
                        and     t1,t3
        if_c            add     t1,#1
                        shr     t3,#4
                        shr     t7,t3

                        shl     t1,#32-4                'multiply t1[3..0] by t2
                        mov     t3,#4
:loop                   shl     t1,#1           wc
        if_c            add     t1,t2
                        djnz    t3,#:loop

fontb_ret               ret
'
'
' textmode(x_scale, y_scale, spacing, justification)
'
textmode_               mov     textsx,arg0             'set text x scale
                        mov     textsy,arg1             'set text y scale
                        mov     textsp,arg2             'set text spacing

                        jmp     #loop
'
'
' fill(x, y, da, db, db2, linechange, lines_minus_1)
'
fill_                   shl     dx,#16                  'get left and right fractions
                        or      dx,h8000
                        mov     t1,dx

                        mov     t2,xlongs               'get x pixels
                        shl     t2,#4

                        add     arg6,#1                 'pre-increment line counter

:yloop                  add     dx,arg2                 'adjust left and right fractions
                        add     t1,arg3

                        cmps    dx,t1           wc      'get left and right integers
        if_c            mov     base0,dx
        if_c            mov     base1,t1
        if_nc           mov     base0,t1
        if_nc           mov     base1,dx
                        sar     base0,#16
                        sar     base1,#16

                        cmps    base0,t2        wc      'left out of range?
        if_c            cmps    hFFFFFFFF,base1 wc      'right out of range?
        if_c            cmp     dy,ylongs       wc      'y out of range?
        if_nc           jmp     #:skip                  'if any, skip

                        mins    base0,#0                'limit left and right
                        maxs    base1,t2        wc
        if_nc           sub     base1,#1

                        shl     base0,#1                'make left mask
                        neg     mask0,#1
                        shl     mask0,base0
                        shr     base0,#5

                        shl     base1,#1                'make right mask
                        xor     base1,#$1E
                        neg     mask1,#1
                        shr     mask1,base1
                        shr     base1,#5

                        sub     base1,base0     wz      'ready long count
                        add     base1,#1

        if_z            and     mask0,mask1             'if single long, merge masks

                        shl     base0,#1                'get long base
                        add     base0,basesptr
                        rdword  base0,base0
                        shl     dy,#2
                        add     base0,dy
                        shr     dy,#2

                        mov     bits0,mask0             'ready left mask
:xloop                  mov     bits1,pcolor            'make color mask
                        and     bits1,bits0
                        rdlong  pass,base0              'read-modify-write long
                        andn    pass,bits0
                        or      pass,bits1
                        wrlong  pass,base0
                        shl     ylongs,#2               'advance to next long
                        add     base0,ylongs
                        shr     ylongs,#2
                        cmp     base1,#2        wz      'one more?
        if_nz           neg     bits0,#1                'if not, ready full mask
        if_z            mov     bits0,mask1             'if one more, ready right mask
                        djnz    base1,#:xloop           'loop if more longs

:skip                   sub     arg5,#1         wc      'delta change?
        if_c            mov     arg3,arg4               'if so, set new deltas
:same
                        add     dy,#1                   'adjust y
                        djnz    arg6,#:yloop            'another y?

                        jmp     #loop
'
'
' Plot line from px,py to dx,dy
'
linepd                  cmps    dx,px           wc, wr  'get x difference
                        negc    sx,#1                   'set x direction

                        cmps    dy,py           wc, wr  'get y difference
                        negc    sy,#1                   'set y direction

                        abs     dx,dx                   'make differences absolute
                        abs     dy,dy

                        cmp     dx,dy           wc      'determine dominant axis
        if_nc           tjz     dx,#:last               'if both differences 0, plot single pixel
        if_nc           mov     count,dx                'set pixel count
        if_c            mov     count,dy
                        mov     ratio,count             'set initial ratio
                        shr     ratio,#1
        if_c            jmp     #:yloop                 'x or y dominant?


:xloop                  call    #plotp                  'dominant x line
                        add     px,sx
                        sub     ratio,dy        wc
        if_c            add     ratio,dx
        if_c            add     py,sy
                        djnz    count,#:xloop

                        jmp     #:last                  'plot last pixel


:yloop                  call    #plotp                  'dominant y line
                        add     py,sy
                        sub     ratio,dx        wc
        if_c            add     ratio,dy
        if_c            add     px,sx
                        djnz    count,#:yloop

:last                   call    #plotp                  'plot last pixel

linepd_ret              ret
'
'
' Plot pixel at px,py
'
plotd                   mov     px,dx                   'set px,py to dx,dy
                        mov     py,dy

plotp                   tjnz    pwidth,#wplot           'if width > 0, do wide plot

                        mov     t1,px                   'compute pixel mask
                        shl     t1,#1
                        mov     mask0,#%11
                        shl     mask0,t1
                        shr     t1,#5

                        cmp     t1,xlongs       wc      'if x or y out of bounds, exit
        if_c            cmp     py,ylongs       wc
        if_nc           jmp     #plotp_ret

                        mov     bits0,pcolor            'compute pixel bits
                        and     bits0,mask0

                        shl     t1,#1                   'get address of pixel long
                        add     t1,basesptr
                        mov     t2,py
                        rdword  t1,t1
                        shl     t2,#2
                        add     t1,t2

                        rdlong  t2,t1                   'write pixel
                        andn    t2,mask0
                        or      t2,bits0
                        wrlong  t2,t1
plotp_ret
plotd_ret               ret
'
'
' Plot wide pixel
'
wplot                   mov     t1,py                   'if y out of bounds, exit
                        add     t1,#7
                        mov     t2,ylongs
                        add     t2,#7+8
                        cmp     t1,t2           wc
        if_nc           jmp     #plotp_ret

                        mov     t1,px                   'determine x long pair
                        sub     t1,#8
                        sar     t1,#4
                        cmp     t1,xlongs       wc
                        muxc    jumps,#%01              '(use jumps[1..0] to store writes)
                        add     t1,#1
                        cmp     t1,xlongs       wc
                        muxc    jumps,#%10

                        test    jumps,#%11      wz      'if x out of bounds, exit
        if_z            jmp     #plotp_ret

                        shl     t1,#1                   'get base pair
                        add     t1,basesptr
                        rdword  base1,t1
                        sub     t1,#2
                        rdword  base0,t1

                        mov     t1,px                   'determine pair shifts
                        shl     t1,#1
                        movs    :shift1,t1
                        xor     :shift1,#7<<1
                        add     t1,#9<<1
                        movs    :shift0,t1
                        test    t1,#$F<<1       wz      '(account for special case)
        if_z            andn    jumps,#%01

                        mov     pass,#0                 'ready to plot slices
                        mov     slice,slicesptr

:loop                   rdlong  mask0,slice             'get next slice
                        mov     mask1,mask0

:shift0                 shl     mask0,#0                'position slice
:shift1                 shr     mask1,#0

                        mov     bits0,pcolor            'colorize slice
                        and     bits0,mask0
                        mov     bits1,pcolor
                        and     bits1,mask1

                        mov     t1,py                   'plot lower slice
                        add     t1,pass
                        cmp     t1,ylongs       wc
        if_c            call    #wslice

                        mov     t1,py                   'plot upper slice
                        test    pwidth,#1       wc
                        subx    t1,pass
                        cmp     t1,ylongs       wc
        if_c            call    #wslice

                        add     slice,#4                'next slice
                        add     pass,#1
                        cmp     pass,passes     wz
        if_nz           jmp     #:loop

                        jmp     #plotp_ret
'
'
' Plot wide pixel slice
'
wslice                  shl     t1,#2                   'ready long offset

                        add     base0,t1                'plot left slice
                        test    jumps,#%01      wc
        if_c            rdlong  t2,base0
        if_c            andn    t2,mask0
        if_c            or      t2,bits0
        if_c            wrlong  t2,base0

                        add     base1,t1                'plot right slice
                        test    jumps,#%10      wc
        if_c            rdlong  t2,base1
        if_c            andn    t2,mask1
        if_c            or      t2,bits1
        if_c            wrlong  t2,base1

                        sub     base0,t1                'restore bases
                        sub     base1,t1

wslice_ret              ret
'
'
' Get arc point from args and then move args 5..7 to 2..4
'
arcmod                  call    #arca                   'get arc using first 5 args

                        mov     arg0,dx                 'set arg0,arg1
                        sub     arg0,xorigin
                        mov     arg1,yorigin
                        sub     arg1,dy

                        mov     arg2,arg5               'move args 5..7 to 2..4
                        mov     arg3,arg6
                        mov     arg4,arg7

arcmod_ret              ret
'
'
' Get arc dx,dy from arg0,arg1
'
'   in:         arg0,arg1 = center x,y
'               arg2/t4 = x length
'               arg3/t5 = y length
'               arg4/t6 = 13-bit angle
'
'   out:        dx,dy = arc point
'
arca                    mov     t4,arg2                 'use args
                        mov     t5,arg3
                        mov     t6,arg4

arcd                    call    #setd                   'reset dx,dy to arg0,arg1

                        mov     t1,t6                   'get arc dx
                        mov     t2,t4
                        call    #polarx
                        add     dx,t1

                        mov     t1,t6                   'get arc dy
                        mov     t2,t5
                        call    #polary
                        sub     dy,t1
arcd_ret
arca_ret                ret
'
'
' Polar to cartesian
'
'   in:         t1 = 13-bit angle
'               t2 = 16-bit length
'
'   out:        t1 = x|y
'
polarx                  add     t1,sine_90              'cosine, add 90° for sine lookup
polary                  test    t1,sine_180     wz      'get sine quadrant 3|4 into nz
                        test    t1,sine_90      wc      'get sine quadrant 2|4 into c
                        negc    t1,t1                   'if sine quadrant 2|4, negate table offset
                        or      t1,sine_table           'or in sine table address >> 1
                        shl     t1,#1                   'shift left to get final word address
                        rdword  t1,t1                   'read sine/cosine word
                        call    #multiply               'multiply sine/cosine by length to get x|y
                        add     t1,h8000                'add 1/2 lsb to round up x|y fraction
                        shr     t1,#16                  'justify x|y integer
                        negnz   t1,t1                   'if sine quadrant 3|4, negate x|y
polary_ret
polarx_ret              ret

sine_90                 long    $0800                   '90° bit
sine_180                long    $1000                   '180° bit
sine_table              long    $E000 >> 1              'sine table address shifted right
'
'
' Multiply
'
'   in:         t1 = 16-bit multiplicand (t1[31..16] must be 0)
'               t2 = 16-bit multiplier
'
'   out:        t1 = 32-bit product
'
multiply                mov     t3,#16
                        shl     t2,#16
                        shr     t1,#1           wc

:loop   if_c            add     t1,t2           wc
                        rcr     t1,#1           wc
                        djnz    t3,#:loop

multiply_ret            ret
'
'
' Defined data
'
zero                    long    0                       'constants
d0                      long    $200
h8000                   long    $8000
hFFFFFFFF               long    $FFFFFFFF
color1                  long    %%1111111111111111
color2                  long    %%2222222222222222

fontptr                 long    0                       'font pointer (set before cognew command)

pcolor                  long    %%1111111111111111      'pixel color
pwidth                  long    0                       'pixel width
passes                  long    1                       'pixel passes
textsx                  long    1                       'text scale x
textsy                  long    1                       'text scale y
textsp                  long    6                       'text spacing
'
'
' Undefined data
'
t1                      res     1       'temps
t2                      res     1
t3                      res     1
t4                      res     1
t5                      res     1
t6                      res     1
t7                      res     1

arg0                    res     1       'arguments passed from high-level
arg1                    res     1
arg2                    res     1
arg3                    res     1
arg4                    res     1
arg5                    res     1
arg6                    res     1
arg7                    res     1

basesptr                res     1       'pointers
slicesptr               res     1

xlongs                  res     1       'bitmap metrics
ylongs                  res     1
xorigin                 res     1
yorigin                 res     1

dx                      res     1       'line/plot coordinates
dy                      res     1
px                      res     1
py                      res     1

sx                      res     1       'line
sy                      res     1
count                   res     1
ratio                   res     1

pass                    res     1       'plot
slice                   res     1
base0                   res     1
base1                   res     1
mask0                   res     1
mask1                   res     1
bits0                   res     1
bits1                   res     1

{{

┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}