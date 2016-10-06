// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Include File
// File: building.inc
// Desc: building for 'balcony.pov' demonstration scene
// Date: July/August 2001
// Auth: Christoph Hormann

// Updated: 09Aug2008 (jh) for v3.7 distribution

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (version < 3.7)
  #version 3.5;
#end

#declare Tex_Stone=
texture {
  pigment {

    agate
    color_map {
      [0.7 color rgb <0.6,0.6,0.6> ]
      [1.0 color rgb 0.46 ]
    }
    warp { turbulence <0.7,0.4,0.4> }
    scale <0.3,1.2,1.2>*0.25
    rotate -60*y
    rotate 70*z
  }
  finish {
    ambient 0.0//0.1
    diffuse 0.5
    specular 0.15
  }
  normal {
    granite 0.2
    warp {turbulence 1}
    scale 0.1
  }
}

#declare Tex_Floor_A=
texture {
  pigment {
    color rgb <0.6,0.6,0.6>
  }
  finish {
    ambient 0.0
    diffuse 0.5
    specular 0.15
  }
  normal {
    granite 0.2
    warp {turbulence 1}
    scale 0.25
    translate 2
  }
}

#declare Tex_Floor_B=
texture {
  pigment {
    color rgb <0.18,0.18,0.22>
  }
  finish {
    ambient 0.0
    diffuse 0.5
    specular 0.15
  }
  normal {
    granite 0.2
    warp {turbulence 1}
    scale 0.25
  }
}

#declare Tex_Floor=
texture {
  checker
  texture { Tex_Floor_A },
  texture { Tex_Floor_B }
  scale 0.4
  rotate 45*z
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "shapes.inc"

#declare fn_Rad=
function {
  sin(pow(abs(z*0.4),1.8))+0.4
}

#declare Small_Column_part1=
isosurface {
  function { min(sqrt( x*x + y*y ) - fn_Rad(x, y, z), sqrt( x*x + y*y + pow(abs(z*2),1.4)) - 1.0) }
  max_gradient 1.8
  //eval
  accuracy 0.001
  contained_by { box { -4, 4 } }
  translate 4*z
  scale 6

  texture { Tex_Stone }
}

#declare Small_Column=
union {
  object { Small_Column_part1 translate 3*z }
  object { Round_Cylinder_Union (0, 3*z, 10, 1) }
  object { Round_Cylinder_Union (51*z, 54*z, 10, 1) }

  scale 0.01

  translate 0.1*z
  texture { Tex_Stone }
}

#declare Large_Column=
union {
  object { Round_Cylinder_Merge (0.7*z, 0.8*z, 0.22, 0.02) }

  cylinder { 0.75*z, 3*z, 0.2 }
  object { Round_Box_Union (<-0.23,-0.23,-0.1>, <0.23,0.23,0.15>, 0.02) }
  object { Round_Box_Union (<-0.20,-0.20,0.00>, <0.20,0.20,0.70>, 0.02) }
  object { Round_Box_Union (<-0.23,-0.23,0.58>, <0.23,0.23,0.70>, 0.02) }
  object { Round_Box_Union (<-0.26,-0.26,0.64>, <0.26,0.26,0.74>, 0.02) }

  torus { 0.2, 0.06 rotate 90*x translate 2.74*z }

  object { Round_Box_Union (<-0.26,-0.26,2.80>, <0.26,0.26,2.88>, 0.01) }

  texture { Tex_Stone }
}

#declare Balustrade=
union {
  object { Round_Box_Union (<0.0,-0.15,0>, <5,0.15,0.1>, 0.02) }
  object { Round_Box_Union (<0.0,-0.15,0.64>, <5,0.15,0.74>, 0.02) }

  #declare Cnt=0;

  #while (Cnt<10)
    object { Small_Column translate(Cnt*0.5+0.6)*x }
    #declare Cnt=Cnt+1;
  #end

  texture { Tex_Stone }
}

#declare Walls=
union {
  box { <-0.5,-0.5,-1>, <-2,5.5,3.2> }
  box { <-0.5,-0.5,-1>, <5.5,-2,3.2> }

  texture { Tex_Stone }
}

#declare Ceiling_Segment=
union {
  box { <-0.20,-0.20,2.94>, <0.20,0.20,2.98> }
  box { <-0.14,-0.14,2.94>, <0.14,0.14,3.01> }
}

#declare Ceiling=
union {
  box { <5.6,5.6,3.04>, <-2,-2,3.5> }

  union {
    object { Round_Box_Union (<0.0,-0.26,3.1>, <6,0.26,2.88>, 0.02) rotate 180*z }
    object { Round_Box_Union (<0.0,-0.26,3.1>, <6,0.26,2.88>, 0.02) rotate -90*z }
    translate <5,5,0>
  }

  difference {
    box { <5.0,5.0,2.95>, <-2,-2,3.5> }
    object { Ceiling_Segment translate <4.44,4.44,0> }
    object { Ceiling_Segment translate <4.44,3.94,0> }
    object { Ceiling_Segment translate <3.94,4.44,0> }
    object { Ceiling_Segment translate <3.94,3.94,0> }
    object { Ceiling_Segment translate <4.44,3.44,0> }
    object { Ceiling_Segment translate <3.44,4.44,0> }
    object { Ceiling_Segment translate <3.94,3.44,0> }
    object { Ceiling_Segment translate <3.44,3.94,0> }
    object { Ceiling_Segment translate <4.44,2.94,0> }
    object { Ceiling_Segment translate <2.94,4.44,0> }
  }

  texture { Tex_Stone }
}

#declare Base=
union {
  box { <5.4,5.4,0>, <0,0,-1> texture { Tex_Floor } }
  box { <6,6,-1>, <0,0,-2> }

  texture { Tex_Stone }
}

object { Balustrade rotate 180*z translate <5,5,0> }
object { Balustrade rotate -90*z translate <5,5,0> }
object { Large_Column translate <5,5,0> }
object { Base }
object { Walls }
object { Ceiling }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

