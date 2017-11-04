// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Include File
// File: table_stuff.inc
// Desc: stuff on the table for 'balcony.pov' demonstration scene
// Date: July/August 2001
// Auth: Christoph Hormann

// Updated: 09Aug2008 (jh) for v3.7 distribution

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (version < 3.7)
  #version 3.5;
#end

#declare Mat_Glass=
material {
  texture {
    pigment { color rgbt 1 }
    finish {
      ambient 0.0
      diffuse 0.1
      specular 0.5
      roughness 0.01

      reflection {
        0.15, 1.0
        fresnel on
      }

      conserve_energy
    }
  }
  interior {
    ior 1.5
    fade_distance 0.1
    fade_power 1001
    fade_color <0.4,0.4,0.8>
  }
}

#declare Tex_Box_Metal=
texture {
  pigment {
    color rgb <0.5,0.45,0.4>
  }
  finish {
    ambient 0.0
    diffuse 0.05
    specular 0.5
    roughness 0.01
    metallic

    reflection {
      0.8
      metallic
    }
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "shapes.inc"
#include "functions.inc"

#declare Table_Height=0.66;

#declare Box_Iso=
isosurface {
  function  { -f_superellipsoid(x,y,z, 0.2, 0.2) }
  contained_by {box { -1.2, 1.2 }}
  max_gradient 1.1

  translate 1.001*z
}

#declare Box=
union {
  intersection {
    object { Box_Iso }
    object { Round_Box_Merge (<-1.1,-1.1,1.6>, <1.1,1.1,-0.1>, 0.4) }

  }
  intersection {
    object { Box_Iso }
    object { Round_Box_Merge (<-1.1,-1.1,1.6>, <1.1,1.1,2.2>, 0.4) }
    scale 1.01
    rotate 0.2
  }
  torus {
    0.6, 0.13
    rotate 90*z
    translate 2*z
  }

  scale <1,1,0.6>
}

#include "bglass.inc"

union {
  object {
    Glass

    photons{
      target 1.0
      refraction on
      reflection on
    }

    translate <-0.16,-0.1,0>
  }

  object {
    Box

    texture {
      Tex_Box_Metal
    }
    photons{
      target 1.0
      reflection on
    }

    scale 0.06
    translate <-0.1,0.2,0>
  }


  translate <3.3,2.52,Table_Height>
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
