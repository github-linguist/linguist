// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Include File
// File: bglass.inc
// Desc: drinking glass for 'balcony.pov' demonstration scene
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
      roughness 0.05

      reflection {
        0.0, 1.0
        fresnel on
      }

      conserve_energy
    }
  }
  interior {
    ior 1.5
    fade_distance 0.1
    fade_power 1001
    fade_color <0.4,0.4,0.4>
  }
}

#declare Mat_Liquid=
material {
  texture {
    pigment { color rgbt 1 }
    finish {
      ambient 0.0
      diffuse 0.1
      specular 0.5
      roughness 0.01

      reflection {
        0.0, 1.0
        fresnel on
      }

      conserve_energy
    }
  }
  interior {
    ior 1.3
    fade_distance 0.03
    fade_power 1001
    fade_color <0.8,0.3,0.4>
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "shapes.inc"

#local Content_Shape=
merge {
  cylinder {
    0*z, 5*z, 3.0
  }

  object { Round_Cylinder_Merge (2*z, -3*z, 3.0, 0.25) }

  material { Mat_Liquid }
}

#declare Glass=
union {
  merge {
    difference {
      cylinder {
        0.01*z, 14*z, 3.2
      }

      cylinder {
        0*z, 10*z, 3.0
        translate 4.6*z
      }

      object { Round_Cylinder_Merge (2*z, -3*z, 3.0, 0.25) translate 4.6*z }
    }
    torus {
      3.1, 0.1
      rotate 90*x
      translate 14*z
    }

    material { Mat_Glass }
  }

  object { Content_Shape scale 0.99 translate 4.6*z }

  scale 1.1
  scale 0.01
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

