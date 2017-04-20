// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Include File
// File: table.inc
// Desc: table for 'balcony.pov' demonstration scene
// Date: July/August 2001
// Auth: Christoph Hormann

// Updated: 09Aug2008 (jh) for v3.7 distribution

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (version < 3.7)
  #version 3.5;
#end

#declare Tex_Table_Foot=
texture {
  pigment {
    color rgb 0.4
  }
  finish {
    ambient 0.0
    diffuse 0.3
    specular 0.4
    roughness 0.01
    metallic

    reflection {
      0.8
      metallic
    }
  }
}

#declare Tex_Table_Foot_Bottom=
texture {
  pigment {
    color rgb 0.2
  }
  finish {
    ambient 0.0
    diffuse 0.3
    specular 0.4
    roughness 0.02
  }
}

#declare Tex_Dark_Wood=
texture {
  pigment {
    bozo
    color_map {
      [0.3 color rgb <0.0,0.0,0.16> ]
      [0.5 color rgb <0.0,0.0,0.08> ]
      [0.7 color rgb <0.0,0.0,0.0> ]
    }
    scale <4,1,1>*0.036
  }
  finish {
    ambient 0.09
    diffuse 0.3
    specular 0.5
    roughness 0.025

    reflection {
      0.15
      metallic
    }
  }
  normal {
    granite 0.1
    scale 0.03
    accuracy 0.007
  }
  scale 0.5
  rotate 90*y
}

#declare Tex_Dark_Wood2 =
texture {
  pigment {
    wood
    color_map {
      [ 0.0000  color rgb<0.6431, 0.3176, 0.0824> ]
      [ 0.1000  color rgb<0.6196, 0.2824, 0.0588> ]
      [ 0.2000  color rgb<0.7137, 0.3725, 0.1529> ]
      [ 0.3000  color rgb<0.7529, 0.4157, 0.1922> ]
      [ 0.4000  color rgb<0.8157, 0.4941, 0.2588> ]
      [ 0.5000  color rgb<0.7686, 0.4745, 0.2196> ]
      [ 0.6000  color rgb<0.8471, 0.5647, 0.2980> ]
      [ 0.7000  color rgb<0.8627, 0.5843, 0.3137> ]
      [ 0.8000  color rgb<0.8902, 0.6314, 0.3529> ]
      [ 0.9000  color rgb<0.8627, 0.6118, 0.3294> ]
      [ 1.0000  color rgb<0.8392, 0.5922, 0.3098> ]
    }

    turbulence <0.075, 0.075, 0.65>
    scale      <0.04, 0.04, 0.6>

    scale 0.27
    rotate 91*y
    translate -0.1*z
  }
  finish {
    ambient 0.0
    diffuse 0.4
    specular 0.3
    roughness 0.025

    reflection {
      0.17
      metallic
    }
  }
  normal {
    granite 0.015
    scale <0.02, 0.02, 0.06>
    rotate 91*y
    accuracy 0.007
  }
}


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "shapes.inc"

#declare Table_Height=0.66;

#declare Table=
union {

  torus { 0.49, 0.01 sturm rotate 90*x translate z*(Table_Height-0.01) }
  cylinder { z*(Table_Height-0.05), z*(Table_Height-0.01), 0.5 }
  cylinder { z*(Table_Height-0.01), z*Table_Height, 0.49 }

  union {
    cylinder { z*0.01, z*0.63, 0.03 translate 0.37*x }
    cylinder { z*0.01, z*0.63, 0.03 translate 0.37*x rotate 120*z }
    cylinder { z*0.01, z*0.63, 0.03 translate 0.37*x rotate 240*z }

    texture { Tex_Table_Foot }

    rotate -70*z
  }

  union {
    object { Round_Cylinder_Merge (0, z*0.03, 0.031, 0.005) translate 0.37*x }
    object { Round_Cylinder_Merge (0, z*0.03, 0.031, 0.005) translate 0.37*x rotate 120*z }
    object { Round_Cylinder_Merge (0, z*0.03, 0.031, 0.005) translate 0.37*x rotate 240*z }

    texture { Tex_Table_Foot_Bottom }

    rotate -70*z
  }

  texture { Tex_Dark_Wood2 }

  #if (show_TableCloth)
    scale <0.97, 0.97, 0.99>
  #else
    scale <0.97, 0.97, 1.00>
  #end
}

object { Table translate <3.3,2.52,0> }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
