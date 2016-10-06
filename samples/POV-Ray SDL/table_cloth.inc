// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Include File
// File: table_cloth.inc
// Desc: table cloth for 'balcony.pov' demonstration scene
// Date: July/August 2001
// Auth: Christoph Hormann

// Updated: 09Aug2008 (jh) for v3.7 distribution

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (version < 3.7)
  #version 3.5;
#end

#declare ClCol01=color rgb <0.8, 0.7, 0.4>;
#declare ClCol02=color rgb <0.07, 0.12, 0.4>;

#declare CPig1=
pigment {
  gradient x
  triangle_wave
  color_map {
    [0.04 ClCol01 ]
    [0.04 ClCol02 ]
    [0.06 ClCol02 ]
    [0.06 ClCol01 ]
    [0.09 ClCol01 ]
    [0.09 ClCol02 ]
    [0.13 ClCol02 ]
    [0.13 ClCol01 ]
    [0.16 ClCol01 ]
    [0.16 ClCol02 ]
    [0.18 ClCol02 ]
    [0.18 ClCol01 ]
  }
}

#declare CPig2=
pigment {
  gradient y
  triangle_wave
  pigment_map {
    [0.04 CPig1 ]
    [0.04 ClCol02 ]
    [0.06 ClCol02 ]
    [0.06 CPig1 ]
    [0.09 CPig1 ]
    [0.09 ClCol02 ]
    [0.13 ClCol02 ]
    [0.13 CPig1 ]
    [0.16 CPig1 ]
    [0.16 ClCol02 ]
    [0.18 ClCol02 ]
    [0.18 CPig1 ]
  }
}

    /*
#include "clothutil.inc"

ReadClothFile("cloth.cth")
DrawSmoothTriangles2(Points, 90, 90, on, true, "cloth.inc")
        */
#declare Table_Cloth=
mesh2{
  #include "cloth.inc"
  texture {
    uv_mapping
    pigment {
      CPig2
    }
    finish {
      ambient 0.0
      diffuse 0.6
    }
    normal {
      quilted 0.16
      scale 0.008
    }
  }
  rotate -30*z
}

object { Table_Cloth translate <3.3,2.52,0> }

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

