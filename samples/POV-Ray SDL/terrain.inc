// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence of Vision Ray Tracer Include File
// File: terrain.inc
// Desc: terrain for 'balcony.pov' demonstration scene
// Date: July/August 2001
// Auth: Christoph Hormann

// Updated: 09Aug2008 (jh) for v3.7 distribution

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (version < 3.7)
  #version 3.5;
#end

#declare Tex_Vegetation=
texture {
  pigment {
    bozo
    color_map {
      [0.3 color rgb <0.20,0.35,0.1>*0.9 ]
      [0.8 color rgb <0.12,0.35,0.1>*0.7 ]
    }
    scale 4
  }
  finish {
    ambient 0.0
    diffuse 0.5
    brilliance 0.8
    specular 0.1
  }
  normal {
    granite 0.4
  }
}

#declare Tex_Stone=
texture {
  pigment {
    color rgb <0.6,0.6,0.6>
  }
  finish {
    ambient 0.0//0.1
    diffuse 0.45
    specular 0.15
  }
}

#declare Tex_Terrain=
texture {
  slope -z
  texture_map {
    [0.34 Tex_Vegetation ]
    [0.34 Tex_Stone ]
  }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/



#declare Terrain=
object {
  height_field {

    function 300,300 {
      pigment {
        function { 1-(min(pow(x*x + z*z,1.3), 1) -0.0001) }
        color_map {
          [0.0 color rgb 0.0]
          [1.0 color rgb 1.0]
        }
        scale 0.35
        translate <0.5,0,0.5>

        warp { turbulence 0.3 }

        scale 3
        warp { turbulence 0.4 lambda 2.2 octaves 8 }
        scale 1/3

        rotate -90*x
        translate -0.5*y
        scale <1, -1, 1>
        translate 0.5*y
      }
    }

    water_level 0.02

    rotate 90*x
    rotate -10*z

    scale <4, 4, 1>
    scale 30
  }
}


union {
  object {
    Terrain
    translate <130, 368, -10>
  }

  object {
    Terrain
    rotate -180*z
    scale 0.3
    translate <90, 97, -6>
  }
  texture { Tex_Terrain }
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
