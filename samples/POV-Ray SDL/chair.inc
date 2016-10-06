// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Include File
// File: chair.inc
// Desc: chair for 'balcony.pov' demonstration scene
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
}



/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "shapes.inc"

#local Chair_Tube_Rad=0.01;
#local Chair_Tube_Curve_Rad=0.045;
#local Chair_Leg_Angle=7;
#local Chair_Leg_AngleA=4;
#local Chair_Leg_Depth=0.13;

#local Chair_Plate_Curve_Rad=0.03;
#local Chair_Plate_Thickness=0.008;
#local Chair_Plate_Width=0.48;
#local Chair_Plate_UWidth=0.22;
#local Chair_Plate_Height=0.45;
#local Chair_Plate_UAngle=5;
#local Chair_Plate_Depth=0.26;

#local Chair_Leg=
union {

  intersection {
    torus { Chair_Tube_Curve_Rad, Chair_Tube_Rad }
    plane { x, 0 }
    plane { -x, 0 rotate (-90+Chair_Leg_Angle)*y }
  }

  cylinder {
    <-Chair_Tube_Curve_Rad,0,0>,
    <-Chair_Tube_Curve_Rad,0,-Chair_Plate_Height+0.01>, Chair_Tube_Rad
    rotate Chair_Leg_Angle*y
  }
  cylinder {
    <0,0,Chair_Tube_Curve_Rad>,
    <Chair_Leg_Depth,0,Chair_Tube_Curve_Rad>, Chair_Tube_Rad
  }

  object { Round_Cylinder_Merge (
    <-Chair_Tube_Curve_Rad,0,-Chair_Plate_Height-0.003>,
    <-Chair_Tube_Curve_Rad,0,-Chair_Plate_Height+0.025>, Chair_Tube_Rad+0.001, 0.005)
    rotate Chair_Leg_Angle*y
    texture { Tex_Table_Foot_Bottom }
  }

  translate -Chair_Leg_Depth*x

  translate Chair_Plate_Height*cos(radians(Chair_Leg_Angle))*z

}

#macro Chair_Back(Rotate)
union {

  intersection {
    torus { Chair_Tube_Curve_Rad, Chair_Tube_Rad }
    plane { x, 0 }
    plane { z, 0 }

    translate <-Chair_Leg_Depth, 0.0,Chair_Tube_Curve_Rad*2>
  }

  cylinder {
    <-0.08, 0.0,Chair_Tube_Curve_Rad>,
    < 0.05, 0.0,Chair_Tube_Curve_Rad>, Chair_Tube_Rad
  }

  intersection {
    torus { 4, Chair_Tube_Rad }
    plane { z, 0.4 }
    plane { -z, 0 }

    translate <-4-Chair_Leg_Depth-Chair_Tube_Curve_Rad, 0.0,Chair_Tube_Curve_Rad*2>
  }

  translate -Chair_Tube_Curve_Rad*z

  rotate 3*y
  rotate Rotate*x

  translate Chair_Tube_Curve_Rad*z

  translate Chair_Plate_Height*cos(radians(Chair_Leg_Angle))*z


}
#end

#local Chair_Base=
union {

  union {
    object { Chair_Back(-0.25) translate  Chair_Tube_Rad*2*y }

    object { Chair_Leg }
    object { Chair_Leg rotate 180*z }
    rotate -Chair_Leg_AngleA*x
    translate -0.23*y
  }
  union {
    object { Chair_Back( 0.25) translate -Chair_Tube_Rad*2*y }

    object { Chair_Leg }
    object { Chair_Leg rotate 180*z }
    rotate Chair_Leg_AngleA*x
    translate 0.23*y
  }

  texture { Tex_Table_Foot }
}

#local Chair_Plate=
union {

  intersection {
    merge {
      torus { Chair_Plate_Curve_Rad, Chair_Plate_Thickness translate  (Chair_Plate_Width/2)*y }
      torus { Chair_Plate_Curve_Rad, Chair_Plate_Thickness translate -(Chair_Plate_Width/2)*y }

      difference {
        cylinder {
          -(Chair_Plate_Width/2)*y, (Chair_Plate_Width/2)*y,
          Chair_Plate_Curve_Rad+Chair_Plate_Thickness
        }
        cylinder {
          -(Chair_Plate_Width/1.8)*y, (Chair_Plate_Width/1.8)*y,
          Chair_Plate_Curve_Rad-Chair_Plate_Thickness
        }
      }
    }

    plane { -z, 0 rotate -20*y}
    plane { -x, 0 }
  }


  merge {
    cylinder {
      <0.0, -Chair_Plate_Width/2, Chair_Plate_Curve_Rad>
      <0.0,  Chair_Plate_Width/2, Chair_Plate_Curve_Rad>,
      Chair_Plate_Thickness
    }

    sphere { <0.0, -Chair_Plate_Width/2, Chair_Plate_Curve_Rad>, Chair_Plate_Thickness }
    sphere { <0.0,  Chair_Plate_Width/2, Chair_Plate_Curve_Rad>, Chair_Plate_Thickness }

    rotate 70*y
  }

  box {
    < 0.00, -Chair_Plate_Width/2, Chair_Plate_Curve_Rad-Chair_Plate_Thickness>,
    <-Chair_Plate_Depth,  Chair_Plate_Width/2, Chair_Plate_Curve_Rad+Chair_Plate_Thickness>
  }
  cylinder {
    < 0.00, -Chair_Plate_Width/2, Chair_Plate_Curve_Rad>
    <-Chair_Plate_Depth, -Chair_Plate_Width/2, Chair_Plate_Curve_Rad>,
    Chair_Plate_Thickness
  }
  cylinder {
    < 0.00,  Chair_Plate_Width/2, Chair_Plate_Curve_Rad>
    <-Chair_Plate_Depth,  Chair_Plate_Width/2, Chair_Plate_Curve_Rad>,
    Chair_Plate_Thickness
  }

  box {
    <-Chair_Plate_Depth, -Chair_Plate_Width/2+Chair_Plate_Curve_Rad,
      Chair_Plate_Curve_Rad-Chair_Plate_Thickness>,
    <-Chair_Plate_Depth-Chair_Plate_Curve_Rad,  Chair_Plate_Width/2-Chair_Plate_Curve_Rad,
      Chair_Plate_Curve_Rad+Chair_Plate_Thickness>
  }

  cylinder {
    <-Chair_Plate_Depth,  Chair_Plate_Width/2-Chair_Plate_Curve_Rad,
      Chair_Plate_Curve_Rad-Chair_Plate_Thickness>
    <-Chair_Plate_Depth,  Chair_Plate_Width/2-Chair_Plate_Curve_Rad,
      Chair_Plate_Curve_Rad+Chair_Plate_Thickness>,
    Chair_Plate_Curve_Rad
  }
  cylinder {
    <-Chair_Plate_Depth, -Chair_Plate_Width/2+Chair_Plate_Curve_Rad,
      Chair_Plate_Curve_Rad-Chair_Plate_Thickness>
    <-Chair_Plate_Depth, -Chair_Plate_Width/2+Chair_Plate_Curve_Rad,
      Chair_Plate_Curve_Rad+Chair_Plate_Thickness>,
    Chair_Plate_Curve_Rad
  }
  torus {
    Chair_Plate_Curve_Rad, Chair_Plate_Thickness
    rotate 90*x
    translate <-Chair_Plate_Depth,
               -Chair_Plate_Width/2+Chair_Plate_Curve_Rad, Chair_Plate_Curve_Rad>
  }
  torus {
    Chair_Plate_Curve_Rad, Chair_Plate_Thickness
    rotate 90*x
    translate <-Chair_Plate_Depth,
                Chair_Plate_Width/2-Chair_Plate_Curve_Rad, Chair_Plate_Curve_Rad>
  }

  translate (Chair_Plate_Height+0.03)*z
  translate 0.18*x
}

#local Chair_Back_Plate=
union {
  intersection {
    union {
      intersection {
        difference {
          cylinder { <0.0, -0.30, 0.0>, <0.0,  0.30, 0.0>, 4+2*Chair_Plate_Thickness }
          cylinder { <0.0, -0.40, 0.0>, <0.0,  0.40, 0.0>, 4 }
        }
        plane {  y, Chair_Plate_UWidth rotate  Chair_Plate_UAngle*x }
        plane { -y, Chair_Plate_UWidth rotate -Chair_Plate_UAngle*x }
      }

      torus {
        4+Chair_Plate_Thickness, Chair_Plate_Thickness
        scale <1, 1, 1/cos(radians(Chair_Plate_UAngle))>
        rotate  Chair_Plate_UAngle*x
        translate  Chair_Plate_UWidth*y
      }

      torus {
        4+Chair_Plate_Thickness, Chair_Plate_Thickness
        scale <1, 1, 1/cos(radians(Chair_Plate_UAngle))>
        rotate -Chair_Plate_UAngle*x
        translate -Chair_Plate_UWidth*y
      }
    }

    plane { z, 0 rotate -8*y }
    plane { -z, 0 rotate -0.2*y }
  }

  intersection {
    union {
      cylinder {
        <4+Chair_Plate_Thickness,  0.3, 0.0>,
        <4+Chair_Plate_Thickness, -0.3, 0.0>,
        Chair_Plate_Thickness
        rotate -0.2*y
      }
      cylinder {
        <4+Chair_Plate_Thickness,  0.3, 0.0>,
        <4+Chair_Plate_Thickness, -0.3, 0.0>,
        Chair_Plate_Thickness
        rotate -8*y
      }
    }
    plane {  y, Chair_Plate_UWidth rotate  Chair_Plate_UAngle*x }
    plane { -y, Chair_Plate_UWidth rotate -Chair_Plate_UAngle*x }
  }
  sphere {
    <4+Chair_Plate_Thickness,  Chair_Plate_UWidth, 0.0>,
    Chair_Plate_Thickness
    rotate -0.2*y
  }
  sphere {
    <4+Chair_Plate_Thickness, -Chair_Plate_UWidth, 0.0>,
    Chair_Plate_Thickness
    rotate -0.2*y
  }

  sphere {
    <4+Chair_Plate_Thickness,
     Chair_Plate_UWidth-4*sin(radians(Chair_Plate_UAngle))*sin(radians(8)), 0.0>,
    Chair_Plate_Thickness
    rotate -8*y
  }
  sphere {
    <4+Chair_Plate_Thickness,
    -Chair_Plate_UWidth+4*sin(radians(Chair_Plate_UAngle))*sin(radians(8)), 0.0>,
    Chair_Plate_Thickness
    rotate -8*y
  }


  translate <-4-Chair_Leg_Depth-Chair_Tube_Curve_Rad+Chair_Tube_Rad,
             0.0, Chair_Tube_Curve_Rad*2>

  translate -Chair_Tube_Curve_Rad*z

  rotate 3*y

  translate Chair_Tube_Curve_Rad*z

  translate Chair_Plate_Height*cos(radians(Chair_Leg_Angle))*z

}


union {
  object { Chair_Back_Plate texture { Tex_Dark_Wood rotate 90*y } }
  object { Chair_Plate texture { Tex_Dark_Wood } }

  object { Chair_Base }
  scale 0.92
  rotate -60*z
  translate <2.68,3.35,0>
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
