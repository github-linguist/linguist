// This work is licensed under the Creative Commons Attribution-ShareAlike 3.0 Unported License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/3.0/ or send a
// letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

// Persistence Of Vision Ray Tracer Scene Description File
// File: balcony.pov
// Desc: Povray demonstration scene
// Date: July/August 2001
// Auth: Christoph Hormann
//
// ***********************************************************************
//
// -------------------- 'balcony' demonstration scene --------------------
//
// written July-August 2001 by Christoph Hormann <chris_hormann@gmx.de>
//
// demonstrates use of various new or changed features:
//
//    - isosurface (for the rail columns)
//    - mesh2 object (table cloth)
//    - uv-mapping (table cloth)
//    - pattern image type (background heightfield)
//    - slope pattern (background heightfield)
//    - variable reflection (water & glass)
//    - metallic reflection
//    - conserve_energy
//    - function pattern (water)
//    - fading interior (water, drink)
//    - 'circular' and 'orient' area_light
//    - radiosity
//    - photons (objects on the table)
//
// ***********************************************************************
// 
// Command line options:
//
// -w240 -h320
// -w480 -h640 +a0.3
// -w600 -h800 +a0.3

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#version 3.6;

#include "functions.inc"
#include "colors.inc"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#declare AreaLight=on;
#declare Radiosity=on;
#declare Photons=on;
#declare TestLight=off;
#declare show_Fog=true;
#declare show_Water=true;
#declare show_Terrain=true;
#declare show_Building=true;
#declare show_Table=true;
#declare show_TableCloth=true;
#declare show_Chair=true;
#declare show_Table_Stuff=true;

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

global_settings{
  max_trace_level 15
  assumed_gamma 1
    
  #if (Radiosity=on)
    radiosity{
      pretrace_start 0.08
      pretrace_end   0.01
      count 130
      nearest_count 5
      error_bound 0.3

      recursion_limit 1
      low_error_factor 0.5
      gray_threshold 0.0
      minimum_reuse 0.015
      brightness 1.0
      adc_bailout 0.01/2
      normal on
    }
  #end

  #if (Photons=on)
    photons {
      spacing 0.002
    }
  #end

}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
// This scene uses a non-standard camera set-up. 
// (See CAMERA in the included documentation for details.) 
// If you are new to POV-Ray, you might want to try a different demo scene.

camera {
  location    <0.5, 0.5, 1.2>
  direction   y
  sky         z
  up          z
  right x*image_width/image_height // keep propotions with any aspect ratio
  look_at     <5, 4.3, 0.9>
  angle       36
}

/*
camera {                          // table detail camera
  location    <1.5, 1.5, 1.0>
  direction   y
  sky         z
  up          z
  right x*image_width/image_height // keep propotions with any aspect ratio
  look_at     <3.3,2.52,0.5>
  angle       30
}
*/

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/


#if (TestLight=on)
  light_source {
    <2, 2, 2>
    color rgb 0.7
  }
#end


light_source {
  <3.0, -2.5, 2.6>*10000
  color rgb <3.43,2.87,1.95>
  #if (AreaLight=on)
    area_light 400*x 400*y  4,4
    jitter
    circular
    orient
  #end

  photons {
    reflection on
    refraction on
  }
}


#if (show_Fog)

fog{
   fog_type 2
   fog_alt 1.2
   fog_offset 0
   color rgbt <0.60, 0.68, 0.82, 0.0>
   distance 700
   up z
}

#end

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#include "sky.inc"

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (show_Water)
  #include "water.inc"
#end

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (show_Building)
  #include "building.inc"
#end


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (show_Terrain)
  #include "terrain.inc"
#end

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (show_Table)
  #include "table.inc"
#end

#if (show_TableCloth)
  #include "table_cloth.inc"
#end

#if (show_Table_Stuff)
  #include "table_stuff.inc"
#end

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#if (show_Chair)
  #include "chair.inc"
#end

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

