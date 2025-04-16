// The original code is GraphicConfiguration.inc, released November 1, 1999.
//
//
// The initial developer of the original code is Mike Lischke (www.soft-gems.net),
//
// Copyright (C) 1999-2005 Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
//
// This file is part of the image library GraphicEx.
//
// In this configuration file you can determine which parts of the library are to be compiled.
// This is in particular important for LZW compression code for which Unisys owns a patent.
// If you want to use LZW decoding then you have to make a deal with Unisys. Don't make me
// responsible for using the code.
// Other configuration options include to limit the number of available image formats to
// save memory, if necessary.
//
//----------------------------------------------------------------------------------------------------------------------

// Pick your preferred image formats here or leave them all enabled. By disabling
// certain formats which are not of interest you can save some memory.
{$define UseLZW}  //patent is out, yay!
{$define SGIGraphic}             // *.bw, *.rgb, *.rgba, *.sgi images are supported
{$define AutodeskGraphic}        // *.cel, *.pic images
{-$define NewTIFFGraphic}            // *.tif, *.tiff images
{$define OldTIFFGraphic}
{$define TargaGraphic}           // *.tga, *.vst, *.icb, *.vda, *.win images
{$define PCXGraphic}             // *.pcx, *.pcc, *.scr images
{$define PCDGraphic}             // *.pcd images
{$define PortableMapGraphic}     // *.ppm, *.pgm, *.pbm images
{$define CUTGraphic}             // *.cut (+ *.pal) images
{$define GIFGraphic}             // *.gif images
{$define RLAGraphic}             // *.rla, *.rpf images
{$define PhotoshopGraphic}       // *.psd, *.pdd images
{$define PaintshopProGraphic}    // *.psp images
{$define PortableNetworkGraphic} // *.png images
{$define EPSGraphic}             // *.eps images
{$define ArtsAndLettersGraphic}  // *.ged images

// adjust dependent definitions
{$ifdef NewTIFFGraphic} {$define TIFFGraphic} {$endif}
{$ifdef OldTIFFGraphic} {$define TIFFGraphic} {$endif}
{$ifndef TIFFGraphic} {$undef EPSGraphic} {$endif}
