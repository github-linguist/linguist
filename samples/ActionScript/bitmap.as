// To import the BitmapData class:
import flash.display.BitmapData;

// Creates a new BitmapData object with a width of 500 pixels and a height of 300 pixels.
var bitmap:BitmapData = new BitmapData(500, 300);

// Create a BitmapData with transparency disallowed
var opaqueBitmap:BitmapData = new BitmapData(500, 300, false);

// Bitmap with initial fill colour, as 0xAARRGGBB (default is white)
var redFilledBitmap:BitmapData = new BitmapData(400, 300, true, 0xFFFF0000);

// Get the colour value of the pixel at point (200, 200)
bitmap.getPixel(200, 200)     // As 0xRRGGBB
bitmap.getPixel32(200, 200)   // As 0xAARRGGBB

// Set the colour value of the pixel at point (300, 200) to blue
bitmap.setPixel(300, 200, 0x0000FF);       // As 0xRRGGBB
bitmap.setPixel32(300, 200, 0xFF0000FF);   // As 0xAARRGGBB

// Fill the bitmap with a given colour (as 0xAARRGGBB) after construction
bitmap.fillRect(bitmap.rect, 0xFF44FF44);
