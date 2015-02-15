// Set up the canvas
var canvas = document.createElement("canvas"),
    ctx = canvas.getContext("2d"),
    width = 400, height = 400;

ctx.canvas.width = width;
ctx.canvas.height = height;

// Optionaly add it to the current page
document.body.appendChild(canvas);

// Draw an image
var img = document.createElement("img");
img.onload = function(){
    // Draw the element into the top-left of the canvas
    ctx.drawImage(img, 0, 0);
};
img.src = "//placehold.it/400x400";

// Fill the canvas with a solid blue color
ctx.fillStyle = "blue";
ctx.fillRect(0, 0, width, height);

// Place a black pixel in the middle
// Note that a pixel is a 1 by 1 rectangle
// This is the fastest method as of 2012 benchmarks
ctx.fillStyle = "black";
ctx.fillRect(width / 2, height / 2, 1, 1);
