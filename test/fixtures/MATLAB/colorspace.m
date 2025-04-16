%COLORSPACE   Gets/sets/changes the color space.
%    IN = COLORSPACE(IN,COL), with IN a color image, changes the color
%    space of image IN to COL, converting the pixel values as required.
%    If COL is 'grey', a scalar image is returned with the luminosity (Y).
%    If COL is '', no transformation is made, the color space information
%    is simply removed.
%
%    A color space is any string recognized by the system. It is possible
%    to specify any other string as color space, but no conversion of pixel
%    values can be made, since the system doesn't know about that color
%    space. Color space names are not case sensitive. Recognized color
%    spaces are:
%
%       grey (or gray)
%       RGB
%       sRGB
%       CMY
%       CMYK
%       HSI
%       ICH
%       ISH
%       HCV
%       HSV
%       XYZ
%       Yxy
%       Lab (or L*a*b*, CIELAB)
%       Luv (or L*u*v*, "CIELUV")
%       LCH (or L*C*H*)
%
%    See also: dip_image/iscolor

function obj = colorspace(obj,col)
if nargin==1
   obj = obj.ColorSpace;
elseif isempty(col)
   obj.ColorSpace = '';
else
   obj = colorspacemanager(obj,col);
end
