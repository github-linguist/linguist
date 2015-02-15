var
 radians = Math.PI / 4, // Pi / 4 is 45 degrees. All answers should be the same.
 degrees = 45.0,
 sine = Math.sin(radians),
 cosine = Math.cos(radians),
 tangent = Math.tan(radians),
 arcsin = Math.asin(sine),
 arccos = Math.acos(cosine),
 arctan = Math.atan(tangent);

// sine
window.alert(sine + " " + Math.sin(degrees * Math.PI / 180));
// cosine
window.alert(cosine + " " + Math.cos(degrees * Math.PI / 180));
// tangent
window.alert(tangent + " " + Math.tan(degrees * Math.PI / 180));
// arcsine
window.alert(arcsin + " " + (arcsin * 180 / Math.PI));
// arccosine
window.alert(arccos + " " + (arccos * 180 / Math.PI));
// arctangent
window.alert(arctan + " " + (arctan * 180 / Math.PI));
