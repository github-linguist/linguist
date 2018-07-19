Python 3.2.2 (default, Sep  4 2011, 09:51:08) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> from math import degrees, radians, sin, cos, tan, asin, acos, atan, pi
>>> rad, deg = pi/4, 45.0
>>> print("Sine:", sin(rad), sin(radians(deg)))
Sine: 0.7071067811865475 0.7071067811865475
>>> print("Cosine:", cos(rad), cos(radians(deg)))
Cosine: 0.7071067811865476 0.7071067811865476
>>> print("Tangent:", tan(rad), tan(radians(deg)))
Tangent: 0.9999999999999999 0.9999999999999999
>>> arcsine = asin(sin(rad))
>>> print("Arcsine:", arcsine, degrees(arcsine))
Arcsine: 0.7853981633974482 44.99999999999999
>>> arccosine = acos(cos(rad))
>>> print("Arccosine:", arccosine, degrees(arccosine))
Arccosine: 0.7853981633974483 45.0
>>> arctangent = atan(tan(rad))
>>> print("Arctangent:", arctangent, degrees(arctangent))
Arctangent: 0.7853981633974483 45.0
>>>
