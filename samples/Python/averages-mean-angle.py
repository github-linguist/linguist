>>> from cmath import rect, phase
>>> from math import radians, degrees
>>> def mean_angle(deg):
...     return degrees(phase(sum(rect(1, radians(d)) for d in deg)/len(deg)))
...
>>> for angles in [[350, 10], [90, 180, 270, 360], [10, 20, 30]]:
...     print('The mean angle of', angles, 'is:', round(mean_angle(angles), 12), 'degrees')
...
The mean angle of [350, 10] is: -0.0 degrees
The mean angle of [90, 180, 270, 360] is: -90.0 degrees
The mean angle of [10, 20, 30] is: 20.0 degrees
>>>
