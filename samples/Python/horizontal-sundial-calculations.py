from __future__ import print_function
import math
try: raw_input
except: raw_input = input

lat = float(raw_input("Enter latitude       => "))
lng = float(raw_input("Enter longitude      => "))
ref = float(raw_input("Enter legal meridian => "))
print()

slat = math.sin(math.radians(lat))
print("    sine of latitude:   %.3f" % slat)
print("    diff longitude:     %.3f" % (lng-ref))
print()
print("Hour, sun hour angle, dial hour line angle from 6am to 6pm")

for h in range(-6, 7):
  hra = 15 * h
  hra -= lng - ref
  hla = math.degrees(math.atan(slat * math.tan(math.radians(hra))))
  print("HR=%3d; HRA=%7.3f; HLA=%7.3f" % (h, hra, hla))
