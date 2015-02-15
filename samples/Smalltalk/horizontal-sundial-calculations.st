|lat slat lng ref hra hla pi|
pi := 1 arcTan * 4.
'Enter latitude:       ' display. lat := stdin nextLine asNumber.
'Enter longitude:      ' display. lng := stdin nextLine asNumber.
'Enter legal meridian: ' display. ref := stdin nextLine asNumber.
slat := lat degreesToRadians sin.
('sine of latitude: %1' % { slat }) displayNl.
('diff longitude: %1' % { lng - ref }) displayNl.

'Hour, sun hour angle, dial hour line angle from 6am to 6pm' displayNl.

-6 to: 6 do: [ :h |
  hra := 15.0 * h.
  hra := hra - lng + ref.
  hla := (hra degreesToRadians tan * slat) arcTan radiansToDegrees.
  ('HR= %1;  %4  HRA=%2;  %4  HLA= %3' % { h. hra. hla. $<9> }) displayNl.
]
