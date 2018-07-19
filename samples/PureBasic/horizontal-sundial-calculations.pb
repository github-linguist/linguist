If OpenConsole()
  Define.f lat, slat, lng, ref
  Define.i h
  Print("Enter latitude       => "): lat=ValF(Input())
  Print("Enter longitude      => "): lng=ValF(Input())
  Print("Enter legal meridian => "): ref=ValF(Input())
  PrintN("")

  slat=Sin(lat*2*#PI/360)
  PrintN("    sine of latitude:   "+StrF(slat,3))
  PrintN("    diff longitude:     "+StrF((lng-ref),3)+#CRLF$)
  PrintN("Hour, sun hour angle, dial hour line angle from 6am to 6pm")

  For h=-6 To 6
    Define.f hra, hla
    hra=15*h
    hra=hra-(lng-ref)
    hla=ATan(slat*Tan(hra*2*#PI/360))*360/(2*#PI)
    PrintN("HR="+RSet(Str(h),3)+"; HRA="+RSet(StrF(hra,3),7)+"; HLA="+RSet(StrF(hla,3),7))
  Next

EndIf
