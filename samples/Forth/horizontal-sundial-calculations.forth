: faccept ( -- f )
  pad 32 accept pad swap >float 0= throw ;
: >radians ( deg -- rad ) 180e f/ pi f* ;
: >degrees ( rad -- deg ) pi f/ 180e f* ;
: sundial
  cr ." Enter latitude: "
  faccept >radians fsin
  cr ." Enter longitude: "
  faccept
  cr ." Enter legal meridian: "
  faccept f-   ( sin[latitude] longitude )

  cr ." Hour : HourAngle , DialAngle"
  7 -6 do
    cr i . ." : "
    fover fover fnegate i 15 * s>d d>f f+
    fdup f. ." , "
    >radians ftan f* fatan >degrees f.
  loop fdrop fdrop ;
