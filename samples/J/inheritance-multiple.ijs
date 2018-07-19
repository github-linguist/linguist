coclass 'Camera'

create=: verb define
  NB. creation-specifics for a camera go here
)

destroy=: codestroy

NB. additional camera methods go here

coclass 'MobilePhone'

create=: verb define
  NB. creation-specifics for a mobile phone go here
)

destroy=: codestroy

NB. additional phone methods go here

coclass 'CameraPhone'
coinsert 'Camera MobilePhone'

create=: verb define
  create_Camera_ f. y
  create_MobilePhone_ f. y
  NB. creation details specific to a camera phone go here
)

destroy=: codestroy

NB. additional camera-phone methods go here
