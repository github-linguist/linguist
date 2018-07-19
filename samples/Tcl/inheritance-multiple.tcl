package require TclOO

oo::class create Camera
oo::class create MobilePhone
oo::class create CameraPhone {
    superclass Camera MobilePhone
}
