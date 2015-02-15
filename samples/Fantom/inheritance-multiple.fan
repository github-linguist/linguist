// a regular class
class Camera
{
  Str cameraMsg ()
  {
    "camera"
  }
}

// a mixin can only contain methods
mixin MobilePhone
{
  Str mobileMsg ()
  {
    "mobile phone"
  }
}

// class inherits from Camera, and mixes in the methods from MobilePhone
class CameraPhone : Camera, MobilePhone
{
}

class Main
{
  public static Void main ()
  {
    cp := CameraPhone ()
    echo (cp.cameraMsg)
    echo (cp.mobileMsg)
  }
}
