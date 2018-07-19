(defprotocol Camera)

(defprotocol MobilePhone)

(deftype CameraPhone []
  Camera
  MobilePhone)
