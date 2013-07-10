(defprotocol ISound (sound []))
 
(deftype Cat []
  ISound
  (sound [_] "Meow!"))
 
(deftype Dog []
  ISound
  (sound [_] "Woof!"))
 
(extend-type default
  ISound
  (sound [_] "... silence ..."))
 
(sound 1) ;; => "... silence ..."
