(import [javax.sound.sampled AudioFormat AudioSystem SourceDataLine])

(defn play [sample-rate bs]
  (let [af (AudioFormat. sample-rate 8 1 true true)]
    (doto (AudioSystem/getSourceDataLine af)
      (.open af sample-rate)
      .start
      (.write bs 0 (count bs))
      .drain
      .close)))

(defn note [hz sample-rate ms]
  (let [period (/ hz sample-rate)]
    (->> (range (* sample-rate ms 1/1000))
	 (map #(->> (* 2 Math/PI % period)
		    Math/sin
		    (* 127 ,)
		    byte) ,))))

(def morse-codes
     {\A ".-"   \J ".---" \S "..."   \1 ".----" \. ".-.-.-" \: "---..."
      \B "-..." \K "-.-"  \T "-"     \2 "..---" \, "--..--" \; "-.-.-."
      \C "-.-." \L ".-.." \U "..-"   \3 "...--" \? "..--.." \= "-...-"
      \D "-.."  \M "--"   \V "...-"  \4 "....-" \' ".----." \+ ".-.-."
      \E "."    \N "-."   \W ".--"   \5 "....." \! "-.-.--" \- "-....-"
      \F "..-." \O "---"  \X "-..-"  \6 "-...." \/ "-..-."  \_ "..--.-"
      \G "--."  \P ".--." \Y "-.--"  \7 "--..." \( "-.--."  \" ".-..-."  ;"
      \H "...." \Q "--.-" \Z "--.."  \8 "---.." \) "-.--.-" \$ "...-..-"
      \I ".."   \R ".-."  \0 "-----" \9 "----." \& ".-..."  \@ ".--.-."
      \space " "})

(def sample-rate 1024)

(let [hz 440
      ms 50]
  (def sounds
       {\. (note hz sample-rate (* 1 ms))
	\- (note hz sample-rate(* 3 ms))
	:element-gap (note 0 sample-rate (* 1 ms))
	:letter-gap (note 0 sample-rate (* 3 ms))
	\space (note 0 sample-rate (* 1 ms))})) ;includes letter-gap on either side
	
(defn convert-letter [letter]
  (->> (get morse-codes letter "")
       (map sounds ,)
       (interpose (:element-gap sounds) ,)
       (apply concat ,)))

(defn morse [s]
  (->> (.toUpperCase s)
       (map convert-letter ,)
       (interpose (:letter-gap sounds) ,)
       (apply concat ,)
       byte-array
       (play sample-rate ,)))
