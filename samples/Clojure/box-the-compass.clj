(ns boxing-the-compass
  (:use [clojure.string :only [capitalize]]))

(def headings
     (for [i (range 0 (inc 32))]
       (let [heading (* i 11.25)]
	 (case (mod i 3)
	       1 (+ heading 5.62)
	       2 (- heading 5.62)
	       heading))))

(defn angle2compass
  [angle]
  (let [dirs ["N" "NbE" "N-NE" "NEbN" "NE" "NEbE" "E-NE" "EbN"
	      "E" "EbS" "E-SE" "SEbE" "SE" "SEbS" "S-SE" "SbE"
	      "S" "SbW" "S-SW" "SWbS" "SW" "SWbW" "W-SW" "WbS"
	      "W" "WbN" "W-NW" "NWbW" "NW" "NWbN" "N-NW" "NbW"]
	unpack {\N "north" \E "east" \W "west" \S "south" \b " by " \- "-"}
	sep  (/ 360 (count dirs))
	dir  (int (/ (mod (+ angle (/ sep 2)) 360) sep))]
    (capitalize (apply str (map unpack (dirs dir))))))

(print
 (apply str (map-indexed #(format "%2s %-18s %7.2f\n"
				  (inc (mod %1 32)) (angle2compass %2) %2)
			 headings)))
