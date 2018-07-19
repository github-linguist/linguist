(import '(java.util GregorianCalendar))
(defn yuletide [start end]
	    (filter #(= (. (new GregorianCalendar %
			 (. GregorianCalendar DECEMBER) 25) get (. GregorianCalendar DAY_OF_WEEK))
		 (. GregorianCalendar SUNDAY)) (range start (inc end))))

(yuletide 2008 2121)
