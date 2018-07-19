(import java.util.GregorianCalendar
	java.text.DateFormatSymbols)

(->> (for [year (range 1900 2101)
	   month [0 2 4 6 7 9 11] ;; 31 day months
	   :let [cal (GregorianCalendar. year month 1)
		 day (.get cal GregorianCalendar/DAY_OF_WEEK)]
	   :when (= day GregorianCalendar/FRIDAY)]
       (println month "-" year))
     count
     (println "Total Months: " ,))
