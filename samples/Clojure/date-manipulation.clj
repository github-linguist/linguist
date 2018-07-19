(import java.util.Date
	java.text.SimpleDateFormat)

(defn time+12 [s]
  (let [sdf (SimpleDateFormat. "MMMM d yyyy h:mma zzz")]
    (-> (.parse sdf s)
	(.getTime ,)
	(+ , 43200000)
	long
	(Date. ,)
	(->> , (.format sdf ,)))))
