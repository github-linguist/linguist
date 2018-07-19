(import '(javax.swing JFrame))

(let [frame (JFrame. "A Window")]
	   (doto frame
		 (.setSize 600 800)
		 (.setVisible true)))
