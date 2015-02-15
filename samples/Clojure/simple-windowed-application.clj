(ns counter-window
  (:import (javax.swing JFrame JLabel JButton)))

(defmacro on-action [component event & body]
  `(. ~component addActionListener
      (proxy [java.awt.event.ActionListener] []
        (actionPerformed [~event] ~@body))))

(defn counter-app []
  (let [counter (atom 0)
        label (JLabel. "There have been no clicks yet")
        button (doto (JButton. "Click me!")
                 (on-action evnt
                   (.setText label
                      (str "Counter: " (swap! counter inc)))))
        panel (doto (JPanel.)
                (.setOpaque true)
                (.add label)
                (.add button))]
    (doto (JFrame. "Counter App")
      (.setContentPane panel)
      (.setSize 300 100)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))
