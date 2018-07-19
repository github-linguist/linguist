(ns active-object
  (:import (java.util Timer TimerTask)))

(defn input [integrator k]
  (send integrator assoc :k k))

(defn output [integrator]
  (:s @integrator))

(defn tick [integrator t1]
  (send integrator
        (fn [{:keys [k s t0] :as m}]
          (assoc m :s (+ s (/ (* (+ (k t1) (k t0)) (- t1 t0)) 2.0)) :t0 t1))))

(defn start-timer [integrator interval]
  (let [timer (Timer. true)
        start (System/currentTimeMillis)]
    (.scheduleAtFixedRate timer
                          (proxy [TimerTask] []
                            (run [] (tick integrator (double (/ (- (System/currentTimeMillis) start) 1000)))))
                          (long 0)
                          (long interval))
    #(.cancel timer)))

(defn test-integrator []
  (let [integrator (agent {:k (constantly 0.0) :s 0.0 :t0 0.0})
        stop-timer (start-timer integrator 10)]
    (input integrator #(Math/sin (* 2.0 Math/PI 0.5 %)))
    (Thread/sleep 2000)
    (input integrator (constantly 0.0))
    (Thread/sleep 500)
    (println (output integrator))
    (stop-timer)))

user> (test-integrator)
1.414065859052494E-5
