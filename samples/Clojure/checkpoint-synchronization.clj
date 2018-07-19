(ns checkpoint.core
  (:gen-class)
  (:require [clojure.core.async :as async :refer [go <! >! <!! >!! alts! close!]]
            [clojure.string :as string]))

(defn coordinate [ctl-ch resp-ch combine]
  (go
    (<! (async/timeout 2000)) ;delay a bit to allow worker setup
    (loop [members {}, received {}] ;maps by in-channel of out-channels & received data resp.
      (let [rcvd-count (count received)
            release   #(doseq [outch (vals members)] (go (>! outch %)))
            received  (if (and (pos? rcvd-count) (= rcvd-count (count members)))
                        (do (-> received vals combine release) {})
                        received)
            [v ch] (alts! (cons ctl-ch (keys members)))]
              ;receive a message on ctrl-ch or any member input channel
        (if (= ch ctl-ch)
          (let [[op inch outch] v] ;only a Checkpoint (see below) sends on ctl-ch
            (condp = op
              :join (do (>! resp-ch :ok)
                        (recur (assoc members inch outch) received))
              :part (do (>! resp-ch :ok)
                        (close! inch) (close! outch)
                        (recur (dissoc members inch) (dissoc received inch)))
              :exit :exit))
          (if (nil? v) ;is the channel closed?
            (do
              (close! (get members ch))
              (recur (dissoc members ch) (dissoc received ch)))
            (recur members (assoc received ch v))))))))

(defprotocol ICheckpoint
  (join [this])
  (part [this inch outch]))

(deftype Checkpoint [ctl-ch resp-ch sync]
  ICheckpoint
  (join [this]
    (let [inch (async/chan), outch (async/chan 1)]
      (go
        (>! ctl-ch [:join inch outch])
        (<! resp-ch)
        [inch outch])))
  (part [this inch outch]
    (go
      (>! ctl-ch [:part inch outch]))))

(defn checkpoint [combine]
  (let [ctl-ch (async/chan), resp-ch (async/chan 1)]
    (->Checkpoint ctl-ch resp-ch (coordinate ctl-ch resp-ch combine))))

(defn worker
  ([ckpt repeats] (worker ckpt repeats (fn [& args] nil)))
  ([ckpt repeats mon]
    (go
      (let [[send recv] (<! (join ckpt))]
        (doseq [n (range repeats)]
          (<! (async/timeout (rand-int 5000)))
          (>! send n) (mon "sent" n)
          (<! recv)  (mon "recvd"))
        (part ckpt send recv)))))


(defn -main
  [& args]
  (let [ckpt (checkpoint identity)
        monitor (fn [id]
                  (fn [& args] (println (apply str "worker" id ":" (string/join " " args)))))]
    (worker ckpt 10 (monitor 1))
    (worker ckpt 10 (monitor 2))))
