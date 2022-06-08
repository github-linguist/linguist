#!/usr/bin/env bb

(ns tell-number
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.edn :refer [read-string]]
            [clojure.pprint :refer [cl-format]]))

(def cli-options
  [["-n" "--number NUMBER" "Number to tell"
    :parse-fn read-string
    :validate [number? "Must be a number"]]])

(def to-english
  (partial cl-format nil "~@(~@[~R~]~^ ~A.~)"))

(defn main- []
  (let [cli-options (:options (parse-opts *command-line-args* cli-options))
        {:keys [number]} cli-options]
    (println (to-english number))))

(main-)
