#!/usr/bin/env bb

(def filter-regex "\\.(clj\\|cljs\\|cljc)$")

(defn clojure-source? [path]
  (re-find #"\.(clj|cljs|cljc)$" path))

(defn modified-files []
  (-> (shell/sh "git" "diff" "--cached" "--name-only" "--diff-filter=ACMR")
      :out
      (str/split #"\n")))

(defn lint-valid? [paths]
  (apply shell/sh "clj-kondo" "--lint" paths))

(defn native-cljstyle? []
  (-> (shell/sh "which" "cljstyle") :exit zero?))

(defn format-files [paths]
  (if (native-cljstyle?)
    (do
      (println "Using native cljstyle...")
      (apply shell/sh "cljstyle" "fix" paths))
    (do
      (println "Using cljstyle from clojure deps...")
      (apply shell/sh "clojure" "-A:format" "fix" paths))))

(defn update-file-index
  "Add unstaged modifications to git, so they get to be part of the current commit."
  [path]
  (let [hash (:out (shell/sh "git" "hash-object" "-w" path))]
    (shell/sh "git" "update-index" "--add" "--cacheinfo" "100644" hash path)))

(let [paths (->> (modified-files)
                 (filter clojure-source?))]
  (when (seq paths)
    (format-files paths)

    (doseq [path paths]
      (update-file-index path))

    (let [{:keys [exit out]} (lint-valid? paths)]
      (when-not (= 0 exit)
        (println "Lint failed.\n" out)
        (System/exit 1)))))
