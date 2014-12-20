(set-env!
 :source-paths  #{"src"}
 :dependencies '[[org.clojure/clojure       "1.6.0"     :scope "provided"]
                 [boot/core                 "2.0.0-rc1" :scope "provided"]
                 [adzerk/bootlaces          "0.1.5"     :scope "test"]
                 [org.clojure/tools.logging "0.3.1"     :scope "test"]])

(require '[adzerk.bootlaces            :refer :all]
         '[clojure.test                :as    test :refer [deftest is run-tests]]
         '[adzerk.boot-logservice      :as    log-service]
         '[clojure.tools.logging       :as    log])

(def +version+ "1.0.0")

(bootlaces! +version+)

(ns-unmap 'boot.user 'test)
(deftask test []
  (with-pre-wrap fileset
    (let [log-dir (temp-dir!)
          config  [:configuration {:scan true, :scanPeriod "10 seconds"}
                   [:appender {:name "FILE" :class "ch.qos.logback.core.rolling.RollingFileAppender"}
                    [:encoder [:pattern "%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"]]
                    [:rollingPolicy {:class "ch.qos.logback.core.rolling.TimeBasedRollingPolicy"}
                     [:fileNamePattern (str (.getPath log-dir) "/%d{yyyy-MM-dd}.%i.log")]
                     [:timeBasedFileNamingAndTriggeringPolicy {:class "ch.qos.logback.core.rolling.SizeAndTimeBasedFNATP"}
                      [:maxFileSize "64 MB"]]]
                    [:prudent true]]
                   [:root {:level "INFO"}
                    [:appender-ref {:ref "FILE"}]]]]
      (alter-var-root #'log/*logger-factory* (constantly (log-service/make-factory config)))
      (deftest log-file-created
        (log/info "This should result in a log file being created on disk.")
        (let [logs (.listFiles log-dir)]
          (is (= 1 (count logs)))
          (println "Log file content:" (slurp (first logs))))))
    (let [report (run-tests 'boot.user)]
      (if-not (every? zero? (map report [:fail :error]))
        (throw (ex-info "Tests failed" report))))
    fileset))

(task-options!
 pom  {:project     'adzerk/boot-logservice
       :version     +version+
       :description "Carefree Logback logging in boot projects"
       :url         "https://github.com/adzerk/boot-logservice"
       :scm         {:url "https://github.com/adzerk/boot-logservice"}
       :license     {:name "Eclipse Public License"
                     :url  "http://www.eclipse.org/legal/epl-v10.html"}})
