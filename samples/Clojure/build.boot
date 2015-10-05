;; from: https://github.com/boot-clj/boot#configure-task-options

(set-env!
  :source-paths #{"src"}
  :dependencies '[[me.raynes/conch "0.8.0"]])

(task-options!
  pom {:project 'my-project
       :version "0.1.0"}
  jar {:manifest {"Foo" "bar"}})

(deftask build
  "Build my project."
  []
  (comp (pom) (jar) (install)))
