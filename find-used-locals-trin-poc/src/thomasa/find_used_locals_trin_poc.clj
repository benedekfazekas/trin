(ns thomasa.find-used-locals-trin-poc
  (:require [rewrite-clj.zip :as zip]
            [thomasa.trin :as trin]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!")
  (trin/parse-loc {} (zip/of-string "[1 2 3]"))
  :foo)
