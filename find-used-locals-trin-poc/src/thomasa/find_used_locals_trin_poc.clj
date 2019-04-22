(ns thomasa.find-used-locals-trin-poc
  (:require [thomasa.trin :as trin]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!")
  (trin/foo))
