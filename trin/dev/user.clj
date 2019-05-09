(ns user
  (:require [thomasa.trin :as trin]
            [rewrite-clj.zip :as zip]))

(defn print-ast-info [ast]
  (doseq [node (trin/all-zlocs ast)]
    (println "sexpr:" (zip/sexpr node) "\n  ast:" (pr-str (:ast-info (first node))))
    (println)))
