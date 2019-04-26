(ns thomasa.trin
  (:require [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.subedit :as zsub]
            [clojure.zip :as clj-zip]))

(defn foo
  "I don't do a whole lot."
  []
  (println "Hello, World!")
  :foo)

(defn all-zlocs
  "Generate a seq of all zlocs in a depth-first manner"
  [zipper]
  (take-while (complement zip/end?) (iterate zip/next zipper)))

(defn attach-ast-info [loc ast-key ast-value-fn]
  (clj-zip/edit loc (fn [m] (update-in m [:ast-info ast-key] ast-value-fn))))

(defn- add-to-locals [node locals]
  (swap! locals conj (zip/sexpr node))
  node)

(defn parse-let-loc [env loc-let]
  (let [locals (atom (:locals env))]
      (zip/prewalk
       loc-let
       (fn [node]
         (println (zip/sexpr node))
         (let [locs @locals]
             (cond-> node

               :always
               (attach-ast-info :env (fn [env] (assoc env :locals locs)))

               (locs (zip/sexpr node))
               (attach-ast-info :op (constantly :local))

               (and (= :vector (zip/tag (zip/up node)))
                    (= 'let (zip/sexpr (zip/left (zip/up node))))
                    (symbol? (zip/sexpr node)); good for now
                    )
               (add-to-locals locals)

               ))))))

(defn parse-loc [loc]
  (zip/prewalk
   loc
   (fn [node] (and (zip/seq? node)
                   (= 'let (zip/sexpr (zip/down node)))))
   (fn [node]
     (let [env (or (-> (first node) :ast-info :env)
                   {:locals #{}})]
         (zsub/subedit-node node (partial parse-let-loc env))))))

;; - improve binding parsing
;;   - local should not be available yet in init sexpr
;;   - should record, and parse (!) init sexpr
;;   - should be aware of already available locals (shadowing)
