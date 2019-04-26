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

(defn- add-to-locals [binding-node init-node locals]
  (swap! locals assoc (zip/sexpr binding-node) (zip/sexpr init-node)))

(declare parse-let-loc)

(defn- parse-bindings [locals env node]
  (if-let [binding (and (not (zip/end? (zip/next node)))
                        (zip/next node))]
    (let [init (->> (merge env {:locals @locals})
                    (partial parse-let-loc)
                    (zsub/subedit-node (zip/next binding)))]
      (add-to-locals binding init locals)
      (parse-bindings locals env init))
    node))

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

               ((set (keys locs)) (zip/sexpr node))
               (-> (attach-ast-info :op (constantly :local))
                   (attach-ast-info :init (constantly (locs (zip/sexpr node)))))

               (and (= :vector (zip/tag node))
                    (= 'let (zip/sexpr (zip/left node))))
               (-> (zsub/subedit-node (partial parse-bindings locals env))
                   zip/down
                   zip/rightmost)

               ))))))

(defn parse-loc [env loc]
  (zip/prewalk
   loc
   (fn [node] (and (zip/seq? node)
                   (= 'let (zip/sexpr (zip/down node)))))
   (fn [node]
     (zsub/subedit-node node (partial parse-let-loc env)))))

;; - improve binding parsing
;;   - should be aware of already available locals (shadowing)
