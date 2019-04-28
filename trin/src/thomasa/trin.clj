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

(declare parse-loc)

(defn- parse-bindings* [locals env node]
  (let [binding node
        init (->> (merge env {:locals @locals})
                  (partial parse-loc)
                  (zsub/subedit-node (zip/right binding)))]
    (add-to-locals binding init locals)
    (if-let [next-binding (zip/right init)]
      (parse-bindings* locals env next-binding)
      init)))

(defn- parse-bindings [locals env node]
  (if-let [first-binding (zip/down node)]
    (zip/up (parse-bindings* locals env first-binding))
    node))

(defn- parse-let-body* [env node]
  (let [node (zsub/subedit-node node (partial parse-loc env))]
    (if-let [next-body-loc (zip/right node)]
      (parse-let-body* env next-body-loc)
      node)))

(defn- parse-let-body [locals env node]
  (if-let [first-body-loc (zip/right node)]
    (parse-let-body* (merge env {:locals @locals}) first-body-loc)
    node))

(defn parse-let-loc [env loc-let]
  (let [locals (atom (:locals env))]
    (-> (zip/down loc-let)
        zip/right
        (zsub/subedit-node (partial parse-bindings locals env))
        ((partial parse-let-body locals env))
        zip/up)))

(defn- skip-to-bottom [loc]
  (or (some-> (zip/down loc)
              zip/rightmost
              skip-to-bottom)
      (zip/rightmost loc)))

(defn- let-loc?
  [node]
  (and (zip/seq? node)
       (= 'let (zip/sexpr (zip/down node)))))

(defn- locals-contains?
  [locs node]
  ((set (keys locs)) (zip/sexpr node)))

(defn parse-node [env node]
  (let [locs (:locals env)]
    (cond-> node

      (locals-contains? locs node)
      (-> (attach-ast-info :op (constantly :local))
          (attach-ast-info :init (constantly (locs (zip/sexpr node)))))

      :always
      (attach-ast-info :env (fn [env] (assoc env :locals locs))) ;; or update with merge?!

      (let-loc? node)
      (-> (zsub/subedit-node (partial parse-let-loc env))
          skip-to-bottom))))

(defn parse-loc [env loc]
  (zip/prewalk
   loc
   (partial parse-node env)))

;; -- empty things: empty let bindings, empty let body
