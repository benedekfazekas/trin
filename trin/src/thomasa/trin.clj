(ns thomasa.trin
  (:require [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.subedit :as zsub]
            [clojure.zip :as clj-zip]))

;; could perhaps go to some testing utils
(defn all-zlocs
  "Generate a seq of all zlocs in a depth-first manner"
  [zipper]
  (take-while (complement zip/end?) (iterate zip/next zipper)))

(defn attach-ast-info
  "Attach info to the node under the key `:ast-info` and the suppliend `ast-key`.
  `ast-fn-value` should accept the original value under `[:ast-info ast-key]`."
  [loc ast-key ast-value-fn]
  (clj-zip/edit loc (fn [m] (update-in m [:ast-info ast-key] ast-value-fn))))

(defn- add-to-locals
  "Assocs the sexpr of `init-node` under the sexpr of `binding-node` as key into
  the locals atom."
  [binding-node init-node locals]
  (swap! locals assoc (zip/sexpr binding-node) (zip/sexpr init-node)))

(declare parse-loc)

(defn- parse-bindings*
  [locals env node]
  (let [binding node
        init (->> (merge env {:locals @locals})
                  (partial parse-loc)
                  (zsub/subedit-node (zip/right binding)))]
    (add-to-locals binding init locals)
    (if-let [next-binding (zip/right init)]
      (parse-bindings* locals env next-binding)
      init)))

(defn- parse-bindings
  "Parses a binding form eg. `[a (some sexpr) b (some other sexpr)]`.
  Calls `add-to-locals` to modify value of `locals` atom to record locals
  defined in binding. Init sexps are parsed by calling `parse-loc`."
  [locals env node]
  (if-let [first-binding (zip/down node)]
    (zip/up (parse-bindings* locals env first-binding))
    node))

(defn- parse-let-body*
  [env node]
  (let [node (zsub/subedit-node node (partial parse-loc env))]
    (if-let [next-body-loc (zip/right node)]
      (parse-let-body* env next-body-loc)
      node)))

(defn- parse-let-body
  "Parses let body by calling `parse-loc` on every sexpr in let body sequentially.
  Expect node to point to the let binding vector."
  [locals env node]
  (if-let [first-body-loc (zip/right node)]
    (parse-let-body* (merge env {:locals @locals}) first-body-loc)
    node))

(defn parse-let-loc
  "Parses a let expression."
  [env loc-let]
  (let [locals (atom (:locals env))]
    (-> (zip/down loc-let)
        zip/right
        (zsub/subedit-node (partial parse-bindings locals env))
        ((partial parse-let-body locals env))
        zip/up)))

(defn- skip-to-bottom
  "Skips to the bottom to the given loc in terms of setting the zipper to the
  deepest rightmost node."
  [loc]
  (or (some-> (zip/down loc)
              zip/rightmost
              skip-to-bottom)
      (zip/rightmost loc)))

(defn- let-loc?
  "Is `node` a let sexpr?"
  [node]
  (and (zip/seq? node)
       (= 'let (zip/sexpr (zip/down node)))))

(defn- locals-contains?
  "Does `locs` contain node's sexpr?"
  [locs node]
  ((set (keys locs)) (zip/sexpr node)))

(defn- resolve-init
  "Tries to resolve `init-k` recusively in terms of resolving the resolved value
  as key."
  [locs init-k]
  (when-let [init-v (locs init-k)]
    (or (resolve-init locs init-v)
        init-v)))

(defn parse-node
  "Parses a node with `env`."
  [env node]
  (let [locs (:locals env)]
    (cond-> node

      (locals-contains? locs node)
      (-> (attach-ast-info :op (constantly :local))
          (attach-ast-info :local (constantly :let))
          (attach-ast-info :init (constantly (locs (zip/sexpr node))))
          (attach-ast-info :init-resolved (constantly (resolve-init locs (zip/sexpr node)))))

      :always
      (attach-ast-info :env (fn [env] (assoc env :locals locs))) ;; or update with merge?!

      (let-loc? node)
      (-> (zsub/subedit-node (partial parse-let-loc env))
          skip-to-bottom))))

(defn parse-loc
  "Parses loc by walking it and calling `parse-node` on every node."
  [env loc]
  (zip/prewalk
   loc
   (partial parse-node env)))

;; -- empty things: empty let bindings, empty let body
