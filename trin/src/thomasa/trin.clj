(ns thomasa.trin
  (:require [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.subedit :as zsub]
            [clojure.zip :as clj-zip]))

;; could perhaps go to some testing utils
(defn all-zlocs
  "Generate a seq of all zlocs in a depth-first manner"
  [zipper]
  (take-while (complement zip/end?) (iterate zip/next zipper)))

(defn- let-loc?
  "Is `node` a let sexpr?"
  [node]
  (and (zip/seq? node)
       (= 'let (zip/sexpr (zip/down node)))))

(defn- defn-loc?
  [node]
  (and (zip/seq? node)
       (#{'defn 'defn- 'fn 'fn*} (zip/sexpr (zip/down node)))))

(defn- locals-contains?
  "Does `locs` contain node's sexpr?"
  [locs node]
  ((set (keys locs)) (zip/sexpr node)))

(defn attach-ast-info
  "Attach info to the node under the key `:ast-info` and the suppliend `ast-key`.
  `ast-fn-value` should accept the original value under `[:ast-info ast-key]`."
  [loc ast-key ast-value-fn]
  (clj-zip/edit loc (fn [m] (update-in m [:ast-info ast-key] ast-value-fn))))

(defn- add-to-locals
  "Assocs `local-info` under the sexpr of `binding-node` as key into
  the locals atom. The shape of data stored under the key therefore depends on
  the type of local."
  [binding-node local-info locals]
  (swap! locals assoc (zip/sexpr binding-node) local-info))

(defn- resolve-init
  "Tries to resolve `init-k` recusively in terms of resolving the resolved value
  as key."
  [locs init-k]
  (when-let [init-v (and locs (:init (locs init-k)))]
    (or (resolve-init locs init-v)
        init-v)))

(declare analyze-loc)

(defn- analyze-bindings*
  [locals env node]
  (let [binding    node
        init       (->> (merge env {:locals @locals})
                        (partial analyze-loc)
                        (zsub/subedit-node (zip/right binding)))
        init-sexpr (zip/sexpr init)
        local-info {:op            :local
                    :local         :let
                    :init          init-sexpr
                    :init-resolved (resolve-init @locals init-sexpr)}]
    (add-to-locals binding local-info locals)
    (if-let [next-binding (zip/right init)]
      (analyze-bindings* locals env next-binding)
      init)))

(defn- analyze-bindings
  "Analyzes a binding form eg. `[a (some sexpr) b (some other sexpr)]`.
  Calls `add-to-locals` to modify value of `locals` atom to record locals
  defined in binding. Init sexps are analyzed by calling `analyze-loc`."
  [locals env node]
  (if-let [first-binding (zip/down node)]
    (zip/up (analyze-bindings* locals env first-binding))
    node))

(defn- analyze-sexprs-in-do*
  [env node]
  (let [node (zsub/subedit-node node (partial analyze-loc env))]
    (if-let [next-body-loc (zip/right node)]
      (analyze-sexprs-in-do* env next-body-loc)
      node)))

(defn- analyze-sexprs-in-do
  "Analyzes an (implicit) do body by calling `analyze-loc` on every sexpr in the body
  sequentially.
  Expect node to point to the sexpr to the left of the do. That would be the binding
  vector for a `let`"
  [locals env node]
  (if-let [first-body-loc (zip/right node)]
    (analyze-sexprs-in-do* (merge env {:locals @locals}) first-body-loc)
    node))

(defn analyze-let-loc
  "Analyzes a let expression."
  [env loc-let]
  (let [locals (atom (:locals env))]
    (-> (zip/down loc-let)
        zip/right
        (zsub/subedit-node (partial analyze-bindings locals env))
        ((partial analyze-sexprs-in-do locals env))
        zip/up)))

(defn- skip-to-bottom
  "Skips to the bottom to the given loc in terms of setting the zipper to the
  deepest rightmost node."
  [loc]
  (or (some-> (zip/down loc)
              zip/rightmost
              skip-to-bottom)
      (zip/rightmost loc)))

(defn- analyze-args* [locals env arg arg-index]
  (add-to-locals
   arg
   {:op     :local
    :local  :arg
    :arg-id arg-index}
   locals)
  (if-let [next-arg (zip/right arg)]
    (analyze-args* locals env next-arg (inc arg-index))
    arg))

(defn- analyze-args
  [locals env args-loc]
  (if-let [first-arg (zip/down args-loc)]
    (zip/up (analyze-args* locals env first-arg 0))
    args-loc))

(defn- analyze-fn-loc
  [env loc-fn]
  (let [locals (atom (:locals env))]
    (-> (zip/down loc-fn)
        (zip/find (fn [node] (= :vector (zip/tag node))))
        (zsub/subedit-node (partial analyze-args locals env))
        ((partial analyze-sexprs-in-do locals env))
        zip/up)))

(defn- decorate-local-node [node locs]
  (reduce
   (fn [node [loc-k loc-v]]
     (attach-ast-info node loc-k (constantly loc-v)))
   node
   (locs (zip/sexpr node))))

(defn analyze-node
  "Analyzes a node with `env`."
  [env node]
  (let [locs (:locals env)]
    (cond-> node

      (locals-contains? locs node)
      (decorate-local-node locs)

      :always
      (attach-ast-info :env (fn [env] (assoc env :locals locs))) ;; or update with merge?!

      (defn-loc? node)
      (-> (zsub/subedit-node (partial analyze-fn-loc env))
          skip-to-bottom)

      (let-loc? node)
      (-> (zsub/subedit-node (partial analyze-let-loc env))
          skip-to-bottom))))

(defn analyze-loc
  "Analyzes loc by walking it and calling `analyze-node` on every node."
  [env loc]
  (zip/prewalk
   loc
   (partial analyze-node env)))

;; -- empty things: empty let bindings, empty let body
