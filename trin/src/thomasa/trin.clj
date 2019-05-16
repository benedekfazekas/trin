(ns thomasa.trin
  (:require [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.subedit :as zsub]
            [clojure.zip :as clj-zip]))

;; zip utils
(defn all-zlocs
  "Generate a seq of all zlocs in a depth-first manner"
  [zipper]
  (take-while (complement zip/end?) (iterate zip/next zipper)))

(defn- skip-to-bottom
  "Skips to the bottom to the given loc in terms of setting the zipper to the
  deepest rightmost node."
  [loc]
  (or (some-> (zip/down loc)
              zip/rightmost
              skip-to-bottom)
      (zip/rightmost loc)))

(defn- skip-over
  "Skips over loc to the beginning of the next loc if available."
  [loc]
  (zip/next (skip-to-bottom loc)))

;; predicates
(defn- let-like-loc?
  "Is `node` a let sexpr?"
  [node]
  (and (zip/seq? node)
       (#{'let 'loop 'loop* 'if-let 'when-let 'letfn 'for :let} (zip/sexpr (zip/down node)))))

(defn- fn-loc?
  "Is `node` an fn sexpr?"
  [node]
  (and (zip/seq? node)
       (#{'defn 'defn- 'fn 'fn*} (zip/sexpr (zip/down node)))))

(defn- locals-contains?
  "Does `locs` contain node's sexpr?"
  [locs node]
  ((set (keys locs)) (zip/sexpr node)))

;; ast info manipulation
(def analyzed->local
  {'let      :let
   :let      :let
   'let*     :let
   'when-let :let
   'if-let   :let
   'letfn    :let
   'for      :for
   'loop     :loop
   'loop*    :loop
   'defn     :arg
   'defn-    :arg
   'fn       :arg
   'fn*      :arg})

(defn- attach-ast-info
  "Attach info to the node under the key `:ast-info` and the suppliend `ast-key`.
  `ast-fn-value` should accept the original value under `[:ast-info ast-key]`."
  [loc ast-key ast-value-fn]
  (clj-zip/edit loc (fn [m] (update-in m [:ast-info ast-key] ast-value-fn))))

(defn- add-to-locals
  "Assocs `local-info` under the sexpr of `binding-node` as key into
  the locals atom. The shape of data stored under the key therefore depends on
  the type of local."
  [bound-sym-node local-info locals]
  (swap! locals assoc (zip/sexpr bound-sym-node) local-info))

(defn- resolve-init
  "Tries to resolve `init-k` recusively in terms of resolving the resolved value
  as key."
  [locs init-k]
  (when-let [init-v (and locs (:init (locs init-k)))]
    (or (resolve-init locs init-v)
        init-v)))

(defn- decorate-local-node [node locs]
  (reduce
   (fn [node [loc-k loc-v]]
     (attach-ast-info node loc-k (constantly loc-v)))
   node
   (locs (zip/sexpr node))))

;; analyze handlers
(declare analyze-form)
(declare analyze-vector-of-locals)

(defn- analyze-defaults-map
  "Analyzes a defaults map for desctructuring."
  [locals defaults-key]
  (let [defaults-value (zip/right defaults-key)]
    (swap! locals
           assoc-in
           [(zip/sexpr defaults-key) :default-value]
           (zip/sexpr defaults-value))
    (when-let [next-defaults-key (zip/right defaults-value)]
      (analyze-defaults-map locals next-defaults-key))))

(defn- analyze-map-desctructuring* [locals env k local-info]
  (let [v         (zip/right k)
        key-sexpr (zip/sexpr k)]
    (cond

      (and (map? key-sexpr) (zip/down k))
      (analyze-map-desctructuring* locals env (zip/down k) local-info)

      (#{:keys :strs :syms} key-sexpr)
      (analyze-vector-of-locals locals (dissoc env :analyzing) (zip/down v) local-info)

      (#{:or} key-sexpr)
      (analyze-defaults-map locals (zip/down v))

      (#{:as} key-sexpr)
      (add-to-locals
       v
       (-> (assoc local-info :op :local)
           (assoc :as? true))
       locals)

      :default
      (add-to-locals
       k
       (assoc local-info :op :local)
       locals))
    (if-let [next-k (zip/right v)]
      (analyze-map-desctructuring* locals env next-k local-info)
      v)))

(defn- analyze-map-desctructuring
  "Analyzes destructuring of maps."
  [locals env arg local-info]
  (if-let [first-key (zip/down arg)]
    (zip/up (analyze-map-desctructuring* locals env first-key local-info))
    arg))

(defn- analyze-destructuring
  "Analyzes destructuring."
  [locals env arg arg-sexpr local-info]
  (cond
    (vector? arg-sexpr)
    (when-let [first-arg (zip/down arg)]
      (analyze-vector-of-locals locals env first-arg local-info))

    (map? arg-sexpr)
    (analyze-map-desctructuring locals env arg local-info)))

(defn- analyze-bindings*
  [locals env node]
  (let [binding       node
        binding-sexpr (zip/sexpr node)
        init          (->> (merge env {:locals @locals})
                           (partial analyze-form)
                           (zsub/subedit-node (zip/right binding)))
        init-sexpr    (zip/sexpr init)
        local-info    {:op            :local
                       :local         (analyzed->local (:analyzing env) :undefined)
                       :init          init-sexpr
                       :init-resolved (resolve-init @locals init-sexpr)}]
    (cond
      (= :let binding-sexpr)
      (analyze-bindings* locals env (zip/down (zip/right node)))

      (symbol? binding-sexpr)
      (add-to-locals binding local-info locals)

      :default
      (analyze-destructuring locals env binding binding-sexpr local-info))
    (if-let [next-binding (zip/right init)]
      (analyze-bindings* locals env next-binding)
      init)))

(defn- analyze-bindings
  "Analyzes a binding form eg. `[a (some sexpr) b (some other sexpr)]`.
  Calls `add-to-locals` to modify value of `locals` atom to record locals
  defined in binding. Init sexps are analyzed by calling `analyze-form`."
  [locals env node]
  (if-let [first-binding (zip/down node)]
    (zip/up (analyze-bindings* locals env first-binding))
    node))

(defn- analyze-sexprs-in-do*
  [env node]
  (let [node (zsub/subedit-node node (partial analyze-form env))]
    (if-let [next-body-loc (zip/right node)]
      (analyze-sexprs-in-do* env next-body-loc)
      node)))

(defn- analyze-sexprs-in-do
  "Analyzes an (implicit) do body by calling `analyze-form` on every sexpr in the body
  sequentially.

  Expect node to point to the sexpr to the left of the do. That would be the binding
  vector for a `let`."
  [locals env node]
  (if-let [first-body-loc (zip/right node)]
    (analyze-sexprs-in-do* (merge env {:locals @locals}) first-body-loc)
    node))

(defn- analyze-let-loc
  "Analyzes a let expression."
  [env loc-let]
  (let [locals (atom (:locals env))]
    (-> (zip/down loc-let)
        zip/right
        (zsub/subedit-node (partial analyze-bindings locals env))
        ((partial analyze-sexprs-in-do locals env))
        zip/up)))

(defn- ->rest-seq-key [env]
  (or (and (= :arg (analyzed->local (:analyzing env) :undefined))
           :variadic?)
      :rest-seq?))

(defn- analyze-vector-of-locals
  "Analyzes a vector of locals either in arguments or in desctructuring."
  [locals env arg local-info]
  (let [arg-sexpr (zip/sexpr arg)
        arg       (or (and (= '& arg-sexpr) (zip/right arg)) arg)]
    (cond
      (= '& arg-sexpr)
      (add-to-locals
       arg
       (-> (assoc local-info :op :local)
           (assoc (->rest-seq-key env) true))
       locals)

      (symbol? arg-sexpr)
      (add-to-locals
       arg
       (assoc local-info :op :local)
       locals)

      :default
      (analyze-destructuring locals env arg arg-sexpr local-info))
    (if-let [next-arg (zip/right arg)]
      (analyze-vector-of-locals
       locals
       env
       next-arg
       (or (and (:arg-id local-info)
                (= :arg (analyzed->local (:analyzing env) :undefined))
                (update local-info :arg-id inc))
           local-info))
      arg)))

(defn- analyze-args
  "Analyzes arguments vector of an fn form."
  [locals env args-loc]
  (if-let [first-arg (zip/down args-loc)]
    (zip/up
     (analyze-vector-of-locals
      locals
      env
      first-arg
      {:local  (analyzed->local (:analyzing env) :undefined)
       :arg-id 0}))
    args-loc))

(defn- analyze-fn-loc
  "Analyzes an fn loc."
  [env loc-fn]
  (let [locals (atom (:locals env))]
    (-> (zip/down loc-fn)
        (zip/find (fn [node] (= :vector (zip/tag node))))
        (zsub/subedit-node (partial analyze-args locals env))
        ((partial analyze-sexprs-in-do locals env))
        zip/up)))

(defn- prepare-env [node env]
  (assoc env :analyzing (or (fn-loc? node) (let-like-loc? node))))

(defn- analyze-node
  "Analyzes a node with `env`."
  [env node]
  (let [locs (:locals env)
        env  (prepare-env node env)]
    (cond-> node

      (locals-contains? locs node)
      (decorate-local-node locs)

      :always
      (attach-ast-info :env (fn [env] (assoc env :locals locs))) ;; or update with merge?!

      (fn-loc? node)
      (-> (zsub/subedit-node (partial analyze-fn-loc env))
          skip-to-bottom)

      (let-like-loc? node)
      (-> (zsub/subedit-node (partial analyze-let-loc env))
          skip-to-bottom))))

(defn analyze-form
  "Analyzes `loc` representing a first level form by walking it and calling `analyze-node` on every node."
  [env loc]
  (zip/prewalk
   loc
   (partial analyze-node env)))

(defn analyze-loc
  "Analyzes all first level forms in `loc`.

  Loc typically represents a namespace."
  [env loc]
  (loop [loc loc]
    (let [loc       (analyze-form env loc)
          next-loc  (skip-over loc)]
      (if-not (zip/end? next-loc)
        (recur next-loc)
        (zip/leftmost loc)))))

;; -- empty things: empty let bindings, empty let body
