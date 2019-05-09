(ns thomasa.find-used-locals-trin-poc
  (:require [rewrite-clj.zip :as zip]
            [rewrite-clj.zip.subedit :as zip-subedit]
            [thomasa.trin :as trin]
            [clojure.set :as set]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!")
  (trin/analyze-loc {} (zip/of-string "[1 2 3]"))
  :foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; borrowed from refactor-nrepl for now
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn all-zlocs
  "Generate a seq of all zlocs in a depth-first manner"
  [zipper]
  (take-while (complement zip/end?) (iterate zip/next zipper)))

(defn- node-at-loc?
  "True if node encloses point defined by `loc-line` and `loc-column`."
  [zloc ^long loc-line ^long loc-column]
  (let [[line end-line column end-column] (->> (zip/node zloc)
                                               meta
                                               ((juxt :row :end-row :col :end-col))
                                               (map (comp dec long)))]
    (or (< line loc-line end-line)
        (and (or (= line loc-line)
                 (= end-line loc-line))
             (<= column loc-column end-column)))))

(defn- zip-to
  "Move the zipper to the node at `loc-line` and `loc-col`.

  Implementation uses `all-zlocs` and exploits the fact that it generates
  a seq of nodes in depth-first order."
  [zipper ^long loc-line ^long loc-column]
  (reduce
   (fn [node-at-loc zloc]
     (if (node-at-loc? zloc loc-line loc-column) zloc node-at-loc))
   zipper
   (all-zlocs zipper)))

(defn get-enclosing-sexp
  "Extracts the sexp enclosing point at LINE and COLUMN in FILE-CONTENT,
  and optionally LEVEL.

  A string is not treated as a sexp by this function. If LEVEL is
  provided finds the enclosing sexp up to level. LEVEL defaults to 1
  for the immediate enclosing sexp.

  Both line and column are indexed from 0."
  ([parsed-content line column]
   (get-enclosing-sexp parsed-content line column 1))
  ([parsed-content ^long line ^long column ^long level]
   (let [zloc (zip-to parsed-content line column)
         zloc (nth (iterate zip/up zloc) (dec level))]
     (zip-subedit/subzip
      (cond
        (and zloc (string? (zip/sexpr zloc))) (zip/up zloc)
        (and zloc (seq? (zip/sexpr zloc)))    zloc
        zloc                                  (zip/up zloc)
        :else                                 (throw (ex-info "Can't find sexp boundary"
                                                              {:file-content (zip/sexpr parsed-content)
                                                               :line         line
                                                               :column       column})))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; borrowed from refactor-nrepl ends
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn find-used-locals
  "Adapted version of refactor-nrepl's `refactor-nrepl.find.find-locals/find-used-locals`."
  [source ^long line ^long column]
  (let [analyzed-source  (trin/analyze-loc {} (zip/of-string source))
        selected-sexp    (get-enclosing-sexp analyzed-source line column)
        locals-in-use    (->> (all-zlocs selected-sexp)
                              (filter #(= :local (get-in % [0 :ast-info :op])))
                              (map zip/sexpr)
                              set)
        available-locals (-> selected-sexp first :ast-info :env :locals keys set)]
    (println "selected sexp"   (zip/sexpr selected-sexp)
             " locals in use"  locals-in-use
             " avail locals: " available-locals)
    (-> (set/intersection available-locals locals-in-use)
        (filter available-locals)
        set)))
