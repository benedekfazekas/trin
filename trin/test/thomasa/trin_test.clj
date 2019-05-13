(ns thomasa.trin-test
  (:require [clojure.test :as t]
            [rewrite-clj.zip :as zip]
            [thomasa.trin :as trin]))

(def embedded-let
  "(do
  (println \"foobar\" )
  (let [a 1
        b a
        c 3]
    (println a)
    [b c])
  (println \"baz\"))")

(def let-in-let
  "(do
  (println \"foobar\" )
  (let [a 1
        b a
        c 3]
    (println a)
    (let [c (inc c)]
      [b c]))
  (println \"baz\"))")

(def defn-with-arg
  "(defn not-much-on-kw [kw]
        (println \"kw:\" (name kw)))")

(def defn-with-varargs
  "(defn not-much-on-kws [kw & other-kws]
        (println \"kw:\" (name kw) \"others: \" (map name other-kws)))")

(def defn-with-map-destruct-as-defaults-varargs
  "(defn fn-with-default-optmap-linebreak
  [{:keys [foo
           bar]
    :as all-the-things
    :or {foo
         \"foo\"}}
   & rest-args]
  [:bar :foo]
  (count foo))")

(t/deftest a-test
  (t/testing "FIXME, I don't test much"
    (t/is (= 1 1))))

(t/deftest use-rewrite-clj-node-as-AST
  (t/is (= "foo" (-> (zip/of-string "[1 2 3]")
                     (#'trin/attach-ast-info :foo (constantly "foo"))
                     first
                     :ast-info
                     :foo))
        "Failed to attach data to rewrite-clj node")
  (t/is (= [2 "foo"] (-> (zip/of-string "[1 2 3]")
                         zip/down
                         zip/right
                         (#'trin/attach-ast-info :foo (constantly "foo"))
                         zip/up
                         zip/root
                         zip/edn
                         trin/all-zlocs
                         (nth 2)
                         ((juxt zip/sexpr (comp :foo :ast-info first)))))
        "Failed to make attached data persistent across zipper movements"))

(t/deftest analyze-let-loc-test
  (t/testing "test let embedded into a do"
      (let [embedded-let-with-locals (trin/analyze-loc {} (zip/of-string embedded-let))
            embedded-let-locs        (trin/all-zlocs embedded-let-with-locals)]
        (t/is (= '#{a b c} (-> embedded-let-with-locals
                               zip/down
                               zip/right
                               zip/right
                               zip/down
                               zip/right
                               zip/right
                               first
                               :ast-info
                               :env
                               :locals
                               keys
                               set))
              "Failed to add locals to env.")
        (t/is (= 4 (count (filter (comp #{:local} :op :ast-info first) embedded-let-locs)))
              "Failed to identify all op-locals")))

  (t/testing "test let in let, effectively shadowing"
    (let [let-in-let-locs (->> (zip/of-string let-in-let)
                               (trin/analyze-loc {})
                               trin/all-zlocs)
          op-local-locs   (filter (comp #{:local} :op :ast-info first) let-in-let-locs)]
      (t/is (= 5 (count op-local-locs))
            "Failed to identify all op-locals")
      (t/is (= 3 (-> (drop 2 op-local-locs)
                     ffirst
                     :ast-info
                     :init))
            "Init in outside let for local is incorrect")
      (let [let-in-let-c-ast (-> (last op-local-locs)
                                 first
                                 :ast-info)]
        (t/is (= '(inc c) (:init let-in-let-c-ast))
              "Reference to c local is not shadowed in let in let")
        (t/is (= '{a {:op :local, :local :let, :init 1,       :init-resolved nil},
                   b {:op :local, :local :let, :init a,       :init-resolved 1},
                   c {:op :local, :local :let, :init (inc c), :init-resolved nil}}
                 (:locals (:env let-in-let-c-ast)))
              "Failed to shadow locals")))))

(t/deftest analyze-defn-with-arg-test
  (t/testing "test simple defn with an arg"
    (let [defn-with-arg-locs (->> (zip/of-string defn-with-arg)
                                  (trin/analyze-loc {})
                                  trin/all-zlocs)
          arg-local-in-env   (some (comp :locals :env :ast-info first) defn-with-arg-locs)
          arg-local-node     (first (filter (comp #{:local} :op :ast-info first) defn-with-arg-locs))]
      (t/is (= {'kw {:op     :local
                     :arg-id 0
                     :local  :arg}} arg-local-in-env)
            "Failed to attach locals based on defn arg.")
      (t/is (= {:op     :local
                :arg-id 0
                :local  :arg}
               (-> (first arg-local-node)
                   :ast-info
                   (dissoc :env)))
            "Failed to mark reference to arg in defn body."))))

(t/deftest analyze-defn-with-varargs-test
  (t/testing "test defn with varargs"
    (let [defn-with-varargs-locs (->> (zip/of-string defn-with-varargs)
                                      (trin/analyze-loc {})
                                      (trin/all-zlocs))
          arg-local-in-env   (some (comp :locals :env :ast-info first) defn-with-varargs-locs)
          arg-local-node     (filter (comp #{:local} :op :ast-info first) defn-with-varargs-locs)]
      (t/is (= {'kw        {:op     :local
                            :arg-id 0
                            :local  :arg}
                'other-kws {:op        :local
                            :arg-id    0
                            :local     :arg
                            :variadic? true}} arg-local-in-env)
            "Failed to attach locals based on defn arg and varargs.")
      (t/is (= {:op     :local
                :arg-id 0
                :local  :arg}
               (-> (ffirst arg-local-node)
                   :ast-info
                   (dissoc :env)))
            "Failed to mark reference to arg in defn body.")
      (t/is (= {:op        :local
                :arg-id    0
                :variadic? true
                :local     :arg}
               (-> (last arg-local-node)
                   first
                   :ast-info
                   (dissoc :env)))
            "Failed to mark reference to varargs coll in defn body."))))

(t/deftest analyze-defn-with-map-destruct-as-defaults-varargs-test
  (t/testing "map destructuring, defaults, as, varargs"
    (let [defn-locs        (->> (zip/of-string defn-with-map-destruct-as-defaults-varargs)
                                (trin/analyze-loc {})
                                (trin/all-zlocs))
          arg-local-in-env (some (comp :locals :env :ast-info first) defn-locs)
          arg-local-node   (filter (comp #{:local} :op :ast-info first) defn-locs)]
      (t/is (=
             {'all-the-things {:arg-id 0, :as? true, :local :arg, :op :local},
              'bar            {:arg-id 0, :local :arg, :op :local},
              'foo            {:arg-id 0, :default-value "foo", :local :arg, :op :local},
              'rest-args      {:arg-id 0, :local :arg, :op :local, :variadic? true}}
             arg-local-in-env)
            "Failed to recognise all locals in args.")
      (t/is (= {:op            :local
                :arg-id        0
                :local         :arg
                :default-value "foo"}
               (-> (ffirst arg-local-node)
                   :ast-info
                   (dissoc :env)))
            "Failed to mark reference to arg in defn body."))))
