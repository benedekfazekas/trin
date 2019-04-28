(ns thomasa.trin-test
  (:require [clojure.test :as t]
            [rewrite-clj.zip :as zip]
            [thomasa.trin :as trin]))

(t/deftest a-test
  (t/testing "FIXME, I don't test much"
    (t/is (= 1 1))))

(t/deftest use-rewrite-clj-node-as-AST
  (t/is (= "foo" (-> (zip/of-string "[1 2 3]")
                     (trin/attach-ast-info :foo (constantly "foo"))
                     first
                     :ast-info
                     :foo))
        "Failed to attach data to rewrite-clj node")
  (t/is (= [2 "foo"] (-> (zip/of-string "[1 2 3]")
                         zip/down
                         zip/right
                         (trin/attach-ast-info :foo (constantly "foo"))
                         zip/up
                         zip/root
                         zip/edn
                         trin/all-zlocs
                         (nth 2)
                         ((juxt zip/sexpr (comp :foo :ast-info first)))))
        "Failed to make attached data persistent across zipper movements"))



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

(t/deftest parse-let-loc-test
  (t/testing "test let embedded into a do"
      (let [embedded-let-with-locals (trin/parse-loc {} (zip/of-string embedded-let))
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
                               (trin/parse-loc {})
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
        (t/is (= '{a 1 b a c (inc c)} (:locals (:env let-in-let-c-ast)))
              "Failed to shadow locals")))))
