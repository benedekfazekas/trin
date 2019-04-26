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

(t/deftest parse-let-loc-test
  (let [embedded-let-with-locals (trin/parse-loc (zip/of-string embedded-let))
        locs (trin/all-zlocs embedded-let-with-locals)]
    (t/is (= '#{a b c} (-> embedded-let-with-locals
                           zip/down
                           zip/right
                           zip/right
                           zip/down
                           zip/right
                           zip/right
                           ((fn [node] (println (zip/sexpr node)
                                                "--"
                                                (-> node first :ast-info))
                              node))
                           first
                           :ast-info
                           :env
                           :locals))
          "Failed to add locals to env.")
    (t/is (= 4 (count (filter (comp #{:local} :op :ast-info first) locs)))
          "Failed to identify all op-locals")))

;; test let in let with shadowing
