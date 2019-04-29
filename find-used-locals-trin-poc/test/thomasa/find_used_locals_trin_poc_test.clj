(ns thomasa.find-used-locals-trin-poc-test
  (:require [clojure.test :as t]
            [thomasa.find-used-locals-trin-poc :as sut]))

(t/deftest a-test
  (t/testing "I pass"
    (t/is (= 1 1)))
  (t/testing "depending on local trin"
    (t/is (= :foo (sut/foo :ignore)))))

(def no-args-defn-with-let
  "(defn not-doing-much []
    (map name [:a :b :c])
    (let [prefix \"prefix-\"
          postfix \"-postfix\"]
      (->> (map name [:a :b :c])
           (map #(str prefix %)))))")

(t/deftest test-simple-let
  (t/is (= '() (sut/find-used-locals no-args-defn-with-let 1 10))
        "Used locals not empty outside let.")
  (t/is (= '() (sut/find-used-locals no-args-defn-with-let 3 12))
        "Used locals not empty in bindings form of let")
  (t/is (= '(prefix) (sut/find-used-locals no-args-defn-with-let 5 21))
        "Used local should be symbol prefix and only prefix"))
