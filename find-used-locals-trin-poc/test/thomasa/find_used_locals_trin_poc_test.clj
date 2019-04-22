(ns thomasa.find-used-locals-trin-poc-test
  (:require [clojure.test :as t]
            [thomasa.find-used-locals-trin-poc :as sut]))

(t/deftest a-test
  (t/testing "I pass"
    (t/is (= 1 1)))
  (t/testing "depending on local trin"
    (t/is (= :foo (sut/foo :ignore)))))
