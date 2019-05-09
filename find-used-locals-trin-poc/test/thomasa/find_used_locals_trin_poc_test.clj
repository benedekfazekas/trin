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

(def example-five
  "(ns com.example.five
  (:require [clojure.string :refer [join split blank? trim] :as str]))

;;  remove parameters to run the tests
(defn fn-with-unbounds [s sep]
  (when-not (blank? s)
    (-> s (split #\" \")
        (join sep)
        trim)))

(defn orig-fn [s]
  (let [sep \";\"]
    (when-not (blank? s)
      (-> s
          (split #\" \")
          ((partial join sep))
          trim))))

(defn find-in-let [s p]
  (let [z (trim p)]
    (assoc {:s s
            :p p
            :z z} :k \"foobar\")))

(defn threading-macro [strings]
  (let [sep \",\"]
    (->> strings
         flatten
         (join sep))))

(defn repeated-sexp []
  (map name [:a :b :c])
  (let [name #(str \"myname\" %)]
    (map name [:a :b :c])))

(defn sexp-with-anon-fn [n]
  (let [g 5]
    (#(+ g %) n)))

(defn many-params [x y z a b c]
  (* x y z a b c))

(defn fn-with-default-optmap
  [{:keys [foo bar] :or {foo \"foo\"}}]
  [:bar :foo]
  (count foo))

(defn fn-with-default-optmap-linebreak
  [{:keys [foo
           bar]
    :or {foo
         \"foo\"}}]
  [:bar :foo]
  (count foo))

(defn fn-with-let-default-optmap []
  (let [{:keys [foo bar] :or {foo \"foo\"}} (hash-map)]
    [:bar :foo]
    (count foo)))
")

(t/deftest test-simple-let
  (t/is (= #{} (sut/find-used-locals no-args-defn-with-let 1 10))
        "Used locals not empty outside let.")
  (t/is (= #{} (sut/find-used-locals no-args-defn-with-let 3 12))
        "Used locals not empty in bindings form of let")
  (t/is (= '#{prefix} (sut/find-used-locals no-args-defn-with-let 5 21))
        "Used local should be symbol 'prefix and only 'prefix"))

(t/deftest test-find-used-locals
  (t/is (= '#{s}
           (sut/find-used-locals example-five 11 6)))
  (t/is (= '#{sep s}
           (sut/find-used-locals example-five 12 13)))
  (t/is (= '#{p}
           (sut/find-used-locals example-five 19 16)))
  (t/is (= '#{sep strings}
           (sut/find-used-locals example-five 26 8)))
  (t/is (= '#{name}
           (sut/find-used-locals example-five 33 8)))
  (t/is (= '#{n}
           (sut/find-used-locals example-five 36 5)))
  (t/is (= '#{x y z a b c}
           (sut/find-used-locals example-five 40 4))))

(t/deftest test-find-used-locals-with-deconstruction
  (t/is (= #{}
           (sut/find-used-locals example-five 44 7)))
  (t/is (= '#{foo}
           (sut/find-used-locals example-five 45 7)))
  (t/is (= #{}
           (sut/find-used-locals example-five 57 7)))
  (t/is (= '#{foo}
           (sut/find-used-locals example-five 58 7))))
