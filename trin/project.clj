(require '[clojure.edn :as edn])

(def +deps+ (-> "deps.edn" slurp edn/read-string))

(defn deps->vec [deps]
  (mapv (fn [[dep {:keys [:mvn/version exclusions]}]]
          (cond-> [dep version]
            exclusions (conj :exclusions exclusions)))
        deps))

(def dependencies
  (deps->vec (:deps +deps+)))

(defproject thomasa/trin "0.1.0-SNAPSHOT"
  :dependencies ~dependencies)
