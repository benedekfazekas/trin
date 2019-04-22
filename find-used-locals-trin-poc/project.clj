(require '[clojure.edn :as edn])

(def +deps+ (-> "deps.edn" slurp edn/read-string))

(defn deps->vec [deps]
  (->> deps
       (filter (fn [[_ params]] (:mvn/version params)))
       (mapv (fn [[dep {:keys [:mvn/version exclusions]}]]
               (cond-> [dep version]
                 exclusions (conj :exclusions exclusions))))))

(def dependencies
  (deps->vec (:deps +deps+)))

(defproject thomasa/find-used-locals-trin-poc "0.1.0-SNAPSHOT"
  :source-paths ["src" "../trin/src"]
  :dependencies ~dependencies)
