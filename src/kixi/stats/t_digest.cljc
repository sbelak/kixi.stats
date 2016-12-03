(ns kixi.stats.t-digest
  (:import [com.tdunning.math.stats TDigest]))

(defn t-digest [compression]
  (fn
    ([] (TDigest/createDigest compression))
    ([acc x] (doto acc (.add x 1)))
    ([acc] acc)))

(defn median [& [{:keys [compression]
                  :or {compression 100}}]]
  (completing (t-digest compression)
              #(let [q (.quantile % 0.5)]
                 (when-not (.isNaN q) q))))
