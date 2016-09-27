(ns kixi.stats.histogram
  (:require [kixi.stats.utils :refer [sqrt sq pow floor ceil clz log max-int]]))

(defn get-bucket-index
  [{:keys [sub-bucket-mask unit-magnitude sub-bucket-half-count-magnitude]} value]
  (let [pow2ceiling (- 64 (clz (bit-or (long value) sub-bucket-mask)))]
    (long (- pow2ceiling unit-magnitude (inc sub-bucket-half-count-magnitude)))))

(defn get-sub-bucket-index
  [{:keys [unit-magnitude]} bucket-index value]
  (bit-shift-right (long value) (+ bucket-index unit-magnitude)))

(defn counts-index [{:keys [sub-bucket-half-count-magnitude
                            sub-bucket-half-count
                            counts-len]}
                    bucket-index sub-bucket-index]
  (let [bucket-base-index (bit-shift-left (inc bucket-index) sub-bucket-half-count-magnitude)
        offset-in-bucket (- sub-bucket-index sub-bucket-half-count)
        index (+ bucket-base-index offset-in-bucket)]
    (if (>= index counts-len)
      (throw (IndexOutOfBoundsException.))
      index)))

(defn counts-index-for
  [hist value]
  (let [bucket-index (get-bucket-index hist value)]
    (counts-index hist bucket-index (get-sub-bucket-index hist bucket-index value))))

(defn do-record-value
  ([hist value]
   (do-record-value hist value 1))
  ([hist value count]
   (let [counts-index (counts-index-for hist value)]
     (-> hist
         (update :total-count + count)
         (update :min-value min value)
         (update :max-value max value)
         (update-in [:counts counts-index] (fnil + 0) count)))))

(defn get-target-count-at-percentile
  [{:keys [total-count]} percentile]
  (let [requested-percentile (min percentile 100.0)
        count-at-percentile (int (+ (* requested-percentile total-count 0.01) 0.5))]
    (max count-at-percentile 1)))

(defn get-count-at-index
  [{:keys [counts-len counts]} index]
  (get counts index 0))

(defn get-count-at-value
  [hist value]
  (let [index (counts-index-for hist value)]
    (get-count-at-index hist index)))

(defn get-value-from-sub-bucket
  [{:keys [unit-magnitude]} bucket-index sub-bucket-index]
  (bit-shift-left sub-bucket-index (+ bucket-index unit-magnitude)))

(defn get-value-from-index
  [{:keys [sub-bucket-half-count-magnitude sub-bucket-half-count] :as hist} index]
  (let [bucket-index (dec (bit-shift-right index sub-bucket-half-count-magnitude))
        sub-bucket-index (+ (bit-and index (dec sub-bucket-half-count)) sub-bucket-half-count)]
    (if (neg? bucket-index)
      (get-value-from-sub-bucket hist 0 (- sub-bucket-index sub-bucket-half-count))
      (get-value-from-sub-bucket hist bucket-index sub-bucket-index))))

(defn lowest-equivalent-value
  [hist value]
  (let [bucket-index (get-bucket-index hist value)
        sub-bucket-index (get-sub-bucket-index hist bucket-index value)]
    (get-value-from-sub-bucket hist bucket-index sub-bucket-index)))

(defn values-are-equivalent?
  [hist a b]
  (= (lowest-equivalent-value hist a)
     (lowest-equivalent-value hist b)))

(defn equivalent-value-range
  [{:keys [unit-magnitude sub-bucket-count] :as hist} value]
  (let [bucket-index (get-bucket-index hist value)
        sub-bucket-index (get-sub-bucket-index hist bucket-index value)]
    (bit-shift-left 1 (+ unit-magnitude (if (>= sub-bucket-index sub-bucket-count)
                                          (inc bucket-index)
                                          bucket-index)))))

(defn highest-equivalent-value
  [hist value]
  (let [lowest-equivalent-value (lowest-equivalent-value hist value)
        size-of-equivalent-value-range (equivalent-value-range hist value)]
    (dec (+ lowest-equivalent-value size-of-equivalent-value-range))))



(defn do-value-at-percentile
  [{:keys [counts-len] :as hist} percentile]
  (let [count-at-percentile (get-target-count-at-percentile hist percentile)]
    (loop [index 0
           total 0]
      (let [total (+ total (get-count-at-index hist index))]
        (cond
          (>= index counts-len)
          0
          (>= total count-at-percentile)
          (let [value-at-index (get-value-from-index hist index)]
            (if (zero? percentile)
              (lowest-equivalent-value hist value-at-index)
              (highest-equivalent-value hist value-at-index)))
          :else
          (recur (inc index) total))))))

(defprotocol HistogramP
  (record-value [this value count])
  (value-at-percentile [this percentile])
  (minimum [this])
  (maximum [this]))

(defrecord Histogram [counts total-count min-value max-value
                      sub-bucket-mask unit-magnitude
                      sub-bucket-half-count-magnitude]
  HistogramP
  (record-value [this value count]
    (cond-> this
      (pos? value)
      (do-record-value this value count)))
  (value-at-percentile [this percentile]
    (do-value-at-percentile this percentile))
  (minimum [this]
    (:min-value this))
  (maximum [this]
    (if (zero? (:max-value this))
      0
      (highest-equivalent-value this (:max-value this)))))

(def floor-div (comp long floor /))

(defn get-bucket-count
  [value subb-count unit-magnitude]
  (loop [smallest-untrackable-value (bit-shift-left subb-count unit-magnitude)
         buckets-needed 1]
    #_(prn {:smallest-untrackable-value smallest-untrackable-value
          :buckets-needed buckets-needed})
    (cond
      (> smallest-untrackable-value value)
      buckets-needed
      (> smallest-untrackable-value (floor-div max-int 2))
      (inc buckets-needed)
      :else
      (recur (bit-shift-left smallest-untrackable-value 1)
             (inc buckets-needed)))))

(defn size-of-equivalent-value-range
  [{:keys [sub-bucket-count unit-magnitude] :as hist} value]
  (let [bucket-index (get-bucket-index hist value)
        sub-bucket-index (get-sub-bucket-index hist bucket-index value)]
    (bit-shift-left 1 (+ unit-magnitude
                         (if (> sub-bucket-index sub-bucket-count)
                           (inc bucket-index)
                           bucket-index)))))

(defn median-equivalent-value [hist value]
  (+ (lowest-equivalent-value hist value)
     (bit-shift-right (size-of-equivalent-value-range hist value) 1)))

(defn get-mean-value [{:keys [counts total-count] :as hist}]
  (let [f (fn [acc [index count]]
            (let [value-at-index (get-value-from-index hist index)
                  median-equivalent (median-equivalent-value hist value-at-index)]
              (+ acc (* median-equivalent count))))]
    (when (pos? total-count)
      (double (/ (reduce f 0 counts) total-count)))))

(defn get-stddev-value
  [{:keys [counts total-count] :as hist}]
  (let [mean (get-mean-value hist)
        f (fn [acc [index count]]
            (let [value-at-index (get-value-from-index hist index)
                  median-equivalent (median-equivalent-value hist value-at-index)]
              (+ acc (* count (sq (- median-equivalent mean))))))]
    (sqrt (/ (reduce f 0 counts)
             total-count))))

(defn histogram
  ([significant-figures]
   (histogram 1 2 significant-figures))
  ([highest-trackable-value
    significant-figures]
   (histogram 1 highest-trackable-value significant-figures))
  ([lowest-trackable-value
     highest-trackable-value
    significant-figures]
   (let [unit-magnitude (int (floor (/ (log lowest-trackable-value)
                                       (log 2))))
         largest-value-single-unit-res (* 2 (pow 10 significant-figures))
         sub-count-mag (int (ceil (/ (log largest-value-single-unit-res)
                                     (log 2))))
         sub-bucket-half-count-magnitude (if (> sub-count-mag 1) (dec sub-count-mag) 0)
         sub-bucket-count (int (pow 2 (inc sub-bucket-half-count-magnitude)))
         sub-bucket-half-count (int (floor-div sub-bucket-count 2))
         
         bucket-count (get-bucket-count highest-trackable-value sub-bucket-count unit-magnitude)]
     (map->Histogram
      {:counts {}
       :total-count 0
       :min-value max-int
       :max-value 0
       :sub-bucket-mask (bit-shift-left (dec sub-bucket-count) unit-magnitude)
       :unit-magnitude unit-magnitude
       :sub-bucket-half-count-magnitude sub-bucket-half-count-magnitude
       :sub-bucket-half-count sub-bucket-half-count
       :sub-bucket-count sub-bucket-count
       :bucket-count bucket-count
       :counts-len (* (inc bucket-count)
                      (floor-div sub-bucket-count 2))}))))

(defn int-histogram
  [lowest-trackable-value
   highest-trackable-value
   signficant-figures]
  (fn
    ([]
     (histogram lowest-trackable-value
                highest-trackable-value
                signficant-figures))
    ([acc x]
     (do-record-value acc x))
    ([acc] acc)))
