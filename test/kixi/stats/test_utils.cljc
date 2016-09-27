(ns kixi.stats.test-utils)

(defn approx=
  "Equal to within err fraction, or if one is zero, to within err absolute."
  ([err x y]
   (if (and (map? x) (map? y))
     (->> (merge-with vector x y)
          (vals)
          (map #(apply approx= err %)))
     (or (= x y)
         (== x y)
         (if (or (zero? x) (zero? y))
           (< (- err) (- x y) err)
           (< (- 1 err) (/ x y) (+ 1 err))))))
  ([err x y & more]
   (->> more
        (cons y)
        (every? (partial approx= err x)))))

(def =ish
  "Almost equal"
  (partial approx= 0.0000001))
