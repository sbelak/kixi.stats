(ns kixi.stats.histogram-test
  (:require [kixi.stats.histogram :as sut]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [kixi.stats.test-utils :refer [=ish]]
            #?@(:clj [[clojure.test :as t :refer [deftest is]]
                      [clojure.test.check.clojure-test :refer [defspec]]
                      [clojure.test.check.properties :refer [for-all]]]
                :cljs [[cljs.test :as t :include-macros true :refer [deftest is]]
                       [clojure.test.check.clojure-test :refer-macros [defspec]]
                       [clojure.test.check.properties :refer-macros [for-all]]]))
  (:import [org.HdrHistogram Histogram]))

(def test-opts {:num-tests 100
                :par       4})

(defn equivalent? [hist a b]
  (sut/values-are-equivalent? hist a b))

(defn verified-maximum?
  [hist]
  (let [computed-max (apply max 0 (map (partial sut/get-value-from-index hist) (-> hist :counts keys)))
        computed-max (if (zero? computed-max)
                       0
                       (sut/highest-equivalent-value hist computed-max))]
    (= computed-max (sut/maximum hist))))

(deftest empty-histogram
  (let [hist (sut/histogram 3)]
    (is nil? (sut/minimum hist))
    (is nil? (sut/maximum hist))))

(def highest-trackable-value (* 3600 1000 1000))
(def significant-value-digits 3)

(deftest record-value
  (let [hist (-> (sut/histogram highest-trackable-value significant-value-digits)
                 (sut/do-record-value 4))]
    (is (= 1 (:total-count hist)))
    (is (= 1 (sut/get-count-at-value hist 4)))))

(deftest overflow-throws-exception
  (let [hist (sut/histogram highest-trackable-value 3)]
    (is (thrown? IndexOutOfBoundsException
                 (sut/do-record-value hist (* 3 highest-trackable-value))))))

(deftest large-numbers []
  (let [hist (-> (sut/histogram 20000000 100000000 5)
                 (sut/do-record-value 100000000)
                 (sut/do-record-value 20000000)
                 (sut/do-record-value 30000000))]
    (is (equivalent? hist 20000000
                     (sut/value-at-percentile hist 50.0)))
    (is (equivalent? hist 30000000
                     (sut/value-at-percentile hist 83.33)))
    (is (equivalent? hist 100000000
                     (sut/value-at-percentile hist 83.34)))
    (is (equivalent? hist 100000000
                     (sut/value-at-percentile hist 99.0)))
    (is (verified-maximum? hist))))

(deftest size-of-equivalent-value-range
  (let [hist (sut/histogram highest-trackable-value significant-value-digits)]
    (is (= 1 (sut/equivalent-value-range hist 1)))
    (is (= 2 (sut/equivalent-value-range hist 2500)))
    (is (= 4 (sut/equivalent-value-range hist 8191)))
    (is (= 8 (sut/equivalent-value-range hist 8192)))
    (is (= 8 (sut/equivalent-value-range hist 10000)))
    (is (verified-maximum? hist))))

(deftest scaled-size-of-equivalent-value-range
  (let [hist (sut/histogram 1024 highest-trackable-value significant-value-digits)]
    (is (= (* 1024 1) (sut/equivalent-value-range hist (* 1024 1))))
    (is (= (* 1024 2) (sut/equivalent-value-range hist (* 1024 2500))))
    (is (= (* 1024 4) (sut/equivalent-value-range hist (* 1024 8191))))
    (is (= (* 1024 8) (sut/equivalent-value-range hist (* 1024 8192))))
    (is (= (* 1024 8) (sut/equivalent-value-range hist (* 1024 10000))))
    (is (verified-maximum? hist))))

(deftest lowest-equivalent-value
  (let [hist (sut/histogram highest-trackable-value significant-value-digits)]
    (is (= 10000 (sut/lowest-equivalent-value hist 10007)))
    (is (= 10008 (sut/lowest-equivalent-value hist 10009)))
    (is (verified-maximum? hist))))

(deftest scaled-lowest-equivalent-value
  (let [hist (sut/histogram 1024 highest-trackable-value significant-value-digits)]
    (is (= (* 1024 10000) (sut/lowest-equivalent-value hist (* 1024 10007))))
    (is (= (* 1024 10008) (sut/lowest-equivalent-value hist (* 1024 10009))))
    (is (verified-maximum? hist))))

(deftest highest-equivalent-value
  (let [hist (sut/histogram highest-trackable-value significant-value-digits)]
    (is (= 8183  (sut/highest-equivalent-value hist 8180)))
    (is (= 8191  (sut/highest-equivalent-value hist 8191)))
    (is (= 8199  (sut/highest-equivalent-value hist 8193)))
    (is (= 9999  (sut/highest-equivalent-value hist 9995)))
    (is (= 10007 (sut/highest-equivalent-value hist 10007)))
    (is (= 10015 (sut/highest-equivalent-value hist 10008)))
    (is (verified-maximum? hist))))

(deftest scaled-highest-equivalent-value
  (let [hist (sut/histogram 1024 highest-trackable-value significant-value-digits)]
    (is (= (+ 1023 (* 1024 8183))  (sut/highest-equivalent-value hist (* 1024 8183))))
    (is (= (+ 1023 (* 1024 8191))  (sut/highest-equivalent-value hist (* 1024 8191))))
    (is (= (+ 1023 (* 1024 8199))  (sut/highest-equivalent-value hist (* 1024 8199))))
    (is (= (+ 1023 (* 1024 9999))  (sut/highest-equivalent-value hist (* 1024 9999))))
    (is (= (+ 1023 (* 1024 10007)) (sut/highest-equivalent-value hist (* 1024 10007))))
    (is (= (+ 1023 (* 1024 10015)) (sut/highest-equivalent-value hist (* 1024 10015))))
    (is (verified-maximum? hist))))

(deftest median-equivalent-value
  (let [hist (sut/histogram highest-trackable-value significant-value-digits)]
    (is (= 4 (sut/median-equivalent-value hist 4)))
    (is (= 5 (sut/median-equivalent-value hist 5)))
    (is (= 4001 (sut/median-equivalent-value hist 4000)))
    (is (= 8002 (sut/median-equivalent-value hist 8000)))
    (is (= 10004 (sut/median-equivalent-value hist 10007)))
    (is (verified-maximum? hist))))

(deftest scaled-median-equivalent-value
  (let [hist (sut/histogram 1024 highest-trackable-value significant-value-digits)]
    (is (= (+ 512 (* 1024 4)) (sut/median-equivalent-value hist (* 1024 4))))
    (is (= (+ 512 (* 1024 5)) (sut/median-equivalent-value hist (* 1024 5))))
    (is (= (* 1024 4001) (sut/median-equivalent-value hist (* 1024 4000))))
    (is (= (* 1024 8002) (sut/median-equivalent-value hist (* 1024 8000))))
    (is (= (* 1024 10004) (sut/median-equivalent-value hist (* 1024 10007))))
    (is (verified-maximum? hist))))

(defn histogram'
  [xs]
  (let [f (fn [hist x] (doto hist (.recordValue x)))]
    (reduce f (Histogram. 1 1000 3) xs)))

(defspec percentile-spec
  test-opts
  (for-all [xs (gen/vector gen/pos-int)
            pc (gen/choose 0 100)]
           (is (= (sut/value-at-percentile (transduce identity (sut/int-histogram 1 1000 3) xs) pc)
                  (.getValueAtPercentile (histogram' xs) pc)))))

(defspec mean-spec
  test-opts
  (for-all [xs (gen/vector gen/pos-int)]
           (is (=ish (sut/get-mean-value (transduce identity (sut/int-histogram 1 1000 3) xs))
                     (when (seq xs)
                       (.getMean (histogram' xs)))))))
