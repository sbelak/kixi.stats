(ns kixi.stats.utils)

(def max-int
  Long/MAX_VALUE)

(defn clz
  [value]
  (Long/numberOfLeadingZeros value))

(defn log
  [x]
  #?(:clj  (Math/log x)
     :cljs (js/Math.log x)))

(defn floor
  [x]
  #?(:clj  (Math/floor x)
     :cljs (js/Math.floor x)))

(defn ceil
  [x]
  #?(:clj  (Math/ceil x)
     :cljs (js/Math.ceil x)))

(defn sqrt [x]
  #?(:clj  (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn sq [x]
  (* x x))

(defn pow [x n]
  #?(:clj  (Math/pow x n)
     :cljs (js/Math.pow x n)))

(defn somef [f]
  (fn [x & args]
    (when-not (nil? x)
      (apply f x args))))

(defn post-complete [rf f]
  (completing rf #(f (rf %))))
