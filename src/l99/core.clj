(ns l99.core
  (require [clojure.set :refer [difference]]))

(defn p01 [coll]
  (let [rst (rest coll)]
    (if (empty? rst)
      (list (first coll))
      (recur rst))))

(defn p02 [coll]
  (let [[fst snd] (split-at 2 coll)]
    (if (empty? snd)
      fst
      (recur (rest coll)))))

(defn p03 [coll n]
  (cond
   (<= n 0) nil
   (= n 1) (first coll)
   :else (recur (rest coll) (dec n))))

(defn p04 [coll]
  (loop [coll coll
         len 0]
   (if (empty? coll)
     len
     (recur (rest coll) (inc len)))))

(defn p05 [coll]
  (loop [[fst & more] coll
         reversed '()]
    (if (nil? fst)
      reversed
      (recur more (cons fst reversed)))))

(defn p06 [coll]
  (= coll (reverse coll)))

(defn p07 [[fst & more]]
  (cond
   (nil? fst) '()
   (sequential? fst) (lazy-cat (p07 fst) (p07 more))
   :else (cons fst (lazy-seq (p07 more)))))

(defn p08 [coll]
  (loop [[fst & more] coll
         res '()]
    (cond
     (nil? fst) (reverse res)
     (= fst (first res)) (recur more res)
     :else (recur more (cons fst res)))))

(defn partition-by [f coll]
  (let [append-run #(cons (reverse %1) %2)]
    (loop [[fst & more] coll
           result '()
           run    '()]
      (let [new-run (cons fst run)]
        (cond
         (empty? more) (reverse (append-run new-run result))
         (f fst (first more)) (recur more result new-run)
         :else (recur more (append-run new-run result) (list)))))))

(defn p09 [coll]
  (partition-by = coll))

(defn p10 [coll]
  (let [packed (p09 coll)]
    (map #(list (count %) (first %)) packed)))

(defn p11 [coll]
  (map #(if (= (first %) 1)
          (second %)
          %)
       (p10 coll)))

(defn p12 [coll]
  (->> coll
       (map #(if (sequential? %) (apply repeat %) %))
       flatten))

(defn p14 [coll]
  (mapcat #(list % %) coll))

(defn p15 [coll n]
  (mapcat #(repeat n %) coll))

(defn p16 [coll n]
  (map second (filter #(> (mod (first %) n) 0)
                      (map #(list %2 %1) coll (rest (range))))))

(defn p17 [coll n]
  (loop [left '()
         [fst & more :as right] coll
         n n]
    (if (zero? n)
      (list (reverse left) right)
      (recur (cons fst left) more (dec n)))))

(defn p18 [coll i k]
  (->> coll
       (take k)
       (drop (dec i))))

(defn p19 [coll n]
  (let [n (if (neg? n) (+ (count coll) n) n)
        splitted (p17 coll n)]
    (concat (second splitted) (first splitted))))

(defn p20 [coll n]
  (if (<= n 1)
    (rest coll)
    (cons (first coll) (lazy-seq (p20 (rest coll) (dec n))))))

(defn p21 [x coll n]
  (if (<= n 1)
    (cons x (lazy-seq coll))
    (cons (first coll) (lazy-seq (p21 x (rest coll) (dec n))))))

(defn p22 [a b]
  (if (> a b)
    nil
    (cons a (lazy-seq (p22 (inc a) b)))))

(defn p23 [coll n]
  (list* (take n (shuffle coll))))

(defn p24 [n m]
  (take n (shuffle (range 1 (inc m)))))

(defn p26 [n [fst & more :as coll]]
  (cond
   (or (empty? coll) (zero? n)) '()
   (= 1 n) (map list coll)
   (= n (count coll)) (list coll)
   :else (lazy-cat (map #(cons fst %) (p26 (dec n) more))
                   (p26 n more))))

(defn diff-list [a b]
  (list* (difference (set a) (set b))))

(defn p27 [elems [group-num & rest-group-nums :as group-nums]]
  (case (count group-nums)
    0 '()
    1 (map list (p26 group-num elems))
    (mapcat (fn [a]
              (map #(concat (list a) %)
                   (p27 (diff-list elems a) rest-group-nums)))
            (p26 group-num elems))))

(defn p28-a [coll]
  (sort-by count coll))

(defn p28-b [coll]
  (let [counts (map count coll)]
    (sort-by (fn [e]
               (count (filter #(= % (count e)) counts))) coll)))
