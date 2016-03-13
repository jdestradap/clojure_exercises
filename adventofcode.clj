;; http://adventofcode.com/day/1
;; part one

(defn counting-stars
  "helping to count some starts"
  [string]
  (- (count (filter #{\(} string)) (count (filter #{\)} string))))


;; part two
;; we change "(" by 1 and ")" by -1 and create the initial vector with this values. 
(defn calculate-pos-basement
  [initial-vector end-vector]
  (let [v (#(conj end-vector %) (first initial-vector))]
    (if (= (reduce + v) -1)
      (do (println "Success!")
          (count v))
      (do (println "continue")
          (calculate-pos-basement (rest initial-vector) v)))))

;; http://adventofcode.com/day/2
;;Day 2 first part
(defn calc-surface-area
  [l w h]
  (+ (* 2 (* l w)) (* 2 (* w h)) (* 2 (* h l)) 
     (let [[s q] (sort [l w h])]
        (* s q))))
 
(reduce + (with-open [rdr (clojure.java.io/reader "./input_day_2.txt")]
            (reduce (fn [final-array elem]
                      (conj final-array
                            (let [[l w h] (into [] (map read-string (into [] (re-seq #"\d+" elem))))]
                              (calc-surface-area l w h))))
                    []
                    (line-seq rdr))))

;; Day 2 second part
(defn calc-ribbon-feet
  [l w h]
(+ (* l w h) (let [[s q] (sort [l w h])]
        (* 2 (+ s q)))))

(reduce + (with-open [rdr (clojure.java.io/reader "./input_day_2.txt")]
            (reduce (fn [final-array elem]
                      (conj final-array
                            (let [[l w h] (into [] (map read-string (into [] (re-seq #"\d+" elem))))]
                              (calc-ribbon-feet l w h))))
                    []
                    (line-seq rdr))))

;; http://adventofcode.com/day/3
;; First part

(ns clojure-noob.core
  (:require [clojure.core.match :refer :all]))

(defn move
  [sym]
  (match [sym]
         [\^] '(0 1)
         [\<] '(-1 0)
         [\v] '(0 -1)
         [\>] '(1 0)
         [_]  '(0 0)))

(defn next-move
  [[c1 c2] [m1 m2]]
    (seq [(+ c1 m1) (+ c2 m2)]))

(defn give
  [houses next]  
  (let [[x y] next] (update houses [(keyword (Integer/toString x)) (keyword (Integer/toString y))] (fnil inc 0)))
)

(defn houses-with-presents
  [m]
  (loop [movements m houses {} santa-current '(0 0)]
    (if (empty? movements)
      houses
      (let [[part & remaining] movements n (next-move santa-current (move part))]
        (recur remaining (give-present houses n) n)))))

(count (houses-with-presents (seq (char-array(clojure.string/trim-newline(slurp "/Users/jdestradap/projects/clojure-hello-world/clojure-noob/src/clojure_noob/input_day_3.txt"))))))

;; Second part 

(defn compound-key
  [sequence]
  (let [[x y] sequence] [(keyword (Integer/toString x)) (keyword (Integer/toString y))]))

(defn update-coordenates
  [key-houses sequence-points]
  (if (empty? key-houses)
    sequence-points
    (let [[part & remaining] key-houses sq (update sequence-points part (fnil inc 0))]
      (update-coordenates remaining sq))))

(defn cross-array
  [movements houses santa-current robot-santa-current]
  (if (empty? movements)
    houses
    (let [[santa-part robot-santa-part & remaining] movements
          next-move-santa (next-move santa-current (move santa-part))
          next-move-robot-santa (next-move robot-santa-current (move robot-santa-part))
          h (update-coordenates [(compound-key next-move-santa) (compound-key next-move-robot-santa)] houses)]
      (cross-array remaining h next-move-santa next-move-robot-santa))))

(count (cross-array (seq (char-array(clojure.string/trim-newline(slurp "/Users/jdestradap/projects/clojure-hello-world/clojure-noob/src/clojure_noob/input_day_3.txt.cp")))) {} '(0 0) '(0 0)))

;;Day 4
;;First part

(defn calc-number
  [s number]
  (let [hs (md5 (str s number))]
    (if (= (compare (subs hs 0 5) "00000") 0)
                     number
                     (recur s (inc number)))))
