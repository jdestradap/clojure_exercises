(defn counting-stars
  "helping to count some starts"
  [string]
  (count (filter #{\(} string)))

(defn counting-stars-second
  "helping to count some starts"
  [string]
  (- (count (filter #{\)} string)) (count (filter #{\(} string))))

(defn counting-some-stuf
  [string]
  (counting-stars (seq string)))


(defn counting-some-stuf-second
  [string]
  (counting-stars-second (seq string)))


(defn get-count-by-parenthesis
  [string]
  #() (frequencies (mapv identity string))) 


(into {} 
  (for [[k v] (get-count-by-parenthesis "jsjsjss")] 
    [(keyword k) v]))


(defn convert-to-vector
  [string]
  (mapv identity string))

(defn position-on-string 
  [initial-vector]
  (if (= (first (initial-vector)) \a)
    (inc position)
    "hola"))

(defn test-d
  [vector-elements element]
  (reduce + (conj vector-elements element)))


(defn calculate-pos-basement
  [initial-vector end-vector]
  (let [v (#(conj end-vector %) (first initial-vector))]
    (if (= (reduce + v) -1)
      (do (println "Success!")
          (count v))
      (do (println "continue")
          (calculate-pos-basement (rest initial-vector) v)))))

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn expand-body-parts
  [asym-body-parts f]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          (f asym-body-parts)))


(def body-parts [{:name "head" :size 3}
                 {:name "eye" :size 1}
                 {:name "ear" :size 1}
                 {:name "mouth" :size 1}
                 {:name "nose" :size 1}
                 {:name "neck" :size 2}
                 {:name "shoulder" :size 3}
                 {:name "upper-arm" :size 3}
                 {:name "chest" :size 10}
                 {:name "back" :size 10}
                 {:name "forearm" :size 3}
                 {:name "abdomen" :size 6}
                 {:name "kidney" :size 1}
                 {:name "hand" :size 2}
                 {:name "knee" :size 2}
                 {:name "thigh" :size 4}
                 {:name "leg" :size 3}
                 {:name "achilles" :size 1}
                 {:name "foot" :size 2}])
(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(reduce (fn [final-multiply-parts part]
          (if (= (:name part) "head")
            (conj final-multiply-parts part)))
        []
        [{:name "head" :size 3}]
)


(loop [parts [] part {:name "head" :size 2} cont 0]
  (if (= cont 5)
    parts
    (recur (conj parts part) part (inc cont))))

(defn calc-surface-area
  [l w h]
  (+ (* 2 (* l w)) (* 2 (* w h)) (* 2 (* h l)) 
     (reduce * (let [x (#(apply max %) [l w h])]
                 (remove #(>= % x) [l w h])))))
 
(let [x (#(apply max %) [1 2 3 4])]
  (remove #(>= % x) [1 2 3 4]))

(remove #(>= % 3) [1 2 3 4])

(let [x (#(apply max %) [1 2 3])]
  (remove #(>= % x) [1 2 3]))

(use 'clojure.java.io)
(with-open [rdr (reader "./input_day_2.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))

(with-open [rdr (clojure.java.io/reader "./input_day_2.txt")]
    (reduce conj [] (line-seq rdr)))


(with-open [rdr (clojure.java.io/reader "./input_day_2.txt")]
  (reduce #(conj %) [] (line-seq rdr)))


(reduce conj [] [1 2 3])

(reduce (fn [final-array elem]
          (conj final-array (inc elem)))
        []
        ["20x7x21"])
(def phone-number "672x345x456x3212")
(def matcher (re-matcher #"\d+" phone-number))

(Integer. (re-find matcher))

(re-seq #"\d+" "111x222x2")

(reduce (fn [final-array elem]
          (conj final-array
                ((let [[l w h] (into [] (map read-string (into [] (re-seq #"\d+" elem))))]
                   (calc-surface-area l w h)))))
        []
        (with-open [rdr (clojure.java.io/reader "./input_day_2.txt")]
          (reduce conj [] (line-seq rdr))))

(map #(Integer. %) (into [] (re-seq #"\d+" "111x222x2")))

(let [[h w l] (into [] (map read-string (into [] (re-seq #"\d+" "111x222x2"))))]
        h)

(with-open [rdr (clojure.java.io/reader "./input_day_2.txt")]
  (reduce (fn [final-array elem]
            (conj final-array
                  ((let [[l w h] (into [] (map read-string (into [] (re-seq #"\d+" elem))))]
                     (calc-surface-area l w h)))))
          []
          (line-seq rdr)))

(let [[s q] (sort [1 2 3])]
        s)

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


(let [[l w h] (into [] (map read-string (into [] (re-seq #"\d+" elem))))]
                     (calc-surface-area l w h))

()


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

(let [[s q] (sort [4 3 3])]
        (* 2 (+ s q)))

(defn day-3-first-part
  [x]
  x)

(def a (hash-map [:0 :0] 1))
(update-in a [[:0 :0]] inc)
(update a [:0 :0] (fnil inc 0))


(update {[] :a 1, :b 1} :b (fnil inc 0))
(defn add-position
  "insert a new present to the next house"  
  [x y map]
())

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
