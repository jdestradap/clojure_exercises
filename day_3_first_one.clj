(defn move
  [sym]
  (match [sym]
         [\^] (seq [0 1])
         [\<] (seq [-1 0])
         [\v] (seq [0 -1])
         [\>] (seq [1 0])
         [_]  (seq [0 0])))

(defn ne
  [[c1 c2] [m1 m2]]
    (seq [(+ c1 m1) (+ c2 m2)]))

(defn give
  [houses next]  
  (let [[x y] next] (update houses [(keyword (Integer/toString x)) (keyword (Integer/toString y))] (fnil inc 0)))
)

(defn houses-with-presents
  [m]
  (loop [movements m houses {} current (seq [0 0])]
    (if (empty? movements)
      houses
      (let [[part & remaining] movements n (ne current (move part))]
        (recur remaining (give houses n) n)))))


(count (houses-with-presents (seq (char-array(clojure.string/trim-newline(slurp "/Users/jdestradap/projects/clojure-hello-world/clojure-noob/src/clojure_noob/input_day_3.txt"))))))
