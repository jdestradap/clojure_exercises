;; fibonnaci
(defn fib2 [n-2 n-1 i n]
  (if (= i n)
    n-1
    (recur n-1 (+' n-1 n-2) (inc i) n)))

(defn fib [n]
  (if (= n 1)
    0      
    (fib2 0 1 2 n)))


;; GCD




