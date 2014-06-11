(ns looping-is-recursion)

(defn power [base exp]
  ((fn [accum base exp]
     (if (zero? exp)
       accum
       (recur (* accum base) base (dec exp))))
   1 base exp))

(defn last-element [a-seq]
  ((fn [item items]
     (if (empty? items)
       item
       (recur (first items) (rest items))))
   nil a-seq))

(defn seq= [seq1 seq2]
  (let [e1 (empty? seq1)
        e2 (empty? seq2)]
    (cond (and e1 e2)                      true
          (or e1 e2)                       false
          (= (first seq1) (first seq2))    (recur (rest seq1) (rest seq2))
          :else                            false
    )))

(defn find-first-index [pred a-seq]
  ((fn [index items]
     (cond (empty? items)       nil
           (pred (first items)) index
           :else                (recur (inc index) (rest items))))
   0 a-seq))

(defn avg [a-seq]
  ((fn [counter sum items]
     (if (empty? items)
       (/ sum counter)
       (recur (inc counter) (+ (first items) sum) (rest items))))
   0 0 a-seq))

(defn parity [a-seq]
  ((fn [a-set items]
     (if (empty? items) a-set
       (let [[f & r] items]
         (recur
           (if (contains? a-set f)
             (disj a-set f)
             (conj a-set f))
           r
          ))))
   #{} a-seq))

(defn fast-fibo [n]
  (cond (zero? n) 0
        (== 1 n) 1
        :else
          ((fn [fib_x-2 fib_x-1 x]
             (let [fib_x (+ fib_x-2 fib_x-1)]
               (if (== x n)
                 fib_x
                 (recur fib_x-1 fib_x (inc x)))))
     0 1 2)))

(defn cut-at-repetition [a-seq]
  (->>
    ((fn [accum used items]
      (if (empty? items)
        accum
        (let [[item & remaining-items] items]
          (if (contains? used item)
            accum
            (recur (cons item accum) (conj used item) remaining-items)))))
     [] #{} a-seq)
   reverse))
