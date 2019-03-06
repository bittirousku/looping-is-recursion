(ns looping-is-recursion)

; Write the function (power n k) that computes
; the mathematical expression nk.
; (defn helper [acc base exp]
;   (cond
;     (= base 0) 0
;     (= exp 0) 1
;     (= exp 1) acc
;     :else (helper (* acc base) base (dec exp))))
;
; (defn power [base exp]
;   (helper base base exp))
; better version below:

(defn power [base exp]
  (let [helper (fn [acc base exp]
                (cond
                  (= base 0) 0
                  (= exp 0) 1
                  (= exp 1) acc
                  :else (recur (* acc base) base (dec exp))))]
    (helper base base exp)))


; Compute the last element of a sequence.
(defn last-element [a-seq]
  (let [helper (fn [latest, list]
                 (cond
                   (= latest nil) nil
                   (empty? list) latest
                   :else (recur (first list) (rest list))))]
    (helper (first a-seq) a-seq)))


; Write the function (seq= a-seq b-seq) that compares two sequences for equality.
(defn seq= [seq1 seq2]
  (let [helper (fn [list1, list2]
                 (cond
                   (not= (count list1) (count list2)) false
                   (not= (first list1) (first list2)) false
                   (empty? (and list1 list2)) true
                   :else (recur (rest list1) (rest list2))))]
    (helper seq1 seq2)))


; Implement the function (find-first-index [predicate seq]) that returns the first index in seq for which predicate returns true, or nil if no such index exists.
(defn find-first-index [pred a-seq]
  (loop [list a-seq
         i 0]
    (cond
      (empty? list) nil
      (pred (first list)) i
      :else (recur (rest list) (inc i)))))


; Implement the function (avg a-seq) that computes the average of a sequence.
(defn avg [a-seq]
  (loop [acc 0
         list a-seq
         i 0]
    (cond
      (empty? a-seq) nil
      (empty? list) (/ acc i)
      :else (recur (+ acc (first list)) (rest list) (inc i)))))

; Write the function (parity a-seq) that takes in a sequence
; and returns a set of those elements that occur an odd number
; of times in the sequence.
(defn parity [a-seq]
  (loop [list a-seq
         odds (empty #{})]
    (cond
      (empty? a-seq) nil
      (empty? list) odds
      (contains? odds (first list)) (recur (rest list) (disj odds (first list)))
      :else (recur (rest list) (conj odds (first list))))))

; Write the function (fast-fibo n) that computes the nth fibonacci number
; using loop and recur. Do not use recursion.
(defn fast-fibo [n]
  (loop [Fn 1
         Fn-1 0
         i 1]
    (cond
      (= n 0) 0
      (= n 1) 1
      (= i n) Fn
      :else (recur (+ Fn Fn-1) Fn (inc i)))))

; Write the function (cut-at-repetition a-seq) that takes in a
; sequence and returns elements from the sequence up to the first
; repetition.
(defn cut-at-repetition [a-seq]
  (loop [list a-seq
         ok (empty [])
         seen (empty #{})]
    (cond
      (empty? list) ok
      (contains? seen (first list)) ok
      :else (recur (rest list) (conj ok (first list)) (conj seen (first list))))))
