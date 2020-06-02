(ns tech-interview-prep.core)

;; Given an array of integers, return indices of the two numbers such that they 
;; add up to a specific target.

;; You may assume that each input would have exactly one solution.
;; Given nums = [2, 7, 11, 15], target = 9,

;; Because nums[0] + nums[1] = 2 + 7 = 9,
;; return [0, 1].

(defn two-sum-a
  "Two-pass hash table"
  [nums target]
  (let [nums-index (zipmap nums (range))
        indexes (for [[x i] nums-index
                      [y j] nums-index
                      :when (< i j)
                      :when (= (+ x y) target)]
                  [i j])]
    (first indexes)))
;; => #'tech-interview-prep.core/two-sum-two-pass


(defn two-sum-b
  "One-pass hash table"
  [nums target]
  (loop [ht {}
         i 0]
    (if (contains? ht (- target (nums i)))
      [(ht (- target (nums i))) i]
      (recur (assoc ht (nums i) i) (inc i)))))
;; => #'tech-interview-prep.core/two-sum-one-pass


(two-sum-a [2 7 11 15] 9)
;; => [0 1]
(two-sum-b [2 7 11 15] 9)
;; => [0 1]

(two-sum-a [1 2 3 4] 7)
;; => [2 3]
(two-sum-b [1 2 3 4] 7)
;; => [2 3]

;; Given an array of integers, find if the array contains any duplicates.

;; Your function should return true if any value appears at least twice in the array, and it should return false if every element is distinct.

(defn contains-duplicate-a
  [nums]
  (not= (count (set nums)) (count nums)))
;; => #'tech-interview-prep.core/contains-duplicate-a

(defn contains-duplicate-b
  [nums]
  (let [nums (vec (sort nums))]
    (< 0 (count (for [i (range (dec (count nums)))
                      :when (= (nums i) (nums (inc i)))]
                  (= (nums i) (nums (inc i))))))))
;; => #'tech-interview-prep.core/contains-duplicate-b

(defn contains-duplicate-c
  [nums]
  (loop [s #{}
         i 0]
    (if (= i (count nums))
      false
      (if (contains? s (nums i))
        true
        (recur (conj s (nums i)) (inc i))))))
;; => #'tech-interview-prep.core/contains-duplicate-c

(contains-duplicate-a [1 2 3 1])
;; => true
(contains-duplicate-b [1 2 3 1])
;; => true
(contains-duplicate-c [1 2 3 1])
;; => true

(contains-duplicate-a [1 2 3 4])
;; => false
(contains-duplicate-b [1 2 3 4])
;; => false
(contains-duplicate-c [1 2 3 4])
;; => false

(contains-duplicate-a [1 1 1 3 3 4 3 2 4 1])
;; => true
(contains-duplicate-b [1 1 1 3 3 4 3 2 4 1])
;; => true
(contains-duplicate-c [1 1 1 3 3 4 3 2 4 1])
;; => true


;; Say you have an array for which the ith element is the price of a given stock on day i.

;; If you were only permitted to complete at most one transaction (i.e., buy one and sell one share of the stock), design an algorithm to find the maximum profit.

;; Note that you cannot sell a stock before you buy one.

(defn max-profit-a [prices]
  (loop [i 0
         j 1
         max-profit 0]
    (if (= i (dec (count prices)))
      max-profit
      (if (= j (count prices))
        (recur (inc i) (+ i 2) max-profit)
        (let [profit (- (prices j) (prices i))]
          (if (> profit max-profit)
            (recur i (inc j) profit)
            (recur i (inc j) max-profit)))))))
;; => #'tech-interview-prep.core/max-profit-a

(defn max-profit-b [prices]
  (let [m (apply max (for [i (range (count prices))
                           j (range (inc i) (count prices))]
                       (- (prices j) (prices i))))]
    (if (> m 0)
      m
      0)))
;; => #'tech-interview-prep.core/max-profit-b


(defn max-profit-c [prices]
  (loop [min-price 10000
         max-profit 0
         i 0]
    (if (= i (count prices))
      max-profit
      (if (< (prices i) min-price)
        (recur (prices i) max-profit (inc i))
        (if (> (- (prices i) min-price) max-profit)
          (recur min-price (- (prices i) min-price) (inc i))
          (recur min-price max-profit (inc i)))))))
;; => #'tech-interview-prep.core/max-profit-c

(max-profit-a [7 1 5 3 6 4])
;; => 5
(max-profit-b [7 1 5 3 6 4])
;; => 5
(max-profit-c [7 1 5 3 6 4])
;; => 5


(max-profit-a [7 6 4 3 1])
;; => 0
(max-profit-b [7 6 4 3 1])
;; => 0
(max-profit-c [7 6 4 3 1])
;; => 0


;; Given two strings s and t , write a function to determine if t is an anagram of s.

(defn is-anagram?-a [s t]
  (= (set s) (set t)))
;; => #'tech-interview-prep.core/is-anagram?-a

(defn is-anagram?-b [s t]
  (if (not= (count s) (count t))
    false
    (= (sort s) (sort t))))
;; => #'tech-interview-prep.core/is-anagram?-b

(defn is-anagram?-c [s t]
  ())

(is-anagram?-a "anagram" "nagaram")
;; => true
(is-anagram?-b"anagram" "nagaram")
;; => true
(is-anagram?-c"anagram" "nagaram")


(is-anagram?-a "rat" "car")
;; => false
(is-anagram?-b "rat" "car")
;; => false
(is-anagram?-c "rat" "car")


