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

(two-sum-b [1 2 3 4] 7)
;; => [2 3]

;; Given an array of integers, find if the array contains any duplicates.

;; Your function should return true if any value appears at least twice in the array, and it should return false if every element is distinct.

(defn contains-duplicate-a
  [nums]
  (not= (count (set nums)) (count nums)))
;; => #'tech-interview-prep.core/contains-duplicate

(defn contains-duplicate-b
  [nums]
  (let [nums (vec (sort nums))]
    (< 0 (count (for [i (range (dec (count nums)))
                      :when (= (nums i) (nums (inc i)))]
                  (= (nums i) (nums (inc i))))))))
;; => #'tech-interview-prep.core/contains-duplicate-b
;; => #'tech-interview-prep.core/contains-duplicate-b

(contains-duplicate-a [1 2 3 1])
;; => true
(contains-duplicate-b [1 2 3 1])
;; => true

(contains-duplicate-a [1 2 3 4])
;; => false
(contains-duplicate-b [1 2 3 4])
;; => false

(contains-duplicate-a [1 1 1 3 3 4 3 2 4 1])
;; => true
(contains-duplicate-b [1 1 1 3 3 4 3 2 4 1])
;; => true
