(ns tech-interview-prep.core)

;; Given an array of integers, return indices of the two numbers such that they 
;; add up to a specific target.

;; You may assume that each input would have exactly one solution.
;; Given nums = [2, 7, 11, 15], target = 9,

;; Because nums[0] + nums[1] = 2 + 7 = 9,
;; return [0, 1].

(defn two-sum-two-pass
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


(defn two-sum-one-pass
  "One-pass hash table"
  [nums target]
  (loop [ht {}
         i 0]
    (if (contains? ht (- target (nums i)))
      [(ht (- target (nums i))) i]
      (recur (assoc ht (nums i) i) (inc i)))))
;; => #'tech-interview-prep.core/two-sum-one-pass


(two-sum-one-pass [2 7 11 15] 9)
;; => [0 1]

(two-sum-two-pass [1 2 3 4] 7)
;; => [2 3]
