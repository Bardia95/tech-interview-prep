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
  (if (not= (count s) (count t))
    false
    (= (set s) (set t))))
;; => #'tech-interview-prep.core/is-anagram?-a

(defn is-anagram?-b [s t]
  (if (not= (count s) (count t))
    false
    (= (sort s) (sort t))))
;; => #'tech-interview-prep.core/is-anagram?-b

(defn is-anagram?-c [s t]
  (if (not= (count s) (count t))
    false
    (loop [i 0
           c (zipmap (range 97 123) (repeat 26 0))]
      (if (= i (count s))
        (= c (zipmap (range 97 123) (repeat 26 0)))
        (let [d (update c (int (.charAt s i)) inc)
              e (update d (int (.charAt t i)) dec)]
          (recur (inc i) e))))))
;; => #'tech-interview-prep.core/is-anagram?-c

(is-anagram?-a "anagram" "nagaram")
;; => true
(is-anagram?-b"anagram" "nagaram")
;; => true
(is-anagram?-c "anagram" "nagaram")
;; => true


(is-anagram?-a "rat" "car")
;; => false
(is-anagram?-b "rat" "car")
;; => false
(is-anagram?-c "rat" "car")
;; => false


(defn valid-parens?
  {:doc "Given a string containing just the character:
         '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

         An input string is valid if:
         - Open brackets must be closed by the same type of brackets.
         - Open brackets must be closed in the correct order.

         An empty string is also considered valid."
   :test #(do
            (assert (true? (valid-parens? "()")))
            (assert (true? (valid-parens? "()[]{}")))
            (assert (false? (valid-parens? "([")))
            (assert (false? (valid-parens? "([)]")))
            (assert (true? (valid-parens? "{[()]}"))))}
  [s]
  (let [ht {\] \[
            \) \(
            \} \{}]
    (empty? (reduce #(if (contains? ht %2)
                       (if (not= (ht %2) (last %1))
                         (reduced "invalid")
                         (vec (butlast %1)))
                       (conj %1 %2)) [] s))))


(test #'valid-parens?)
;; => :ok


(defn build-products-array [v]
  (loop [i 1
         r [1]]
    (if (= i (count v))
      r
      (recur (inc i)
             (conj r (* (v (dec i)) (r (dec i))))))))

(def rev-products-array (comp vec reverse build-products-array vec reverse))

(defn product-of-array-except-self
  {:doc "Given an array nums of `n` integers where `n` > 1,
         return an array output where output[i] is equal to
         the product of all the elements of nums except nums[i]"
   :test #(do
            (assert (= (product-of-array-except-self [1 2 3 4]) '(24 12 8 6))))}
  [nums]
  (let [l (build-products-array nums)
        r (rev-products-array nums)]
    (map * l r)))

(test #'product-of-array-except-self)
;; => :ok



(defn cross-sum [nums left right p]
  (if (= left right)
    (nums left)
    (loop [left-subsum ##-Inf
           curl-sum 0
           i p]
      (if (= i (dec left))
        (loop [right-subsum ##-Inf
               curr-sum 0
               j (inc p)]
          (if (= j (inc right))
            (+ left-subsum right-subsum)
            (recur (max right-subsum (+ curr-sum (nums j)))
                   (+ curr-sum (nums j))
                   (inc j))))
        (recur (max left-subsum (+ curl-sum (nums i)))
               (+ curl-sum (nums i))
               (dec i))))))
;; => #'tech-interview-prep.core/cross-sum
;; => #'tech-interview-prep.core/cross-sum


(defn msa-helper [nums left right]
  (if (= left right)
    (nums left)
    (let [p (quot (+ left right) 2)
          left-sum  (msa-helper nums left p)
          right-sum (msa-helper nums (inc p) right)
          cross-sum (cross-sum nums left right p)]
      (max left-sum right-sum cross-sum))))
;; => #'tech-interview-prep.core/msa-helper
;; => #'tech-interview-prep.core/msa-helper

(defn max-sub-array
  {:doc "Given an integer array nums,
         find the contiguous subarray (containing at
         least one number) which has the largest sum
         and return its sum."
   :test #(do
            (assert (= (max-sub-array [-2 1 -3 4 -1 2 1 -5 4]) 6)))}
  [nums]
  (msa-helper nums 0 (dec (count nums))));; => #'tech-interview-prep.core/max-sub-array
;; => #'tech-interview-prep.core/max-sub-array

(test #'max-sub-array)


(max-sub-array [-2 1 4 2 1 4]);; => 12
