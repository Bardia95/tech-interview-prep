(ns tech-interview-prep.week-2)


(defrecord ListNode [val next])
;; => tech_interview_prep.week_2.ListNode

(defn reverse-list-a
  {:doc "Reverse a linked list - iterative solution"
   :test #(let [D (->ListNode 4 nil)
                C (->ListNode 3 D)
                B (->ListNode 2 C)
                A (->ListNode 1 B)
                H (->ListNode 1 nil)
                G (->ListNode 2 H)
                F (->ListNode 3 G)
                E (->ListNode 4 F)
                head A
                reversed-head E]
            (do
              (assert (= (reverse-list head) reversed-head))))}
  [head]
  (loop [prev nil
         curr head]
    (if (= curr nil)
      prev
      (let [next-temp (:next curr)
            curr (assoc curr :next prev)
            prev curr
            curr next-temp]
        (recur prev curr)))))
;; => #'tech-interview-prep.week-2/reverse-list-a


(test #'reverse-list-a)
;; => :ok


(defn reverse-list-b
  {:doc "Reverse a linked list - recursive solution"
   :test #(let [D (->ListNode 4 nil)
                C (->ListNode 3 D)
                B (->ListNode 2 C)
                A (->ListNode 1 B)
                H (->ListNode 1 nil)
                G (->ListNode 2 H)
                F (->ListNode 3 G)
                E (->ListNode 4 F)
                head A
                reversed-head E]
            (do
              (assert (= (reverse-list head) reversed-head))))}
  [head]
  (if (or (= (:next head) nil) (= head nil))
    head
    (let [p (reverse-list (:next head))
          head (assoc-in head [:next :next] head)
          head (assoc head :next nil)]
      p)))
;; => #'tech-interview-prep.week-2/reverse-list-b


(test #'reverse-list-b)
;; => :ok


(defn has-cycle-a?
  {:doc "Given a linked list, determine if it has a cycle in it
         Hash table solution"}
  [head]
  (loop
      [curr head
       ht {}]
    (if (= curr nil)
      false
      (if (contains? ht curr)
        true
        (recur (:next curr)
               (assoc ht curr true))))))
;; => #'tech-interview-prep.week-2/has-cycle-a?


(defn has-cycle-b?
  {:doc "Given a linked list, determine if it has a cycle in it
         Two pointers solution"}
  [head]
  (if (or (= head nil) (= (:next head) nil))
    false
    (loop [slow head
           fast (:next head)]
      (if (= slow fast)
        true
        (if (or (= fast nil) (= (:next fast) nil))
          false
          (recur (:next slow)
                 (:next (:next fast))))))))
;; => #'tech-interview-prep.week-2/has-cycle-b?


(defn max-area
  {:doc "Given `n` non-negative integers `a1`, `a2`, ..., `an` ,
         where each represents a point at coordinate (`i`, `ai`).

         `n` vertical lines are drawn such that the two endpoints
         of line `i` is at (`i`, `ai`) and (`i`, `0`).

         Find two lines, which together with x-axis forms a container,
         such that the container contains the most water."
   :test #(do
            (assert (= (max-area [1 8 6 2 5 4 8 3 7]) 49)))}
  [height]
  (loop [maxarea 0
         l 0
         r (dec (count height))]
    (if (= l r)
      maxarea
      (let [maxarea (max maxarea (* (min (height l) (height r)) (Math/abs (- r l))))]
        (if (< (height l) (height r))
          (recur maxarea (inc l) r)
          (recur maxarea l (dec r)))))))
;; => #'tech-interview-prep.week-2/max-area


(test #'max-area)
;; => :ok
