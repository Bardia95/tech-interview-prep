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
