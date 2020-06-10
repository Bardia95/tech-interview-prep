(ns tech-interview-prep.week-2)


(defrecord ListNode [val next])
;; => tech_interview_prep.week_2.ListNode

(defn reverse-list
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
;; => #'tech-interview-prep.week-2/reverse-list


(test #'reverse-list)
;; => :ok


(def D (->ListNode 4 nil))
;; => #'tech-interview-prep.week-2/D

(def C (->ListNode 3 D))
;; => #'tech-interview-prep.week-2/C

(def B (->ListNode 2 C))
;; => #'tech-interview-prep.week-2/B

(def A (->ListNode 1 B))
;; => #'tech-interview-prep.week-2/A

(def head A)
;; => #'tech-interview-prep.week-2/head

head
;; => #tech_interview_prep.week_2.ListNode{:val 1, :next #tech_interview_prep.week_2.ListNode{:val 2, :next #tech_interview_prep.week_2.ListNode{:val 3, :next #tech_interview_prep.week_2.ListNode{:val 4, :next nil}}}}

(reverse-list head)
;; => #tech_interview_prep.week_2.ListNode{:val 4, :next #tech_interview_prep.week_2.ListNode{:val 3, :next #tech_interview_prep.week_2.ListNode{:val 2, :next #tech_interview_prep.week_2.ListNode{:val 1, :next nil}}}}

