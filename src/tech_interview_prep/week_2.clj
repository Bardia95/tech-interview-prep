(ns tech-interview-prep.week-2)


(defrecord ListNode [val next])
;; => tech_interview_prep.week_2.ListNode

(defn reverse-list [head]
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

