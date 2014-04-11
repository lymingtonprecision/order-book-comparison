(ns obc.t-order-line
  (:use midje.sweet)
  (:use obc.order-line)
  (:import java.util.Date java.sql.Timestamp)
  (:require [obc.order-line-util :as u] [clojure.string :as s]))

(facts "about `invoiced?`"
  (fact "is true when the line's status is 'invoiced' in any case"
    (tabular
      (invoiced? {:status ?status}) => ?expected
        ?status    ?expected
        "INVOICED" true
        "inVoiCed" true
        "invoiced" true
        "Released" false))
  (fact "is false when the line has no status"
    (invoiced? {}) => false))

(facts "about `is-now-invoiced?`"
  (let [released {:status "Released"}
        partially-delivered {:status "PartiallyDelivered"}
        invoiced {:status "Invoiced"}
        other-invoiced {:status "Invoiced"}]
    (tabular
      (is-now-invoiced? ?status1 ?status2) => ?expected
        ?status1 ?status2            ?expected
        released invoiced            true
        invoiced released            false
        invoiced other-invoiced      false
        released partially-delivered false)))

(facts "about `is-for-tooling?`"
  (fact "checks the customers part with `is-tooling-part?`")
    (is-for-tooling? {:customer-part ..part1..}) => true
    (provided
      (u/is-tooling-part? ..part1..) => true)
    (is-for-tooling? {:customer-part ..part2..}) => false
    (provided
      (u/is-tooling-part? ..part2..) => false))

(fact "our expected output from `diff` is"
  (let [d (Date.)
        l1 {:order-book-id 1
            :order-no "L3456"
            :customer-po "CPL1020-34"
            :line-no 12
            :rel-no 1
            :part-no "100123456R01"
            :customer-part "X102030103"
            :customer-issue "AB"
            :due-date d
            :performance-date (Date. (+ (.getTime d) (* 28 u/seconds-per-day)))
            :date-offset nil
            :qty-due 4
            :unit-price 10.01
            :status "Released"
            :data nil}
        l2 {:data "CPL1020-34,12,1,X102030103,AB,2014-04-01,2014-04-14,5,10"
            :order-book-id 4
            :order-no "CPL1020-34"
            :customer-po "CPL1020-34"
            :line-no 12
            :rel-no 2
            :customer-part "X102030113"
            :customer-issue "AC"
            :due-date d
            :performance-date (Date. (+ (.getTime d) (* 14 u/seconds-per-day)))
            :date-offset 14
            :qty-due 5
            :unit-price 10
            :status "Released"}
        d (reduce
            (fn [h k] (assoc h k [(k l1) (k l2)]))
            {}
            [:customer-part
             :customer-issue
             :due-date
             :qty-due
             :unit-price])]
    (diff l1 l2) => d))

(facts "about `diff`"
  (let [l {:order-book-id 1
           :order-no 1
           :customer-po 1
           :line-no 1
           :rel-no 1
           :part-no "100123456R01"
           :customer-part "X102030103"
           :customer-issue "AB"
           :due-date (Date.)
           :performance-date (Date.)
           :date-offset nil ; 38 if from SV and customer = 1030, 14 if 1032
           :qty-due 5
           :unit-price 10
           :status "Released"
           :data nil}]
    (facts "it acknowledges differences in"
      (fact "customer part"
        (let [k :customer-part
              nl (assoc l k "D4567")
              d {k [(k l) (k nl)]}]
          (diff l nl) => (contains d)))
      (fact "customer part issue"
        (let [k :customer-issue
              nl (assoc l k "AC")
              d {k [(k l) (k nl)]}]
          (diff l nl) => (contains d)))
      (fact "unit price within 2 DP"
        (let [k :unit-price
              p (k l)
              nl (assoc l k 5.006)
              d {k [(k l) (k nl)]}]
          (tabular
            (diff l (assoc l k ?price)) => (contains ?expected)
              ?price      ?expected
              (+ p 1)     (assoc d k [p ?price])
              (+ p 0.1)   (assoc d k [p ?price])
              (+ p 0.01)  (assoc d k [p ?price])
              (+ p 0.001) {}
              (+ p 0.006) (assoc d k [p ?price]))))
      (fact "qty due"
        (let [k :qty-due
              nl (assoc l k (* (k l) 2))
              d {k [(k l) (k nl)]}]
          (diff l nl) => (contains d)))
      (doseq [k [:due-date :performance-date]]
        (fact (str k " with no offset")
          (let [nl (assoc l k (Date. (+ (.getTime (k l)) u/seconds-per-day)))
                d {k [(k l) (k nl)]}]
            (diff l nl) => (contains d)))
        (fact (str k " not matching offset")
          (let [nl (assoc l :date-offset 10)
                d {k [(k l) (k nl)]}]
            (diff l nl) => (contains d)))))
    (facts "it ignores differences in"
      (doseq [k [:due-date :performance-date]]
        (fact (str k " matching delivery offset")
          (let [o 10
                os (* o u/seconds-per-day)
                l (assoc l :date-offset o)
                nl (assoc l k (Date. (+ (.getTime (k l)) os)))]
            (diff l nl) =not=> (contains k))))
      (fact "status"
        (let [nl (assoc l :status "Invoiced")]
          (diff l nl) =not=> (contains :status)))
      (fact "order details"
        (let [ks [:order-book-id :order-no :customer-po :line-no :rel-no]
              nl (reduce
                   (fn
                     [h [k v]]
                     (assoc h k (if (contains? ks k) (inc v) v)))
                   {}
                   l)]
          (diff l nl) =not=> (contains ks)))
      (fact "data"
        (let [nl (assoc l :data "1,1,3,PO121415,D35460,AC,5,5.76")]
          (diff l nl) =not=> (contains :data)))))) 

(facts "about `diff-field`s default behaviour"
  (fact "it returns false"
    (diff-field :default "abc" 123) => false))

(facts "about `diff-field :customer-part`"
  (let [diff (partial diff-field :customer-part)
        part1 "abc"
        part2 "123"]
    (fact "is false when both parts are tooling"
      (diff ..part1.. ..part2..) => false
      (provided
        (u/is-tooling-part? ..part1..) => true
        (u/is-tooling-part? ..part2..) => true))
    (fact "is true when only one part is tooling"
      (diff part1 part2) => true
      (provided
        (u/is-tooling-part? part1) => true
        (u/is-tooling-part? part2) => false))
    (fact "is false when both parts are the same, regardless of case"
      (diff part1 (s/upper-case part1)) => false)
    (fact "is true when the parts are different"
      (diff part1 part2) => true)
    (fact "is false when the parts are the same alphanumeric sequence"
      (diff "a-bcd-123-fg9" "abc.d1-23+f/g9") => false)))

(facts "about `diff-field :customer-issue`"
  (let [diff (partial diff-field :customer-issue)
        issue1 "AA"
        issue2 "001"]
    (fact "is false when both issues are the same, regardless of case"
      (diff issue1 (s/upper-case issue1)) => false
    (fact "is true when the issues are different"
      (diff issue1 issue2) => true)
    (fact "is false when the issues are the same alphanumeric sequence"
      (diff "AB-1" "AB/1") => false))))

(facts "about `diff-field :qty-due`"
  (let [diff (partial diff-field :qty-due)]
    (fact "is false when the quantities are the same"
      (diff 1 1) => false
      (diff 2.34 2.34) => false)
    (fact "is true when the quantities are different"
      (diff 1 2) => true
      (diff 2.34 2.33) => true)))

(facts "about `diff-field :unit-price`"
  (let [diff (partial diff-field :unit-price)]
    (fact "is false when the prices are the same, to 2 decimal places"
      (diff 1 1) => false
      (diff 1.123 1.124) => false)
    (fact "is true when the prices are different, to 2 decimal places"
      (diff 1 2) => true
      (diff 1.124 1.125) => true)))

(facts "about `diff-field :due-date`"
  (let [diff (partial diff-field :due-date)
        today (Date.)
        later-today ((fn [d] (.setMinutes d (-> d .getMinutes inc)) d) (Date.))
        tomorrow ((fn [d] (.setDate d (-> d .getDate inc)) d) (Date.))]
    (fact "is false when given dates at different times of the same day"
      (diff today later-today) => false)
    (fact "is false when given differing instance of the same time"
      (diff today (Timestamp. (.getTime today))) => false)
    (fact "is true when given different dates"
      (diff today tomorrow) => true
      (diff today (Timestamp. (.getTime tomorrow))) => true
      (diff
        (Timestamp. (.getTime today))
        (Timestamp. (.getTime tomorrow))) => true)))

(facts "about `diff-field :performance-date`"
  (let [diff (partial diff-field :performance-date)
        today (Date.)
        later-today ((fn [d] (.setMinutes d (-> d .getMinutes inc)) d) (Date.))
        tomorrow ((fn [d] (.setDate d (-> d .getDate inc)) d) (Date.))]
    (fact "is false when given dates at different times of the same day"
      (diff today later-today) => false)
    (fact "is false when given differing instance of the same time"
      (diff today (Timestamp. (.getTime today))) => false)
    (fact "is true when given different dates"
      (diff today tomorrow) => true
      (diff today (Timestamp. (.getTime tomorrow))) => true
      (diff
        (Timestamp. (.getTime today))
        (Timestamp. (.getTime tomorrow))) => true)))
