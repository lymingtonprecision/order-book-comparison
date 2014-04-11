(ns obc.t-order-line-util
  (:use midje.sweet)
  (:use obc.order-line-util)
  (:import java.util.Date java.sql.Timestamp))

(facts "about `seconds-per-day`"
  (fact "is equal to the number of milliseconds per 24 hours"
    seconds-per-day => (* 1000 60 60 24)))

(facts "about `apply-offset`"
  (fact "it applies date offsets to date fields"
    (let [d (Date.)
          o 10
          od (Date. (+ (.getTime d) (* o seconds-per-day)))]
      (apply-offset :due-date {:due-date d :date-offset o}) => od)))

(facts "about `is-tooling-part?`"
  (fact "is true when part part contains 'tooling'"
    (is-tooling-part? "some tooling bits") => true
    (is-tooling-part? "tOOLing") => true
    (is-tooling-part? "tooling") => true)
  (fact "is false when the part doesn't contain 'tooling'"
    (is-tooling-part? "X102345789") => false)
  (fact "is false when the part is nil"
    (is-tooling-part? nil) => false))
