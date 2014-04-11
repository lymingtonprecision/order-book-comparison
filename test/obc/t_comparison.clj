(ns obc.t-comparison
  (:use midje.sweet)
  (:use obc.comparison))

(facts "about `perform`"
  (fact "it categorizes lines with no differences as matching"
    (perform {:lines {..id.. ..line1..}} {:lines {..id.. ..line2..}}) =>
      {:matching {..id.. ..line1..}}
    (provided
      (obc.order-line/diff ..line1.. ..line2..) => nil))

  (fact "it categorizes lines with differences as different"
    (perform {:lines {..id.. ..line1..}} {:lines {..id.. ..line2..}}) =>
      {:different {..id.. {:sources [..line1.. ..line2..] :diff ..diff..}}}
    (provided
      (obc.order-line/diff ..line1.. ..line2..) => ..diff..))

  (fact "it collects unique lines by argument sequence"
    (let [ob1 {:id 1 :lines {11 ..line11..}}
          ob2 {:id 2 :lines {21 ..line21..}}]
      (perform ob1 ob2) =>
        {:unique [(:lines ob1) (:lines ob2)]}))

  (fact "it categorizes lines that are now invoiced as invoiced"
    (perform {:lines {..id.. ..line1..}} {:lines {..id.. ..line2..}}) =>
      {:invoiced {..id.. ..line2..}}
    (provided
      (obc.order-line/is-now-invoiced? ..line1.. ..line2..) => true)))

