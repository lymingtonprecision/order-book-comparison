(ns obc.order-line
  (:use clojure.set obc.order-line-util)
  (:require [clojure.string :as s]))

(defn invoiced?
  "Returns a true if the line has been invoiced, false otherwise"
  [{status :status}]
  (if (nil? status)
    false
    (-> status s/lower-case (= "invoiced"))))

(defn is-now-invoiced?
  "Compares two instances of the, presumed, same line
   if the first instance is not invoiced but the later is returns true
   otherwise returns false"
  [l1 l2]
  (if (invoiced? l1) false (invoiced? l2)))

(defn is-for-tooling?
  "Returns true if the line is for a tooling part, otherwise false"
  [{part :customer-part}]
  (is-tooling-part? part))

(defmulti diff-field (fn [field & values] field))

(defmethod diff-field :default
  [_ & values]
  false)

(defmethod diff-field :customer-part
  [_ & parts]
  (not
    (or
      (every? is-tooling-part? parts)
      (apply same-alpha-numeric? parts))))

(defmethod diff-field :customer-issue
  [_ & issues]
  (not (apply same-alpha-numeric? issues)))

(defmethod diff-field :qty-due
  [_ & qtys]
  (not (apply == qtys)))

(defmethod diff-field :unit-price
  [_ & prices]
  (not (apply = (map #(format "%.2f" (float %)) prices))))

(doseq [k [:due-date :performance-date]]
  (defmethod diff-field k
    [_ & dates]
    (not (apply same-day? dates))))

(defn diff
  "Compares two lines and returns a map of the differences between them
   consisting of the name of the differing field as the key and a sequence
   of the values in argument order:

   (diff {:qty-due 5 :unit-price 40} {:qty-due 7 :unit-price 40})
   => {:qty-due [5 7]}"
  [l1 l2]
  (let [fields (intersection (-> l1 keys set) (-> l2 keys set))]
    (reduce
      (fn
        [d f]
        (let [v1 (f l1)
              v2 (f l2)]
          (if (diff-field f (apply-offset f l1) (apply-offset f l2))
            (assoc d f [v1 v2])
            d)))
      {}
      fields)))
