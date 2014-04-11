(ns obc.order-line-util
  (:import java.util.Date)
  (:require [clojure.string :as s]))

(def seconds-per-day 86400000)

(defn apply-offset
  [f l]
  (let [v (f l)]
    (if (re-find #"date$" (str f))
      (if-let [o (:date-offset l)]
        (Date. (+ (.getTime v) (* o seconds-per-day)))
        v)
      v)))

(defn is-tooling-part? [part]
  (if (nil? part)
    false
    (not (nil? (re-matches #"(?i).*tooling.*" part)))))

(defn same-alpha-numeric? [& strings]
  (let [alpha-numeric #"[^A-Za-z0-9]"]
    (apply = (map #(s/lower-case (s/replace % alpha-numeric "")) strings))))

(defn same-day? [& dates]
  (apply = (map (fn [d] [(.getYear d) (.getMonth d) (.getDate d)]) dates)))
