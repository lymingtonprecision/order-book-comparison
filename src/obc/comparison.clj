(ns obc.comparison
  (:require [obc.order-line :as l])
  (:use clojure.set))

(defn unique-lines
  "Returns a two-element sequence of the unique subsets of the given maps,
   in the same order as it's arguments."
  [lines1 lines2]
  (let [ids1 (-> lines1 keys set)
        ids2 (-> lines2 keys set)
        only-in-1 (select-keys lines1 (difference ids1 ids2))
        only-in-2 (select-keys lines2 (difference ids2 ids1))]
    (if
      (every? empty? [only-in-1 only-in-2]) nil
      [only-in-1 only-in-2])))

(defn common-lines
  "Returns the set of common keys between two maps"
  [lines1 lines2]
  (let [ids (intersection (-> lines1 keys set) (-> lines2 keys set))]
    (reduce
      #(assoc %1 %2 [(lines1 %2) (lines2 %2)])
      {}
      ids)))

(defn perform
  "Compares two order books and returns a map of the similarities/differences.

  (perform
    {:source 'ifs :lines {..id.. ..line.. ..id.. ..line..}}
    {:source 'sv :lines {..id.. ..line.. ..id.. ..line..]})
  => {
      :invoiced {..id.. ..line..}
      :matching {..id.. ..line..}
      :unique [{..id.. ..line..} {..id.. ..line..}]
      :different {..id.. {:sources [..line.. ..line..]
                          :diff {..field.. [..value.. ..value..}}}}"
  [{lines1 :lines} {lines2 :lines}]
  (reduce
    (fn [comparison [id [l1 l2 :as lines]]]
      (if (l/is-now-invoiced? l1 l2)
        (assoc-in comparison [:invoiced id] l2)
        (if-let [d (l/diff l1 l2)]
          (assoc-in comparison [:different id] {:sources lines :diff d})
          (assoc-in comparison [:matching id] l1))))
    (if-let [uniques (unique-lines lines1 lines2)] {:unique uniques} {})
    (common-lines lines1 lines2)))

