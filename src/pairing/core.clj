(ns pairing.core
  [:require [clj-time.core :refer (local-date)]])

(def prefs
  "Gets the grouping preferences for a user (gender, start-date, end-date)."
  (juxt :gender :start-date :end-date))

(defn group-by-prefs
  "Groups the given pool of users by gender, start-date, end-date."
  [pool]
  (group-by prefs pool))

(defn pairs-and-leftovers
  "Gets [pairs, leftovers] in subgroup."
  [subgroup]
  (let [[pairs leftovers] (split-with #(= (count %) 2) (partition-all 2 subgroup))]
    [pairs (apply concat leftovers)]))

(defn pair-with-matching-prefs
  "Pairs people from pool with matching prefs. Returns pairs and
  remaining ungrouped people."
  [pool]
  (loop [all-pairs []
         all-leftovers []
         subgroups (vals (group-by-prefs pool))]
    (if (seq subgroups)
      (let [[new-pairs new-leftovers] (pairs-and-leftovers (first subgroups))]
        (recur (into all-pairs new-pairs)
               (into all-leftovers new-leftovers)
               (next subgroups)))
      [all-pairs all-leftovers])))
