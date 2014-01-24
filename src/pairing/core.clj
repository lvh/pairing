(ns pairing.core
  [:require [clj-time.core :as t]]
  [:require [clojure.math.combinatorics :as c]])

(def reqs
  "Gets the grouping requirements for a user (gender)."
  (juxt :gender))

(def at-conf
  "Gets [start-date end date] for a user, the dates when the user will
  be arriving and leaving from the conference."
  (juxt :start-date :end-date))

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

(defn pair-prefs
  "Pairs people from pool with exactly matching prefs. Returns pairs and
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

(defn date-as-instant
  "Turns a date into a readable time instant (same day, noon Zulu)."
  [date]
  (apply t/date-time
         (conj ((juxt t/year t/month t/day) date) ;; this day
               12) ;; noon
         ))

(defn days-difference
  "Gets the number of days between two dates."
  [day-one day-two]
  (let [[start end] (sort [day-one day-two])]
    (let [[start end] (map date-as-instant [start end])]
      (t/in-days (t/interval start end)))))

(defn unmatched-days
  "Gets the number of unmatched days from the intervals that two users are staying."
  [[start-one end-one] [start-two end-two]]
  (+ (days-difference start-one start-two)
     (days-difference end-one end-two)))
