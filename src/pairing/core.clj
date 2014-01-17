(ns pairing.core
  [:require [clj-time.core :refer (local-date)]])

(defn prefs
  "Gets the grouping preferences for a user (gender, start-date, end-date)."
  [x]
  ((juxt :gender :start-date :end-date) x))

(defn group-by-prefs
  "Groups the given pool of users by gender, start-date, end-date."
  [pool]
  (group-by prefs pool))
