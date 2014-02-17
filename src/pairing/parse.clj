(ns pairing.parse
  [:require [clojure-csv.core :refer [parse-csv]]]
  [:require [clojure.string :as s]]
  [:require [clj-time.format :as tf]])

(defn- keywordify
  "Keywordifies a string. Like keyword, except first lowercases and
  turns spaces into dashes."
  [s]
  (keyword (s/lower-case (s/replace s " " "-"))))

(defn- discard-empty
  [rows]
  (remove #(every? empty? %) rows))

(defn- parse-date
  [s]
  (tf/parse-local-date (tf/formatter-local "yy-MM-dd") s))

(def ^:private special-parse-rules
  "Functions for parsing specific values in CSV entries."
  {:check-in parse-date
   :check-out parse-date})

(defn- fix-entry
  [entry]
  (loop [entry entry
         rem-rules special-parse-rules]
    (if (seq rem-rules)
      (let [[k v] (first rem-rules)
            f (get special-parse-rules k)]
        (if (nil? f)
          (recur entry (rest rem-rules))
          (recur (assoc entry k (f v))
                 (rest rem-rules))))
      entry)))

(defn parse-pairing-csv
  "Parses a room pairing CSV.

  Empty rows (rows where all entries are the empty string) are
  discarded.
  "
  [s]
  (let [[header-strs & rows] (parse-csv s)
        rows (discard-empty rows)
        header (map keywordify header-strs)
        make-entry (partial zipmap header)]
    (into #{} (map make-entry rows))))
