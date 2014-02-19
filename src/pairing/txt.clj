(ns pairing.txt
  [:require [clojure.core.strint :refer [<<]]]
  [:require [clojure.string :refer [join]]]
  [:require [pairing.core :refer [unmatched-days-for-pair]]])

(defn report-person
  [p]
  (<< "~(:first-name p) ~(:last-name p) (FA# ~(:id p), ~(:check-in p) -> ~(:check-out p))"))

(defn pair-info
  [one two]
  (<< "Unmatched days: ~(unmatched-days-for-pair [one two])"))

(defn report-pair
  "Produces a string that give you all the information on a pair: the
  info on the individuals, plus the pair id (given), plus extra pair
  info."

  [pair-id [one two]]
  (<< "Pair #~{pair-id}\n~(report-person one)\n~(report-person two)\n~(pair-info one two)"))

(defn report-pairs
  [pairs]
  (let [[last-id lines] (reduce (fn [[id lines] pair]
                                  [(inc id) (conj lines (report-pair id (seq pair)))])
                                [1 []]
                                pairs)]
    lines))

(defn report-leftovers
  [leftovers]
  (concat ["Leftovers"] (map report-person leftovers)))

(defn report
  "Reports on a pairing. Takes a pairing, i.e. the result of a pairing
  function, which has structure [pairs leftovers]."

  [[pairs leftovers]]
  (join [(join "\n\n" (report-pairs pairs))
         "\n\n"
         (join "\n" (report-leftovers leftovers))]))
