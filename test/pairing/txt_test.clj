(ns pairing.txt-test
  (:require [pairing.txt :as r]
            [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [clj-time.core :as t]))

(def jan-10th (t/local-date 2014 01 10))
(def jan-15th (t/local-date 2014 01 15))
(def jan-20th (t/local-date 2014 01 20))
(def jan-21st (t/local-date 2014 01 21))
(def alice {:id 1
            :first-name "Alice"
            :last-name "Testsson"
            :gender :female
            :check-in jan-10th
            :check-out jan-20th})
(def bob {:id 2
          :first-name "Bob"
          :last-name "Testoni"
          :gender :male
          :check-in jan-10th
          :check-out jan-15th})
(def carol {:id 3
            :first-name "Carol"
            :last-name "Testamazzi"
            :gender :female
            :check-in jan-10th
            :check-out jan-20th})
(def dave {:id 4
           :first-name "Dave"
           :last-name "Tester"
           :gender :male
           :check-in jan-10th
           :check-out jan-15th})
(def mallory {:id 666
              :first-name "Mallory"
              :last-name "Evilsson"
              :gender :male
              :check-in (t/local-date 666 6 6)
              :check-out (t/local-date 666 6 10)})

(deftest report-pair-tests
  (testing "report a pair"
    (is (= (r/report-pair 1 [alice carol])
           (join "\n" ["Pair #1"
                       "Alice Testsson (FA# 1, 2014-01-10 -> 2014-01-20)"
                       "Carol Testamazzi (FA# 3, 2014-01-10 -> 2014-01-20)"
                       "Unmatched days: 0"])))))

(deftest report-pairs-tests
  (testing "report two pairs, assuming report-pair is correct"
    (is (= (r/report-pairs #{#{alice carol} #{bob dave}})
           [(r/report-pair 1 [alice carol])
            (r/report-pair 2 [bob dave])]))))

(deftest report-leftovers-tests
  (testing "report one leftover"
    (is (= (r/report-leftovers #{mallory})
           ["Leftovers"
            "Mallory Evilsson (FA# 666, 0666-06-06 -> 0666-06-10)"]))))

(deftest report-tests
  (testing "simple report"
    (is (= (r/report [#{#{alice carol} #{bob dave}} #{mallory}])
           (join "\n" ["Pair #1"
                       "Alice Testsson (FA# 1, 2014-01-10 -> 2014-01-20)"
                       "Carol Testamazzi (FA# 3, 2014-01-10 -> 2014-01-20)"
                       "Unmatched days: 0"
                       ""
                       "Pair #2"
                       "Bob Testoni (FA# 2, 2014-01-10 -> 2014-01-15)"
                       "Dave Tester (FA# 4, 2014-01-10 -> 2014-01-15)"
                       "Unmatched days: 0"
                       ""
                       "Leftovers"
                       "Mallory Evilsson (FA# 666, 0666-06-06 -> 0666-06-10)"])))))
