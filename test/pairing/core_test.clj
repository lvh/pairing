(ns pairing.core-test
  (:require [clojure.test :refer :all]
            [clj-time.core :as t]
            [pairing.core :refer :all]))

(def jan-10th (t/local-date 2014 01 10))
(def jan-15th (t/local-date 2014 01 15))
(def jan-20th (t/local-date 2014 01 20))
(def jan-21st (t/local-date 2014 01 21))

(def alice {:name "Alice"
            :gender :female
            :start-date jan-10th
            :end-date jan-20th})
(def bob {:name "Bob"
          :gender :male
          :start-date jan-10th
          :end-date jan-15th})
(def carol {:name "Carol"
            :gender :female
            :start-date jan-10th
            :end-date jan-20th})
(def dave {:name "Dave"
           :gender :male
           :start-date jan-10th
           :end-date jan-15th})
(def ewa {:name "Ewa"
          :gender :female
          :start-date jan-10th
          :end-date jan-21st})
(def felix {:name "Fix-it Felix"
            :gender :male
            :start-date jan-15th
            :end-date jan-20th})
(def mallory {:name "Mallory"
              :gender :male
              :start-date (t/local-date 666 6 6)
              :end-date (t/local-date 666 6 10)})

(deftest prefs-tests
  (testing "get preferences from a user"
    (is (= (prefs alice)
           [:female (:start-date alice) (:end-date alice)]))))

(deftest group-by-prefs-tests
  (testing "empty pool"
    (is (= (group-by-prefs []) {})))
  (testing "one person"
    (is (= (group-by-prefs [alice])
           {(prefs alice) [alice]})))
  (testing "two unpairable people"
    (is (= (group-by-prefs [alice bob])
           {(prefs alice) [alice]
            (prefs bob) [bob]})))
  (testing "two pairable people"
    (is (= (group-by-prefs [alice carol])
           {(prefs alice) [alice carol]})))
  (testing "two pairable people plus one unpairable person"
    (is (= (group-by-prefs [alice bob carol])
           {(prefs alice) [alice carol]
            (prefs bob) [bob]}))))

(deftest pairs-and-leftovers-tests
  (testing "empty -> no pairs, no leftovers"
    (is (= (pairs-and-leftovers [])
           [[]
            []])))
  (testing "one person -> no pairs, one leftover"
    (is (= (pairs-and-leftovers [alice])
           [[]
            [alice]])))
  (testing "two persons -> one pair, no leftovers"
    (is (= (pairs-and-leftovers [alice bob])
           [[[alice bob]]
            []])))
  (testing "three persons -> one pair, one leftover"
    (is (= (pairs-and-leftovers [alice bob carol])
           [[[alice bob]]
            [carol]]))))

(deftest pair-prefs-tests
  (testing "empty"
    (is (= (pair-prefs [])
           [[] []])))
  (testing "one pair, one extra"
    (is (= (pair-prefs [alice bob carol])
           [[[alice carol]]
            [bob]])))
  (testing "two pairs"
    (is (= (pair-prefs [alice bob carol dave])
           [[[alice carol] [bob dave]]
            []])))
  (testing "two pairs, one extra"
    (is (= (pair-prefs [alice bob carol dave mallory])
           [[[alice carol] [bob dave]]
            [mallory]])))
  (testing "one pair, two unmatchable leftovers"
    (is (= (pair-prefs [alice bob carol mallory])
           [[[alice, carol]],
            [bob, mallory]]))))

(deftest unmatched-days-tests
  (testing "perfect matches"
    (is (= (unmatched-days (at-conf alice) (at-conf carol))
           0)))
  (testing "same start day, one day difference in end day"
    (is (= (unmatched-days (at-conf alice) (at-conf ewa))
           1)))
  (testing "same start day, five days difference in end day"
    (is (= (unmatched-days (at-conf alice) (at-conf bob))
           5)))
  (testing "same end day, five days difference in start day"
    (is (= (unmatched-days (at-conf alice) (at-conf felix))
           5))))

(deftest days-difference-tests
  (testing "equal dates"
    (is (= (days-difference jan-10th jan-10th)
           0)))
  (testing "different dates, first before second"
    (is (= (days-difference jan-10th jan-15th)
           5)))
  (testing "different dates, second before first"
    (is (= (days-difference jan-15th jan-10th)
           5))))

(deftest group-by-unmatched-days-tests
  (testing "empty"
    (is (= (group-by-unmatched-days [])
           {})))
  (testing "one pair with exactly matching days"
    (is (= (group-by-unmatched-days [alice carol])
           {0 [#{alice carol}]}))))

(deftest pair-subgroup-days-tests
  (testing "two pairs with exact matches"
    (is (= (pair-subgroup-days [alice bob carol dave])
           [#{#{alice carol}
              #{bob dave}}
            #{}]))))

(deftest pair-days-tests
  (testing "two pairs with exact matches"
    (is (= (pair-days [alice bob carol dave])
           [#{#{alice carol}
              #{bob dave}}
            #{}])))
  (testing "two pairs plus one leftover with bad date incompatibility"
    (is (= (pair-days [alice bob carol dave mallory])
           [#{#{alice carol}
              #{bob dave}}
            #{mallory}]))))

(def alice-preffing-carol (assoc alice :partner-name "Carol"))
(def carol-preffing-alice (assoc carol :partner-name "Alice"))
(def bob-preffing-dave (assoc bob :partner-name "Dave"))
(def dave-preffing-bob (assoc dave :partner-name "Bob"))

(deftest pair-partners-tests
  (testing "with no partner information, pool goes straight to leftovers"
    (is (let [pool #{alice bob carol dave}]
          (= (pair-partners pool)
             [#{} pool]))))
  (testing "with partner information, partners are matched up"
    (is (= (pair-partners #{alice-preffing-carol
                            carol-preffing-alice
                            bob-preffing-dave
                            dave-preffing-bob})
           [#{#{alice-preffing-carol carol-preffing-alice}
              #{bob-preffing-dave dave-preffing-bob}}
            #{}]))))
