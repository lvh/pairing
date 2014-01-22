(ns pairing.core-test
  (:import java.util.Date)
  (:require [clojure.test :refer :all]
            [clj-time.core :refer (local-date)]
            [pairing.core :refer :all]))

(def alice {:name "Alice"
            :gender :female
            :start-date (local-date 2014 01 10)
            :end-date (local-date 2014 01 20)})
(def bob {:name "Bob"
          :gender :male
          :start-date (local-date 2014 01 10)
          :end-date (local-date 2014 01 15)})
(def carol {:name "Carol"
            :gender :female
            :start-date (local-date 2014 01 10)
            :end-date (local-date 2014 01 20)})
(def dave {:name "Dave"
           :gender :male
           :start-date (local-date 2014 01 10)
           :end-date (local-date 2014 01 15)})
(def mallory {:name "Mallory"
              :gender :male
              :start-date (local-date 666 6 6)
              :end-date (local-date 666 6 10)})

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

(deftest pair-with-matching-prefs-tests
  (testing "empty"
    (is (= (pair-with-matching-prefs [])
           [[] []])))
  (testing "one pair, one extra"
    (is (= (pair-with-matching-prefs [alice bob carol])
           [[[alice carol]]
            [bob]])))
  (testing "two pairs"
    (is (= (pair-with-matching-prefs [alice bob carol dave])
           [[[alice carol] [bob dave]]
            []])))
  (testing "two pairs, one extra"
    (is (= (pair-with-matching-prefs [alice bob carol dave mallory])
           [[[alice carol] [bob dave]]
            [mallory]])))
  (testing "one pair, two unmatchable leftovers"
    (is (= (pair-with-matching-prefs [alice bob carol mallory])
           [[[alice, carol]],
            [bob, mallory]]))))
