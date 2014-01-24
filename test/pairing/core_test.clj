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

(deftest pair-matching-prefs-tests
  (testing "empty"
    (is (= (pair-matching-prefs [])
           [[] []])))
  (testing "one pair, one extra"
    (is (= (pair-matching-prefs [alice bob carol])
           [[[alice carol]]
            [bob]])))
  (testing "two pairs"
    (is (= (pair-matching-prefs [alice bob carol dave])
           [[[alice carol] [bob dave]]
            []])))
  (testing "two pairs, one extra"
    (is (= (pair-matching-prefs [alice bob carol dave mallory])
           [[[alice carol] [bob dave]]
            [mallory]])))
  (testing "one pair, two unmatchable leftovers"
    (is (= (pair-matching-prefs [alice bob carol mallory])
           [[[alice, carol]],
            [bob, mallory]]))))
