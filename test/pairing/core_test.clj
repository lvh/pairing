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

(deftest prefs-test
  (testing "get preferences from a user"
    (is (= (prefs alice)
           [:female (:start-date alice) (:end-date alice)]))))

(deftest group-by-prefs-test
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
