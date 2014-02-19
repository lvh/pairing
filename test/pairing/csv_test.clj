(ns pairing.csv-test
  (:require [pairing.csv :refer :all]
            [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [clj-time.core :as t]))

(def header-line "id,name,check-in,check-out,partner id,notes")
(def lvh-line "1,lvh,14-04-07,14-04-10,2,")
(def ewa-line "2,ewa,14-04-07,14-04-10,1,")
(def csv-data (join "\n" [header-line lvh-line ewa-line]))

(def ci-date (t/local-date 2014 4 7))
(def co-date (t/local-date 2014 4 10))

(deftest parse-pairing-csv-tests
  (testing "parses empty csv correctly"
    (is (= (parse-pairing-csv header-line)
         #{})))
  (testing "parses csv with some lines in correctly"
    (is (= (parse-pairing-csv csv-data)
           #{{:id 1 :name "lvh" :check-in ci-date :check-out co-date :partner-id 2 :notes ""}
             {:id 2 :name "ewa" :check-in ci-date :check-out co-date :partner-id 1 :notes ""}}))))
