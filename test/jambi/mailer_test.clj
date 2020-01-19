(ns jambi.mailer-test
  (:require [clojure.test :refer :all]))


;; CONSTANTS
;; --------------------------------
;; "-s 'i am' -b 'a valid' -t 'format@gmail.com'"
;; ==> 6x args
(def ^:const subject "-s 'simple-subject")
(def ^:const body "-b simple-body")
(def ^:const to-mail "-t devkotek@gmail.com")
;; --------------------------------


(deftest test-ship-valid-core
  (let [super-valid (str subject body to-mail)
        no-body (str subject "-b '' " to-mail)
        with-cc (str no-body ",kotekmath@gmail.com")
        expected {:code 1, :message "SUCCESS", :desc "message sent successfully"}]
    (is (= (ship super-valid) expected))
    (is (= (ship no-body) expected))
    (is (= (ship with-cc) expected))))

(deftest test-ship-valid-help
  (let [help-in ("-h")
        expected {:code 1, :message "SUCCESS", :desc "help shown to user"}]
    (is (= (ship help-in)) expected)))

(deftest test-ship-invalid
  (let [not-enough-args (str "" (apply str (repeat (rand-int 5) "a")))
        no-subject (str "-s '' " body to-mail)
        no-body (str subject "-b '' " to-mail)
        bad-mail (str subject body "i.am.bad.email")
        expected {:code 0, :message "FAILED", :desc "input is invalid"}]
    (is (= (ship not-enough-args) expected))
    (is (= (ship no-subject) expected))
    (is (= (ship no-body) expected))
    (is (= (ship bad-mail) expected))))