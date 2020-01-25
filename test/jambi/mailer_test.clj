(ns jambi.mailer-test
  (:require [clojure.test :refer :all])
  (:use jambi.mailer))


;; CONSTANTS
;; --------------------------------
;; "-s 'i am' -b 'a valid' -t 'format@gmail.com'"
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
          exp-format {:code 0, :message "FAILED", :desc "invalid input: wrong format"}
          exp-args (update exp-format :desc #(clojure.string/replace % #"format" "args"))]
      (is (= (ship not-enough-args) exp-args))
      (is (= (ship no-subject) exp-format))
      (is (= (ship no-body) exp-format))
      (is (= (ship bad-mail) exp-args))))