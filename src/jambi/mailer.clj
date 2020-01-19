(ns jambi.mailer
  (:require [clojure.tools.logging :as logger]
            [clojure.string :as s])
  (:import (clojure.lang ExceptionInfo)))


;; CONSTANTS
; ---------------------
(def ^:const regex-space-quotes #"\s(?=(?:[^\']*'[^']*')*[^']*$)")
(def ^:const success {:code 1, :message "SUCCESS", :desc ""})
(def ^:const usage-msg "usage: <mailer -s [subject] -b [body] -t [first@mail.com,second@mail.com]>")

; ---------------------

(defn send [mail]
  (logger/debug (format "Starting to ship mail of %" mail))
  ;;MAIN ALGORITHM GOES HERE)

  (logger/debug "Successfully shipped mail")
  (update success :desc #(str "message sent successfully" %)))


(defn to-map [lst]
  (let [chopped-even (map #(subs % 1) (take-nth 2 lst))
        odd (take-nth 2 (rest lst))
        combine (fn [fst scd] (hash-map (keyword fst) scd))]
    (apply merge (map combine chopped-even odd))))

(defn validate-map [map]
  (logger/debug "Validating map...")
  (let [expected [:s :b :t]]
    (if (or
          (not= (keys map) (seq expected))
          (some s/blank? (vals map)))
      throw (IllegalArgumentException. "Invalid input: wrong format")
      map)))

(defn help-handler
  (logger/debug "Handling help request...")
  (printf usage-msg)
  (update success :desc #(str "help shown to user" %)))

(defn ship [input]
  (logger/debug "Handling input request")
  (let [splitted (s/split input regex-space-quotes)
        num-args (count splitted)
        fst (first splitted)]
    (try
      (cond
        (and (= (num-args) 1) (= (fst "-h"))) (help-handler)
        (= (num-args) 6) (send (validate-map (to-map splitted)))
        :else throw (IllegalArgumentException. "Invalid input: wrong parameter number")))
    (catch ExceptionInfo e
      throw (IllegalArgumentException. (str e)))))


(defn -main [& args]
  (try
    (ship args))
  (catch Exception e
    (let [err (str e)
          json {:code 0, :message "FAILED", :desc err}]
      (logger/error (err))
      (printf "ERR: %s, %s" err usage-msg)
      json)))

