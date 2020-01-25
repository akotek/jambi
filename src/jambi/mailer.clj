(ns jambi.mailer
  (:require [clojure.tools.logging :as logger]
            [clojure.string :as s]
            [jambi.smtp :as smtp]))


;; CONSTANTS
; ---------------------
(def ^:const regex-space-quotes #"\s(?=(?:[^\']*'[^']*')*[^']*$)")
(def ^:const success {:code 1, :message "SUCCESS", :desc ""})
(def ^:const usage-msg "usage: <mailer -s [subject] -b [body] -t [first@mail.com,second@mail.com]>")
; ---------------------

(defn to-map [lst]
  (let [chopped-even (map #(subs % 1) (take-nth 2 lst))
        odd (take-nth 2 (rest lst))
        combine (fn [fst scd] (hash-map (keyword fst) scd))]
    (apply merge (map combine chopped-even odd))))


(defn validate-map [map]
  (logger/debug "validating map...")
  (let [expected [:s :b :t]]
    (if (or (not= (keys map) (seq expected))
            (some s/blank? (vals map)))
      throw (IllegalArgumentException. "invalid input: wrong format")
      map)))

(defn help-handler
  (logger/debug "handling help request...")
  (printf usage-msg)
  (update success :desc #(str "help shown to user" %)))

(defn send-handler [mail]
  (logger/debug (format "starting to ship mail of %" mail))
  (try
    (smtp/send mail)
    (logger/debug "successfully shipped mail")
    (update success :desc #(str "message sent successfully" %)))
  (catch Exception e
    throw (IllegalStateException. (str "error sending smtp : " (str e)))))

(defn ship [input]
  (logger/debug "handling input request")
  (let [splitted (s/split input regex-space-quotes)
        num-args (count splitted)
        fst (first splitted)]
    (try
      (cond
        (and (= (num-args) 1) (= (fst "-h"))) (help-handler)
        (= (num-args) 6) (send-handler (validate-map (to-map splitted)))
        :else throw (IllegalArgumentException. "invalid input: wrong args")))
    (catch Exception e
      (let [err (str e)
            resp {:code 0, :message "FAILED", :desc err}]
        (logger/error err)
        (printf resp)))))


(defn -main [& args]
  (ship args))

