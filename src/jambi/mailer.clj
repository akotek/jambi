(ns jambi.mailer
  (:require [clojure.tools.logging :as logger]
            [clojure.string :as s]
            [jambi.smtp :as smtp]))


;; CONSTANTS
; ---------------------
(def ^:const regex-space-quotes #"\s(?=(?:[^\']*'[^']*')*[^']*$)")
(def ^:const valid-msg "mailer -s [subject] -b [body] -t [first@mail.com,...] -cc [first@mail.com,...]")
(def ^:const usage-msg (str "usage: " "<" valid-msg ">"))
(def ^:const success 0)
(def valid-args (count (s/split valid-msg #" ")))
; ---------------------

(def errors {0 [:SUCCESS    "message sent"]
             1 [:HELP       "help shown to user"]
             2 [:USAGE      "wrong usage"]})

(defn parse [c]
  (let [[e msg] (errors c)]
    {:code c
     :error e
     :message msg}))

(defn to-map [lst]
  (let [chopped-even (map #(subs % 1) (take-nth 2 lst))
        odd (take-nth 2 (rest lst))
        combine (fn [fst scd] (hash-map (keyword fst) scd))]
    (apply merge (map combine chopped-even odd))))


(defn validate [map]
  (let [expected [:s :b :t :cc]]
    (if (or (not= (keys map) (seq expected)) (some s/blank? (vals map)))
      throw (IllegalArgumentException. "invalid input: wrong format")
      map)))

(defn help
  (printf usage-msg)
  (parse 1))

(defn send [mail]
  (try
    (smtp/send mail)
    (logger/debug "successfully shipped mail")
    (parse success)
  (catch Exception e
    throw (IllegalStateException. (str "error sending smtp : " (str e))))))


(defn ship [input]
  (let [splitted (s/split input regex-space-quotes)
        num-args (count splitted)
        fst (first splitted)]
    (try
      (cond
        (and (= (num-args) 1) (= (fst "-h"))) (help)
        (= (num-args) valid-args) (->> splitted
                                       (to-map)
                                       (validate)
                                       (send))
        :else throw (IllegalArgumentException. "invalid input: wrong args")))
    (catch Exception e
      (let [err (str e)
            resp (parse 2)]
        (logger/error err)
        (printf resp)))))


(defn -main [& args]
  (ship args))