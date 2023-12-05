(ns aoc2023.util
  "Utils for AOC2023"
  (:require [clojure.string :as str]
            [clojure.pprint :as p]
            [clojure.java.io :as io]))

(defn str->int [str] (Integer/parseInt str))
(defn str->long [str] (Long/parseLong str))
(defn char->int [char] (Character/digit char 10))

(defn load-input
  [filename]
  (-> filename
      io/resource
      slurp
      str/split-lines))

(defn dbg
  "Debug print value"
  [value]
  (p/pprint value)
  value)

(defn dbgv
  "Debug print type"
  [value]
  (println (type value))
  value)

(defmacro dbgf
  "Debug print form"
  [form]
  (p/pprint form)
  form)

(defmacro varmista
  ([f nimi] `(~f ~(str nimi)))
  ([f nimi pitäisi]
   `(let [tulos# (~f ~(str nimi))]
      (assert (= tulos# ~pitäisi))
      :ok!)))
