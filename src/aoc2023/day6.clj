(ns aoc2023.day6
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.java.io :as io]
            [aoc2023.util :as u]))

(defn travel-factory [x]
  (fn [y] (- (* x y) (* y y))))

(defn parse [lines]
  (let [times (re-seq #"\d+" (first lines))
        distances (re-seq #"\d+" (second lines))
        zipped (map #(vector (u/str->int %1) (u/str->int %2)) times distances)]
    zipped))

(defn parse2 [lines]
  (let [times (re-seq #"\d+" (first lines))
        distances (re-seq #"\d+" (second lines))]
    [(u/str->long (apply str times)) (u/str->long (apply str distances))]))

(defn find-limits [f x l]
  (let [r1 (int (math/ceil (/ (- x (math/sqrt (- (* x x) (* 4 l)))) 2)))
        r2 (int (math/floor (/ (+ x (math/sqrt (- (* x x) (* 4 l)))) 2)))
        v1 (f r1)
        v2 (f r2)]
    [(if (= v1 l) (inc r1) r1)
     (if (= v2 l) (dec r2) r2)]))

(defn find-wins [[time distance]]
  (let [solve (travel-factory time)
        [smallest largest] (find-limits solve time distance)]
    (inc (- largest smallest))))

(defn part1 [filename]
  (->>  filename u/load-input
        parse
        (map find-wins)
        (apply *)))

(defn part2 [filename]
  (->>  filename u/load-input
        parse2
        find-wins))

(u/varmista part1 "day6_example" 288)
(u/varmista part1 "day6_input" 128700)
(u/varmista part2 "day6_example" 71503)
(u/varmista part2 "day6_input" 39594072)
