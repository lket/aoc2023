(ns aoc2023.day6
  (:require [clojure.string :as str])
  (:require [clojure.math :as math])
  (:require [clojure.java.io :as io])
  (:require [aoc2023.util :as u]))

(defn travel-factory [x]
  (let [max-travel (int (/ x 2))
        travel-by-press-time (fn [y] (- (* x y) (* y y)))]
    [max-travel travel-by-press-time]))

(defn parse [lines]
  (let [times (re-seq #"\d+" (first lines))
        distances (re-seq #"\d+" (second lines))
        zipped (map #(vector (u/str->int %1) (u/str->int %2)) times distances)]
    zipped))

(defn parse2 [lines]
  (let [times (re-seq #"\d+" (first lines))
        distances (re-seq #"\d+" (second lines))]
    [(u/str->long (apply str times)) (u/str->long (apply str distances))]))

(defn find-limit [f time space dir-f]
  (if (< space (f (dir-f time)))
    (recur f (dir-f time) space dir-f)
    time))

(defn find-wins [[time distance]]
  (let [[best solve] (travel-factory time)
        smallest (find-limit solve best distance dec)
        largest (find-limit solve best distance inc)]
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
