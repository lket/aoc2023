(ns aoc2023.day4
  (:require [clojure.string :as str])
  (:require [clojure.math :as math])
  (:require [clojure.java.io :as io])
  (:require [aoc2023.util :as u]))

(defn parse-row [row]
  (let [card (u/str->int (re-find #"\d+" row))
        [w my] (map #(map u/str->int (re-seq #"\d+" %)) (str/split (second (str/split row #":")) #"\|"))]
    [card w my]))

(defn mapify-rows [rows]
  (reduce (fn [result [card w my]]
            (assoc result card (int (math/floor (count (find-common [card w my]))))))
          {} rows))

(defn find-common [[card w my]]
  (clojure.set/intersection (set w) (set my)))

(defn winnings [common]
  (int (math/floor (math/pow 2 (dec (count common))))))

(defn get-cards [current wins]
  (range (inc current) (+ 1 current wins)))

(defn rec-winnings [cards card-n]
  (let [wins (get cards card-n)
        won-cards (map #(rec-winnings cards %) (get-cards card-n wins))]
    (inc (apply + won-cards))))

(defn all-winnings [cards]
  (let [calc-cards (map #(rec-winnings cards %) (keys cards))]
    (apply + calc-cards)))

(defn part1 [filename]
  (->> filename u/load-input
       (map parse-row)
       (map find-common)
       (map winnings)
       (apply +)))

(defn part2 [filename]
  (->> filename u/load-input
       (map parse-row)
       mapify-rows
       all-winnings))

(u/varmista part1 "day4_example" 13)
(u/varmista part1 "day4_input" 21088)
(u/varmista part2 "day4_example2" 30)
(u/varmista part2 "day4_input" 6874754)
