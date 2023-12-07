(ns aoc2023.day7
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math :as math]
            [clojure.java.io :as io]
            [aoc2023.util :as u]))

(def joker-value 11)

(defn compare-hand-values [h1 h2]
  (cond
    (and (empty? h1) (not-empty h2)) -1
    (and (not-empty h1) (empty? h2)) 1
    (and (empty? h1) (empty? h2)) 0
    (and (not-empty h1) (not-empty h2))
    (let [comparison (compare-cards (first h1) (first h2))]
      (if (zero? comparison)
        (recur (rest h1) (rest h2))
        comparison))))

(defn compare-hands [{type-value1 :type-value hand1 :hand} {type-value2 :type-value hand2 :hand}]
  (let [comparison (compare type-value1 type-value2)]
    (if (zero? comparison)
      (compare-hand-values hand1 hand2)
      comparison)))

(defn get-value [c]
  (if (Character/isDigit c)
    (u/char->int c)
    (case c
      \A 14
      \K 13
      \Q 12
      \J joker-value
      \T 10)))

(defn compare-cards [c1 c2]
  (let [v1 (get-value c1)
        v2 (get-value c2)]
    (compare v1 v2)))

(defn count-cards [hand]
  (reduce (fn [counts card]
            (update counts card (fnil inc 0)))
          {} hand))

(defn name-hand [counts]
  (let [sorted (vec (sort #(compare %2 %1) (vals counts)))]
    (match sorted
      [5] [7 :five-of-kind]
      [4 _] [6 :four-of-kind]
      [3 2] [5 :full-house]
      [3 _ _] [4 :three-of-kind]
      [2 2 _] [3 :two-pair]
      [2 _ _ _] [2 :pair]
      :else [1 :high])))

(defn jokerify [counts]
  (let [jokers (get counts \J 0)
        jokerless (dissoc counts \J)
        [most-key most] (last (sort-by second jokerless))]
    (if (or (= jokers 5) (nil? most-key)) counts
        (assoc jokerless most-key (+ most jokers)))))

(defn parse-row [jokerify? row]
  (let [[hand bid] (str/split row #" ")
        counts (count-cards hand)
        counts-j (jokerify counts)
        [type-value name] (name-hand (if jokerify? counts-j counts))
        bid (u/str->int bid)]
    {:hand hand :type-value type-value :hand-name name :bid bid}))

(defn hand-value [hand]
  (* (:rank hand) (:bid hand)))

(defn part1 [filename]
  (->> filename u/load-input
       (map (partial parse-row false))
       (sort compare-hands)
       (map-indexed (fn [i hand] (assoc hand :rank (inc i))))
       (map hand-value)
       (apply +)))

(defn part2 [filename]
  (->> filename u/load-input
       (map (partial parse-row true))
       (sort compare-hands)
       (map-indexed (fn [i hand] (assoc hand :rank (inc i))))
       (map hand-value)
       (apply +)))

(u/varmista part1 "day7_example" 6440)
(u/varmista part1 "day7_input" 253313241)

(def joker-value 0) ; sorry....
(u/varmista part2 "day7_example" 5905)
(u/varmista part2 "day7_input" 253362743)
