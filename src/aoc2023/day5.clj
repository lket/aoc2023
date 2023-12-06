(ns aoc2023.day5
  (:require [clojure.string :as str]
            [clojure.math :as math]
            [clojure.java.io :as io]
            [aoc2023.util :as u]))

;; TODO: make an actual solution for part 2

(defn convert [from val almanac]
  (let [value (get almanac from)
        conversion (loop [stack (:maps value)]
                     (let [[dest src width] (first stack)
                           [_ next _] (first (rest stack))]
                       (if (and (<= src val) (or (nil? next) (< val next)))
                         (let [diff (- val src)
                               valid (<= diff width)]
                           (if valid (+ dest diff) val))
                         (if (empty? (rest stack)) val (recur (rest stack)))
                         )))]
    [conversion (:to value)]))

(defn parse-map [to-map]
  (let [rows (str/split-lines to-map)
        [_ from to] (re-find #"(\w+)-to-(\w+) map:" (first rows))
        maps (map (fn [line]
                    (let [numbers (re-seq #"\d+" line)]
                      (mapv u/str->long numbers)))
                  (rest rows))
        maps (sort-by second maps)]
    [(keyword from) (keyword to) maps]))

(defn parse [input]
  (let [rows (str/split input #"\n\n")
        [seeds maps] [(first rows) (rest rows)]
        seeds (map u/str->long (re-seq #"\d+" seeds))
        maps (map parse-map maps)]
    [seeds (reduce (fn [coll [from to vals]] (assoc coll from {:from from :to to :maps vals})) {} maps)])
  )

(defn solve-id [what id almanac]
  (let [[result to] (convert what id almanac)]
    (if (= to :location)
      result
      (recur to result almanac))))

(defn solve1 [[seeds almanac]]
  (map #(solve-id :seed % almanac) seeds))

(defn solve2
  "skips around the range, trying to find a likely candidate for solution area"
  [[seeds almanac]]
  (let [pairs (mapv vec (partition 2 seeds))
        ranges (mapv (fn [[from width]]
                       (range from (+ from (inc width)) 10000)) pairs)
        seed-range (flatten ranges)]
    (map #(vector % (solve-id :seed % almanac)) (flatten ranges))))

(defn solve2-manual
  "instead of solving the complete range, search around a possible solution"
  [start [seeds almanac]]
  (let [seed-range (range (- start 20000) (+ start 20000))]
    (map #(solve-id :seed % almanac) seed-range)))

(defn part1 [filename]
  (->> filename
       io/resource
       slurp
       parse
       solve1
       (apply min)))

(defn part2 [filename]
  (->> filename
       io/resource
       slurp
       parse
       solve2
       (sort-by second)
       first))

(defn part2-manual [filename guess]
  (->> filename
       io/resource
       slurp
       parse
       (solve2-manual guess)
       (apply min)))

(part1 "day5_example")
(part1 "day5_input")
(part2 "day5_example")

(part2 "day5_input")
; => [3876944151 56941461]
(part2-manual "day5_input" 3876944151)
