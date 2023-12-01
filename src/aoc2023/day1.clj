(ns aoc2023.day1
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [aoc2023.util :as u]))

(def words {"one" 1
            "two" 2
            "three" 3
            "four" 4
            "five" 5
            "six" 6
            "seven" 7
            "eight" 8
            "nine" 9})

(defn replace-first-word [line]
  (str/replace-first line #"(?<!\d\S*)(one|two|three|four|five|six|seven|eight|nine)"
                     (fn [[_ a]] (str (get words a)))))

(defn replace-last-word [line]
  ;; :DDDD
  (str/reverse (str/replace-first (str/reverse line) #"(eno|owt|eerht|ruof|evif|xis|neves|thgie|enin)"
                                  (fn [[_ a]] (str (get words (str/reverse a)))))))

(defn replace-words [line]
  (-> line
      replace-first-word
      replace-last-word))

(defn part1 [filename]
  (->> filename
       u/load-input
       (map #(map u/char->int %))
       (map #(filter pos-int? %))
       (map #(str (first %) (last %)))
       (map u/str->int)
       (apply +)))

(defn part2 [filename]
  (->> filename
       u/load-input
       (map replace-words)
       (map #(map u/char->int %))
       (map #(filter pos-int? %))
       (map #(str (first %) (last %)))
       (map u/str->int)
       (apply +)))

(u/varmista part1 "day1_example" 142)
(u/varmista part2 "day1_example2" 281)
(u/varmista part1 "day1_input" 52974)
(u/varmista part2 "day1_input" 53340)
