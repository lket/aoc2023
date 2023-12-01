(ns aoc2023.day1
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io]))

(def words {
            "one" 1
            "two" 2
            "three" 3
            "four" 4
            "five" 5
            "six" 6
            "seven" 7
            "eight" 8
            "nine" 9})

(defn str->int [str] (Integer/parseInt str))
(defn char->int [char] (Character/digit char 10))

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

(defn parse
  [filename]
  (-> filename
       io/resource
       slurp
       (str/split #"\n")))

(defn part1 [filename]
  (->> filename
       parse
       (map #(map char->int %))
       (map #(filter pos-int? %))
       (map #(str (first %) (last %)))
       (map str->int)
       (apply +)))

(defn part2 [filename]
  (->> filename
       parse
       (map replace-words)
       (map #(map char->int %))
       (map #(filter pos-int? %))
       (map #(str (first %) (last %)))
       (map str->int)
       (apply +)))

(part1 "day1_input")
(part2 "day1_input")
(part2 "day1_example")
(part2 "day1_example2")
