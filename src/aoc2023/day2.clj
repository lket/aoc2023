(ns aoc2023.day2
  (:require [clojure.string :as str])
  (:require [clojure.java.io :as io])
  (:require [aoc2023.util :as u]))

(def limit {:red 12
            :green 13
            :blue 14})

(def cube-regex #"(\d+)|(red|green|blue)")

(defn simplify-cube [s]
  [(u/str->int (first s)) (keyword (second s))])

(defn simplify-game [game]
  (u/str->int (re-find #"\d+" game)))

(defn parse-game [line]
  (let [[game cubes] (str/split line #":")
        picks (str/split cubes #";")
        picks (map #(map simplify-cube (partition 2 (map first (re-seq cube-regex %)))) picks)]
    [(simplify-game game) picks]))

(defn legal-cube [[n color]]
  (<= n (get limit color)))

(defn legal-pick [cubes]
  (map legal-cube cubes))

(defn game-filter [[game picks]]
  (if (every? identity (flatten (map legal-pick picks)))
    game
    0))

(defn mapify-pick [pick]
  (apply hash-map (flatten (map reverse pick))))

(defn min-game [[game picks]]
  (let [g (fn [what where] (get where what 0))
        reqs (reduce (fn [val item]
                       {:red (max (g :red item) (g :red val))
                        :green (max (g :green item) (g :green val))
                        :blue (max (g :blue item) (g :blue val))})
                     {:red 0 :blue 0 :green 0} (map mapify-pick picks))
        power (* (:red reqs) (:green reqs) (:blue reqs))]
    power))

(defn part1 [filename]
  (->> filename
       u/load-input
       (map parse-game)
       (map game-filter)
       (apply +)))

(defn part2 [filename]
  (->> filename
       u/load-input
       (map parse-game)
       (map min-game)
       (apply +)))

(u/varmista part1 "day2_example" 8)
(u/varmista part1 "day2_input" 2006)
(u/varmista part2 "day2_example" 2286)
(u/varmista part2 "day2_input" 84911)
