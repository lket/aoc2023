(ns aoc2023.day3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc2023.util :as u]))

(def dirs [[-1 -1] [0 -1] [1 -1]
           [-1 0]         [1 0]
           [-1 1]  [0 1]  [1 1]])

(defn collect-parts-from-row [parts y row]
  (:parts (reduce (fn [{x :x parts :parts} symbol]
                    (cond
                      (= symbol \.) {:x (inc x) :parts parts}
                      (Character/isDigit symbol) {:x (inc x) :parts parts}
                      true {:x (inc x) :parts (conj parts {:x x :y y :symbol symbol})}))
                  {:x 0 :parts parts} row)))

(defn collect-parts [grid]
  [grid (:parts (reduce (fn [{y :y parts :parts} row]
                          {:y (inc y) :parts (collect-parts-from-row parts y row)})
                        {:y 0 :parts []} grid))])

(defn find-in-dir [grid dir x y num]
  (let [row (get grid y)
        next (get row (+ x dir) \.)]
    (if (Character/isDigit next)
      (recur (assoc-in grid [y (+ x dir)] \.)
             dir (+ dir x) y
             (if (neg-int? dir) (str next num) (str num next)))
      [num grid])))

(defn find-adjacents [grid [x y] number]
  (let [grid (assoc-in grid [y x] \.)
        [back-find grid] (find-in-dir grid -1 x y (str number))
        [forward-find grid] (find-in-dir grid 1 x y "")]
    [grid (u/str->int (str back-find forward-find))]))

(defn find-machines-for-part [grid part]
  (reduce (fn [{x :x y :y symbol :symbol grid :grid parts :parts} [dx dy]]
            (let [gg (fn [[x y] where] (get (get where y []) x \.))
                  target_coords [(+ x dx) (+ y dy)]
                  target (gg target_coords grid)
                  [grid part-number] (if (Character/isDigit target)
                                       (find-adjacents grid target_coords target)
                                       [grid nil])]
              {:x x :y y :symbol symbol :grid grid
               :parts (if (nil? part-number)
                        parts
                        (conj parts part-number))}))
          (assoc part :grid grid) dirs))

(defn find-machines [[grid parts]]
  (map #(find-machines-for-part grid %) parts))

(defn part1 [filename]
  (->> filename u/load-input
       (mapv vec)
       collect-parts
       find-machines
       (map :parts)
       flatten
       (apply +)))

(defn part2 [filename]
  (->> filename u/load-input
       (mapv vec)
       collect-parts
       find-machines
       (filter #(= (:symbol %) \*))
       (filter #(= (count (:parts %)) 2))
       (map :parts)
       (map #(apply * %))
       (apply +)))

(u/varmista part1 "day3_example" 4361)
(u/varmista part1 "day3_input" 520135)
(u/varmista part2 "day3_example" 467835)
(u/varmista part2 "day3_input" 72514855)
