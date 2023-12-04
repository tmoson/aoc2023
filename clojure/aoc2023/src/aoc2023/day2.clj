(ns aoc2023.day2
  (:require [clojure.string :as string]
            [clojure.java.io :as io])
  (:import java.lang.Integer))

(def part-1-numbers {:red 12 :green 13 :blue 14})

(defn possible-set?
  [set nums]
  (if (seq set)
    (if (<= (second (first set)) ((first (first set)) nums))
      (recur (rest set) nums)
      false)
    true))

(defn create-set-color-map
  [set]
  (into {:red 0 :green 0 :blue 0}
        (map
         (fn [x] (let [split-string (string/split x #" ")]
                   {(keyword (second split-string)) (Integer/parseInt (first split-string))}))
         (string/split set #", "))))

(defn possible-replace-game?
  [line]
  (loop [sets (string/split line #"; ")]
    (if (seq sets)
      (if (possible-set? (create-set-color-map (first sets)) part-1-numbers)
        (recur (rest sets))
        false)
      true)))

(defn part-1
  [path]
  (with-open [reader (io/reader path)]
    (loop [line (.readLine reader)
           id-sum 0]
      (if (seq line)
        (let [id-and-sets (string/split line #": ")
              id (Integer/parseInt (second (string/split (first id-and-sets) #" ")))
              sets (second id-and-sets)]
          (if (possible-replace-game? sets)
            (recur (.readLine reader) (+ id-sum id))
            (recur (.readLine reader) id-sum)))
        id-sum))))

(defn max-rgb
  [mp1 mp2]
  (assoc {:red 0 :green 0 :blue 0}
         :red (:red (max-key :red mp1 mp2))
         :green (:green (max-key :green mp1 mp2))
         :blue (:blue (max-key :blue mp1 mp2))))

(defn minimum-cubes
  ([sets]
   (if (seq sets)
     (minimum-cubes sets {:red 0 :blue 0 :green 0})
     {:red 0 :blue 0 :green 0}))
  ([sets min-cubes]
   (if (seq sets)
     (recur (rest sets) (max-rgb (first sets) min-cubes))
     min-cubes)))

(defn part-2
  [path]
  (with-open [reader (io/reader path)]
    (loop [lines (map (fn [x] (second (string/split x #": "))) (line-seq reader))
           power-sum 0]
      (if (seq lines)
        (recur (rest lines)
               (+ power-sum (reduce * (vals (minimum-cubes (map create-set-color-map (string/split (first lines) #"; ")))))))
        power-sum))))
