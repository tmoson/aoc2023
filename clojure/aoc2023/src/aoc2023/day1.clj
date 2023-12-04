(ns aoc2023.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import java.lang.Integer))

(defn get-digit-from-line
  "Returns a 2 digit number made of the first and last numbers in a string"
  [line]
  (loop [chars (seq line)
         first-num nil
         last-num nil]
    (if (seq chars)
      (let [current (int (first chars))]
        (if (and (< current 58) (> current 47))
          (if (nil? first-num)
            (recur (rest chars) (first chars) (first chars))
            (recur (rest chars) first-num (first chars)))
          (recur (rest chars) first-num last-num)))
      (if (nil? first-num)
        0
        (Integer/parseInt (str first-num last-num))))))

(def numbers {:zero "0" :one "1" :two "2" :three "3" :four "4" :five "5" :six "6" :seven "7" :eight "8" :nine "9"})

(defn search-string-for-number
  [input]
  (loop [start 0
         end 2
         out-of-bounds (count input)]
    (if (< end out-of-bounds)
      (if-let [number ((keyword (subs input start end)) numbers)]
        number
        (if (= (- end start) 5)
          (recur (inc start) (- end 2) out-of-bounds)
          (recur start (inc end) out-of-bounds)))
      (if (< (- end start) 3)
        nil
        (if-let [number ((keyword (subs input start end)) numbers)]
          number
          (recur (inc start) end out-of-bounds))))))

(defn get-digit-from-line-spelled
  "Returns a 2 digit number made of the first and last numbers in a string, including spelled numbers"
  [line]
  (loop [start 0
         end 1
         current (int (nth line 0))
         out-of-bounds (count line)
         first-num nil
         last-num nil]
    (if (< end out-of-bounds)
      (if (and (< current 58) (> current 47))
        (if (nil? first-num)
          (if (= end (dec out-of-bounds))
            (recur end (inc end) (int (last line)) out-of-bounds (char current) (char current))
            (recur end (inc end) (int (nth line end)) out-of-bounds (char current) (char current)))
          (if (= end (dec out-of-bounds))
            (recur end (inc end) (int (last line)) out-of-bounds first-num (char current))
            (recur end (inc end) (int (nth line end)) out-of-bounds first-num (char current))))
        (if-let [current-spelled (search-string-for-number (subs line start end))]
          (if (nil? first-num)
            (recur (dec end) end current out-of-bounds current-spelled current-spelled)
            (recur (dec end) end current out-of-bounds first-num current-spelled))
          (if (= end (dec out-of-bounds))
            (recur start (inc end) (int (last line)) out-of-bounds first-num last-num)
            (recur start (inc end) (int (nth line end)) out-of-bounds first-num last-num))))
      (if (and (< current 58) (> current 47))
        (if (nil? first-num)
          (Integer/parseInt (str (last line) (last line)))
          (Integer/parseInt (str first-num (last line))))
        (if-let [last-spelled (search-string-for-number (subs line start))]
          (if (nil? first-num)
            (Integer/parseInt (str last-spelled last-spelled))
            (Integer/parseInt (str first-num last-spelled)))
          (if (nil? first-num)
            0
            (Integer/parseInt (str first-num last-num))))))))

  (defn get-calibration-number
    "Gets the calibration number from a file"
    [path]
    (with-open [reader (io/reader path)]
      (loop [line (.readLine reader)
             calibration-number 0]
        (if (seq line)
          (recur (.readLine reader) (+ calibration-number (get-digit-from-line line)))
          calibration-number))))

  (defn get-calibration-number-spelled
    "Gets the calibration number from a file including spelled digits"
    [path]
    (with-open [reader (io/reader path)]
      (loop [line (.readLine reader)
             calibration-number 0]
        (if (seq line)
          (recur (.readLine reader) (+ calibration-number (get-digit-from-line-spelled line)))
          calibration-number))))
  