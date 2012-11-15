(ns game-of-life-clj.core
  (:require [clojure.set]))

(defn inclusive-neighborhood [element]
  (let [y (first element)
        x (second element)]
    (for [y (range (dec y) (+ 2 y))
          x (range (dec x) (+ 2 x))]
      [y x])))

(defn neighbors [element]
  (clojure.set/difference
    (set (inclusive-neighborhood element))
    #{element}))

(defn occupied? [world element]
  (contains? world element))

(defn has-neighbor-count [neighbor-counts world element]
  (let [neighbors (neighbors element)
        occupied-neighbors (filter #(occupied? world %) neighbors)]
    (contains? neighbor-counts (count occupied-neighbors))))

(defn survives? [element world]
  (has-neighbor-count #{2 3} world element))

(defn survivors [world]
  (set (filter #(survives? % world) world)))

(defn birth-candidates [world]
  (apply clojure.set/union (map neighbors world)))

(defn births [world]
  (set (filter #(has-neighbor-count #{3} world %) (birth-candidates world))))

(defn update [world]
  (clojure.set/union
    (survivors world)
    (births world)))

(defn character-for [element world]
  (if (occupied? world element)
    "X"
    "_"))

(defn row-string [world y xs]
  (let [characters (for [x xs]
                     (character-for [y x] world))]
        (clojure.string/join " " characters)))

(defn world-string [world ys xs]
  (let [rows (for [y ys]
               (row-string world y xs))]
    (clojure.string/join "\n" rows)))

(defn interesting?
  ([world]
    (interesting? world 1000))
  ([world iterations]
   (let [worlds (iterate update world)
         world-after-iterations (nth worlds iterations)
         half-initial-count (/ (count world) 2)]
     (and
       (>= (count world-after-iterations) half-initial-count)
       (> 0 (count world-after-iterations))))))

(defn fun-times
  ([world]
    (fun-times world 4))
  ([world iterations]
   (let [worlds (iterate update world)]
     (dotimes [n iterations]
       (println (world-string (nth worlds n) (range 10) (range 10)))
       (println)))))

(defn sample [n elements]
  (loop [iters n
         current #{}]
    (if (zero? iters)
      current
      (recur (dec iters) (clojure.set/union current #{(rand-nth elements)})))))

(defn points-on-grid [grid-length]
  (for [y (range grid-length)
        x (range grid-length)]
    [y x]))

(defn random-sample-from-grid [grid-length num-points]
  (let [sampling-points (points-on-grid grid-length)]
    (sample num-points sampling-points)))

(defn print-interesting-patterns [max-cells]
  (dotimes [n 100000]
    (let [grid-length 20
          points (random-sample-from-grid grid-length (rand-int max-cells))
          interesting? (interesting? points)]
      (if interesting?
        (do
          (println points)
          (println (world-string points (range grid-length) (range grid-length))))))))
