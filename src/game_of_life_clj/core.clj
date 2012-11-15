(ns game-of-life-clj.core)

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

(defn fun-times
  ([first-world]
    (fun-times first-world 5))
  ([first-world iterations]
    (loop [world first-world
           remaining-iterations iterations]
      (if (zero? remaining-iterations)
        nil
        (do
          (println (world-string world (range 10) (range 10)))
          (println)
          (recur (update world) (dec remaining-iterations)))))))

