(ns game-of-life-clj.core-spec
  (:use [speclj.core]
        [game-of-life-clj.core]))

(describe "board update"
  (it "kills one cell"
    (should= #{} (update #{[1 1]})))
  (it "keeps middle cell alive horizontally"
    (let [start-world #{[5 5] [5 6] [5 7]}]
      (should (contains? (update start-world) [5 6]))))
  (it "keeps middle cell alive vertically"
    (let [start-world #{[4 5] [5 5] [6 5]}]
      (should (contains? (update start-world) [5 5]))))
  (it "keeps middle cell alive with 3 neighbors"
    (let [start-world #{[4 5] [5 5] [6 5] [5 6]}]
      (should (contains? (update start-world) [5 5]))))
  (it "births a cell with 3 neighbors"
    (let [start-world #{[4 5] [6 5] [5 6]}]
      (should (contains? (update start-world) [5 5]))))
  (it "births a cell with 3 neighbors"
    (let [start-world #{[4 5] [5 5] [6 5]}]
      (should (contains? (update start-world) [5 6]))
      (should (contains? (update start-world) [5 4]))))
)

(describe "board-print"
  (it "prints a board with dimension parameters"
    (let [expected-rows ["_ _ _ _ _"
                         "_ _ _ _ _"
                         "_ _ _ _ _"
                         "_ _ _ _ _"
                         "_ _ _ _ _"]
          expected-string (clojure.string/join "\n" expected-rows)]
    (should= expected-string (world-string #{} (range 0 5) (range 5 10)))))
  (it "prints a board with dimension parameters"
    (let [expected-rows ["_ _ _ _ _"
                         "_ _ _ _ _"
                         "_ _ X _ _"
                         "_ _ _ _ _"
                         "_ _ _ _ _"]
          expected-string (clojure.string/join "\n" expected-rows)]
    (should= expected-string (world-string #{[2 2]} (range 0 5) (range 0 5))))))

(describe "fun"
  (it "blinker"
    (fun-times #{[2 2] [2 3] [2 4]}))
  (it "toad"
    (fun-times #{[4 2] [4 3] [4 4] [3 3] [3 4] [3 5]}))
  (it "ship"
    (fun-times #{[3 5] [4 5] [5 5] [5 4] [4 3]} 15))
      )
