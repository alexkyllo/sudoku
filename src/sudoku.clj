(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def example-board 
       [[5 3 0 0 7 0 0 0 0]
        [6 0 0 1 9 5 0 0 0]
        [0 9 8 0 0 0 0 6 0]
        [8 0 0 0 6 0 0 0 3]
        [4 0 0 8 0 3 0 0 1]
        [7 0 0 0 2 0 0 0 6]
        [0 6 0 0 0 0 2 8 0]
        [0 0 0 4 1 9 0 0 5]
        [0 0 0 0 8 0 0 7 9]])

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (second coord))) 
            board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn block-values [board coord]
  (let [min-block (fn [x] (cond 
                            (< x 3) 0
                            (< x 6) 3
                            :else 6))
        upper-left (fn [x] (map min-block coord))]
    (set (for [xrange (range 
                  (first (upper-left coord)) 
                  (+ (first (upper-left coord)) 3))
               yrange (range 
                  (second (upper-left coord)) 
                  (+ (second (upper-left coord)) 3))]
      (get-in board [xrange yrange])))))

(defn valid-values-for [board coord]
  (if-not (zero? (value-at board coord)) #{}
          (set/difference all-values 
                          (block-values board coord)
                          (row-values board coord)
                          (col-values board coord))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-set? [xs]
  (= xs #{1 2 3 4 5 6 7 8 9}))

(defn valid-rows? [board]
  (every? valid-set? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? valid-set? (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (set (block-values board [x y]))))

(defn valid-blocks? [board]
  (every? valid-set? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) 
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [row [0 1 2 3 4 5 6 7 8]
                     col [0 1 2 3 4 5 6 7 8]]
                 [row col])]
   (if (= 0 (value-at board [coord])))))

(defn solve [board]
  nil)
