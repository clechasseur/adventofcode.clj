(ns io.github.clechasseur.adventofcode.y2023.d10
  (:require [io.github.clechasseur.adventofcode.y2023.input.d10 :as d10-input]
            [io.github.clechasseur.adventofcode.util.pt :as pt]
            [io.github.clechasseur.adventofcode.util.grid :as grid]
            [io.github.clechasseur.adventofcode.util.dij :as dij]))

(def input (:map d10-input/input))
(def start-is (:start-is d10-input/input))

(defn grid-get
  [pt]
  (let [c (grid/get input pt)]
    (if (= c \S) start-is c)))

(def else (constantly true))
(def unreachable (constantly :unreachable))
(def flip {:out :in :in :out})
(def in-map
  {\F [[#{\-} {:out :down
               :in  :up}]
       [#{\|} {:out :right
               :in  :left}]
       [#{\J} flip]
       [#{\F} unreachable]
       [else  identity]]
   \7 [[#{\-} {:out :down
               :in  :up}]
       [#{\|} {:out :left
               :in  :right}]
       [#{\L} flip]
       [#{\7} unreachable]
       [else  identity]]
   \J [[#{\-} {:out :up
               :in  :down}]
       [#{\|} {:out :left
               :in  :right}]
       [#{\F} flip]
       [#{\J} unreachable]
       [else  identity]]
   \L [[#{\-} {:out :up
               :in  :down}]
       [#{\|} {:out :right
               :in  :left}]
       [#{\7} flip]
       [#{\L} unreachable]
       [else  identity]]
   \- [[#{\L \J} {:down :in
                  :up   :out}]
       [#{\F \7} {:down :out
                  :up   :in}]
       [#{\|}    unreachable]
       [else     identity]]
   \| [[#{\F \L} {:left  :in
                  :right :out}]
       [#{\7 \J} {:left  :out
                  :right :in}]
       [#{\-}    unreachable]
       [else     identity]]})

(defn get-cur-in
  [cur prev path]
  (let [prev-c (grid-get prev)
        cur-c (grid-get cur)
        [prev-in] (path prev)
        in-fn (->> (in-map prev-c)
                   (map (fn [[cur-fn in-fn]]
                          (when (cur-fn cur-c)
                            in-fn)))
                   (filter some?)
                   first)
        cur-in (in-fn prev-in)]
    (if (= cur-in :unreachable)
      (throw
        (AssertionError. (str "Moving from " prev " (" prev-c ", " prev-in ") to " cur " (" cur-c ") should not be possible")))
      cur-in)))

(def neighbours
  {\F [[ 1  0] [0  1]]
   \| [[ 0 -1] [0  1]]
   \L [[ 0 -1] [1  0]]
   \- [[-1  0] [1  0]]
   \J [[-1  0] [0 -1]]
   \7 [[-1  0] [0  1]]})

(def around
  {\F {:in [[-1  0] [0 -1]]}
   \L {:in [[-1  0] [0  1]]}
   \J {:in [[ 1  0] [0  1]]}
   \7 {:in [[ 0 -1] [1  0]]}
   \| {:left  [[-1 0]]
       :right [[ 1 0]]}
   \- {:up   [[0 -1]]
       :down [[0  1]]}})

(def displacements
  [       [0 -1]
   [-1 0]        [1 0]
          [0  1]      ])

(def get-path
  (memoize
    (fn []
      ;; I "cheated" to get these values: I built the path using Dijkstra and printed it out.
      (loop [path {[:REDACTED] [:right (grid-get [:REDACTED])]}
             cur [:REDACTED]
             prev [:REDACTED]]
        (if (path cur)
          path
          (let [cur-c (grid-get cur)
                cur-in (get-cur-in cur prev path)
                next (->> (neighbours cur-c)
                          (map (partial pt/+ cur))
                          (filter #(not (contains? path %)))
                          first)
                path (assoc path cur [cur-in cur-c])]
            (recur path next cur)))))))

(defn part-1
  []
  (let [path (get-path)]
    (quot (count path) 2)))

(defn part-2
  []
  (let [path (get-path)
        neighbours-fn (fn [pt]
                        (->> displacements
                             (map (partial pt/+ pt))
                             (filter #(some? (grid-get %)))
                             (filter #(not (contains? path %)))))
        ins (->> (mapcat (fn [[pt [in c]]]
                           (->> (get-in around [c in] [])
                                (map (partial pt/+ pt))))
                         path)
                 distinct
                 (filter #(not (contains? path %)))
                 (map #(dij/build % neighbours-fn))
                 (mapcat (fn [{dist :dist}] (keys dist)))
                 distinct)]
    (count ins)))
