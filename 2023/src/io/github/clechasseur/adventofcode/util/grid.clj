(ns io.github.clechasseur.adventofcode.util.grid
  (:refer-clojure :exclude [get]))

(defn get
  "Gets a value in a grid. Returns nil if any dimension is out of bounds."
  [grid pt]
  {:pre [(or (empty? pt) (seqable? grid))]}
  (if (empty? pt)
    grid
    (when-let [grid (clojure.core/get grid (last pt))]
      (recur grid (drop-last pt)))))

(defn locate
  "Locates all points in a grid for which `(pred (get pt))` returns true."
  ([pred grid]
   (locate '() pred grid))
  ([pt pred grid]
   (cond
     (seqable? grid) (->> (map-indexed vector grid)
                          (mapcat (fn [[i grid]] (locate (conj pt i) pred grid)))
                          (map (partial apply vector)))
     (pred grid)     (list pt)
     :else           '())))

(defn locate-one
  "Locates the single point in a grid for which `(pred (get pt))` returns true.

  If no point is found, returns nil. If more than one point is found, an assertion is raised."
  [pred grid]
  (let [pts (seq (locate pred grid))]
    (assert (<= (count pts) 1) (str "Expected to locate at most one point, found " (count pts) ": " pts))
    (first pts)))

(defn dims
  "Returns the number of dimensions in the grid."
  [grid]
  (loop [grid grid
         prev-dims 0]
    (if-not (seqable? grid)
      prev-dims
      (recur (first grid) (inc prev-dims)))))

(defn transpose
  "Transposes a 2-dimensional matrix."
  [grid]
  {:pre [(= (dims grid) 2)]}
  (apply mapv vector grid))
