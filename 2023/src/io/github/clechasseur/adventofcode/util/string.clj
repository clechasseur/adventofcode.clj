(ns io.github.clechasseur.adventofcode.util.string)

(defn safe-subs
  "Safe version of `subs`. If start or end are out of bounds, they will be constrained automatically."
  ([s start]
   (safe-subs s start (count s)))
  ([s start end]
   (let [len (count s)
         minmax (fn [i] (min (max i 0) len))
         start (minmax start)
         end (minmax end)]
     (subs s start end))))
