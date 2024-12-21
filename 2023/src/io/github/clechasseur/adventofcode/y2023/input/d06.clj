(ns io.github.clechasseur.adventofcode.y2023.input.d06)

(def input
  (->> {:races [{:time 0 :distance 0} ;; <REDACTED>
                ]}
       ((fn [input]
          (let [big-time (bigint (apply str (map :time (:races input))))
                big-distance (bigint (apply str (map :distance (:races input))))]
            (assoc input :big-race {:time big-time :distance big-distance}))))))
