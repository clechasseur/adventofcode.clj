(ns io.github.clechasseur.adventofcode.util.time)

(defmacro time-it
  "A macro similar to Clojure's `time`, with pre- and postfix support.

  If `pre` and/or `pre-f` are provided, before running the task, a line will be
  printed to stdout to indicate that the task is about to run. If `pre` is provided,
  its value is printed. If `pre-f` is provided, it is called without argument and
  the returned value is printed.

  Then, the task is executed by calling `f` without argument as if via `time`.

  Finally, if `post` and/or `post-f` are provided, after running the task, a line
  will be printed to stdout to indicate that the task is done. If `post` is provided,
  its value will be printed. If `post-f` is provided, it is called with the task
  result as single argument and the returned value is printed.

  If `off` is logical true, everything is short-circuited - the task is simply
  executed by calling `f` without argument, and it is not timed. Useful to turn off
  timing temporarily while debugging."
  [f & {:keys [pre pre-f post post-f off]}]
  `(let [off# ~off
         sprint-f# (fn [s# s-f#]
                     (let [optional# (when s-f# (s-f#))
                           open-paren# (when (and s# optional#) " (")
                           close-paren# (when (and s# optional#) ")")]
                       (str s# open-paren# optional# close-paren#)))
         print-f# (fn [pre# s# s-f#]
                    (when (and (not off#) (or s# s-f#))
                      (println pre# (sprint-f# s# s-f#))))]
     (print-f# "Running:" ~pre ~pre-f)
     (let [res# (if off# ~f (time ~f))
           post-f# ~post-f]
       (print-f# "Done: " ~post (when post-f# (partial post-f# res#)))
       res#)))
