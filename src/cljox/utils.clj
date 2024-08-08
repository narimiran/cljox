(ns cljox.utils)


(defn at-end? [{:keys [current length]}]
  (>= current length))

(defn in-progress? [m]
  (not (at-end? m)))

(defn advance [m]
  (update m :current inc))

(defn current-element [{:keys [source current]}]
  (nth source current))
