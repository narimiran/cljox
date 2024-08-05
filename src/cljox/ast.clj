(ns cljox.ast)


(defn binary [left operator right]
  {:type :binary
   :left left
   :operator operator
   :right right})

(defn grouping [expr]
  {:type :grouping
   :expr expr})

(defn literal [value]
  {:type :literal
   :value value})

(defn unary [operator right]
  {:type :unary
   :operator operator
   :right right})
