(ns cljox.ast)


(defn binary [left operator right]
  {:type :binary
   :left left
   :operator operator
   :right right})

(defn expr-stmt [expr]
  {:type :expr-stmt
   :expr expr})

(defn grouping [expr]
  {:type :grouping
   :expr expr})

(defn literal [value]
  {:type :literal
   :value value})

(defn print-stmt [expr]
  {:type :print-stmt
   :expr expr})

(defn unary [operator right]
  {:type :unary
   :operator operator
   :right right})
