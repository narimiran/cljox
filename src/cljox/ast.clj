(ns cljox.ast)


(defn assignment [token value]
  {:type :assignment
   :token token
   :value value})

(defn binary [left operator right]
  {:type :binary
   :left left
   :operator operator
   :right right})

(defn block [stmts]
  {:type :block
   :stmts stmts})

(defn expr-stmt [expr]
  {:type :expr-stmt
   :expr expr})

(defn grouping [expr]
  {:type :grouping
   :expr expr})

(defn if-stmt [condition then-branch else-branch]
  {:type :if-stmt
   :cnd condition
   :then then-branch
   :else else-branch})

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

(defn var-decl [token initializer]
  {:type :var-decl
   :token token
   :init initializer})

(defn variable [token]
  {:type :variable
   :token token})
