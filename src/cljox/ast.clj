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

(defn call [calee paren arguments]
  {:type :call
   :calee calee
   :paren paren
   :args arguments})

(defn class-stmt [token superclass methods]
  {:type :class-stmt
   :token token
   :super superclass
   :methods methods})

(defn expr-stmt [expr]
  {:type :expr-stmt
   :expr expr})

(defn func-decl [token params body]
  {:type :func-decl
   :token token
   :params params
   :body body})

(defn get-expr [object token]
  {:type :get-expr
   :object object
   :token token})

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

(defn logical [left operator right]
  {:type :logical
   :left left
   :operator operator
   :right right})

(defn print-stmt [expr]
  {:type :print-stmt
   :expr expr})

(defn return-stmt [kword value]
  {:type :return-stmt
   :kword kword
   :value value})

(defn set-expr [object token value]
  {:type :set-expr
   :object object
   :token token
   :value value})

(defn super [kword method]
  {:type :super
   :kword kword
   :method method})

(defn this [kword]
  {:type :this
   :kword kword})

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

(defn while-stmt [condition body]
  {:type :while-stmt
   :cnd condition
   :body body})
