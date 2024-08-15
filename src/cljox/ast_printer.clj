(ns cljox.ast-printer
  (:require [clojure.string :as str]))


(defmulti pprint :type)


(defn- parenthesize [name & exprs]
  (format "(%s %s)" name (str/join " " (map pprint exprs))))


(defmethod pprint :binary
  [{:keys [left right operator]}]
  (parenthesize (:lexeme operator) left right))

(defmethod pprint :unary
  [{:keys [right operator]}]
  (parenthesize (:lexeme operator) right))

(defmethod pprint :grouping
  [{:keys [expr]}]
  (parenthesize "group" expr))

(defmethod pprint :literal
  [{:keys [value]}]
  (if (nil? value)
    "nil"
    (str value)))

(defmethod pprint :print-stmt
  [{:keys [expr]}]
  (parenthesize "print" expr))

(defmethod pprint :expr-stmt
  [{:keys [expr]}]
  (pprint expr))


(defmethod pprint :var-decl
  [{:keys [token init]}]
  (format "var %s = %s" (:lexeme token) (pprint init)))

(defmethod pprint :variable
  [{:keys [token]}]
  (str "$" (:lexeme token)))

(defmethod pprint nil
  [_]
  "nil")

(defmethod pprint :assignment
  [{:keys [token value]}]
  (format "%s = %s" (:lexeme token) (pprint value)))

(defmethod pprint :block
  [{:keys [stmts]}]
  (str "{" (str/join "; " (map pprint stmts)) "}"))


(defmethod pprint :if-stmt
  [{:keys [cnd then else]}]
  (format "(if (%s) then %s else %s)"
          (pprint cnd) (pprint then) (pprint else)))

(defmethod pprint :logical
  [{:keys [left operator right]}]
  (parenthesize (:lexeme operator) left right))

(defmethod pprint :while-stmt
  [{:keys [cnd body]}]
  (format "(while (%s) do %s)"
          (pprint cnd) (pprint body)))

(defmethod pprint :call
  [{:keys [calee args]}]
  (format "(calling %s [%s])"
          (pprint calee) (str/join ", " (map pprint args))))

(defmethod pprint :func-decl
  [{:keys [token params body]}]
  (format "fun %s (%s) {%s}"
          (:lexeme token)
          (str/join ", " (map pprint params))
          (str/join "; " (map pprint body))))

(defmethod pprint :identifier
  [token]
  (:lexeme token))




(comment
 (require '[cljox.ast :as ast])
 (require '[cljox.scanner :as scanner])

 (def example
   (ast/binary
    (ast/unary (scanner/create-token :minus "-")
               (ast/literal 123))
    (scanner/create-token :star "*")
    (ast/grouping (ast/literal 45.67))))

 (pprint example))
