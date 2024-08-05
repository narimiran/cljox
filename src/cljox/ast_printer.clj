(ns cljox.ast-printer
  (:require [clojure.string :as str]))


(defmulti pprint :type)


(defn- parenthesize [name & exprs]
  (str "("
       name
       (when exprs " ")
       (str/join " " (map pprint exprs))
       ")"))


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
    value))








(comment
 (require '[cljox.token :as token])
 (require '[cljox.ast :as ast])

 (def example
   (ast/binary
    (ast/unary (token/create-token :minus "-")
               (ast/literal 123))
    (token/create-token :star "*")
    (ast/grouping (ast/literal 45.67))))

 (pprint example))
