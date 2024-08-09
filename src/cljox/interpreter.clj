(ns cljox.interpreter
  (:require [cljox.error :as err]))


(defmulti evaluate :type)


(defn- throw-error [token msg]
  (throw (ex-info "runtime error"
                  (err/runtime-error token msg))))


(defn- plus [token l r]
  (cond
    (and (instance? Double l)
         (instance? Double r))
    (+ l r)

    (and (instance? String l)
         (instance? String r))
    (str l r)

    :else
    (throw-error token "operands must be either numbers or strings")))

(defn- numeric-op
  ([token f r]
   (if (instance? Double r)
     (f r)
     (throw-error token "operand must be a number")))
  ([token f l r]
   (if (and (instance? Double l)
            (instance? Double r))
     (f l r)
     (throw-error token "both operands must be numbers"))))

(def operations
  {:greater       >
   :greater-equal >=
   :less          <
   :less-equal    <=
   :minus         -
   :slash         /
   :star          *})

(defmethod evaluate :binary
  [{:keys [left right operator]}]
  (let [l       (evaluate left)
        r       (evaluate right)
        op-type (:type operator)
        op-fn   (op-type operations)]
    (case op-type
      :bang-equal  (not= l r)
      :equal-equal (= l r)
      :plus        (plus operator l r)
      (:greater :greater-equal :less :less-equal :minus :slash :star)
      (numeric-op operator op-fn l r))))

(defmethod evaluate :expr-stmt
  [expr]
  (evaluate (:expr expr)))

(defmethod evaluate :grouping
  [expr]
  (evaluate (:expr expr)))

(defmethod evaluate :literal
  [expr]
  (:value expr))

(defmethod evaluate :print-stmt
  [expr]
  (println (evaluate (:expr expr))))


(defn- truthy? [v]
  (cond
    (nil? v)   false
    (false? v) false
    :else      true))

(defmethod evaluate :unary
  [{:keys [operator right]}]
  (let [value (evaluate right)]
    (case (:type operator)
      :minus (numeric-op operator - value)
      :bang  (not (truthy? value)))))


(defn interpret [stmts]
  (doseq [stmt stmts]
    (try
      (evaluate stmt)
      (catch clojure.lang.ExceptionInfo e
        (err/print-errors (ex-data e))))))
