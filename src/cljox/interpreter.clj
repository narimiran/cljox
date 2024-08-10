(ns cljox.interpreter
  (:require [cljox.error :as err]
            [cljox.environment :as env]))


(defmulti evaluate
  (fn [_ m] (:type m)))


(defn- throw-error [token msg]
  (throw (ex-info "runtime error"
                  (err/runtime-error token msg))))

(defn- declare-var [state name value]
  (env/declare-name! (:env state) name value)
  state)

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
  [state {:keys [left right operator]}]
  (let [state'  (evaluate state left)
        l       (:result state')
        state'' (evaluate state' right)
        r       (:result state'')
        op-type (:type operator)
        op-fn   (op-type operations)]
    (assoc state'' :result
           (case op-type
             :bang-equal  (not= l r)
             :equal-equal (= l r)
             :plus        (plus operator l r)
             (:greater :greater-equal :less :less-equal :minus :slash :star)
             (numeric-op operator op-fn l r)))))

(defmethod evaluate :expr-stmt
  [state expr]
  (evaluate state (:expr expr)))

(defmethod evaluate :grouping
  [state expr]
  (evaluate state (:expr expr)))

(defmethod evaluate :literal
  [state expr]
  (assoc state :result (:value expr)))

(defmethod evaluate :print-stmt
  [state expr]
  (let [state' (evaluate state (:expr expr))]
    (println (:result state'))
    (assoc state' :result nil)))

(defmethod evaluate :var-decl
  [state {:keys [token init]}]
  (let [state' (if init
                 (evaluate state init)
                 (assoc state :result nil))]
    (-> state'
        (declare-var (:lexeme token) (:result state'))
        (assoc :result nil))))

(defmethod evaluate :variable
  [state {:keys [token]}]
  (assoc state :result (env/get-var (:env state) token)))



(defn- truthy? [v]
  (cond
    (nil? v)   false
    (false? v) false
    :else      true))

(defmethod evaluate :unary
  [state {:keys [operator right]}]
  (let [state' (evaluate state right)
        value  (:result state')]
    (assoc state' :result
           (case (:type operator)
             :minus (numeric-op operator - value)
             :bang  (not (truthy? value))))))


(defn- new-state []
  {:env    (atom {})
   :result nil
   :errors []})


(defn interpret [stmts]
  (loop [[hd & tl] stmts
         state (new-state)]
    (if (nil? hd)
      (:result state)
      (recur
       tl
       (try
         (evaluate state hd)
         (catch clojure.lang.ExceptionInfo e
           (err/print-errors [(ex-data e)])
           (-> state
               (update :errors conj (ex-data e))
               (assoc :result nil))))))))
