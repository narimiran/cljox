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

(defn- assign-var [state token value]
  (env/assign! (:env state) token value)
  state)

(defn- truthy? [v]
  (cond
    (nil? v)   false
    (false? v) false
    :else      true))

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


(defmethod evaluate :assignment
  [state {:keys [token value]}]
  (let [state' (evaluate state value)]
    (assign-var state' token (:result state'))))


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

(defn- eval-stmts [state stmts]
  (reduce evaluate state stmts))

(defmethod evaluate :block
  [state {:keys [stmts]}]
  (-> state
      (update :env env/scope-push)
      (eval-stmts stmts)
      (update :env env/scope-pop)
      (assoc :result nil)))


(defmethod evaluate :expr-stmt
  [state expr]
  (evaluate state (:expr expr)))

(defmethod evaluate :grouping
  [state expr]
  (evaluate state (:expr expr)))

(defmethod evaluate :if-stmt
  [state {:keys [cnd then else]}]
  (let [state' (evaluate state cnd)]
    (cond
      (truthy? (:result state')) (evaluate state' then)
      (some? else) (evaluate state' else)
      :else (assoc state' :result nil))))

(defmethod evaluate :literal
  [state expr]
  (assoc state :result (:value expr)))

(defmethod evaluate :logical
  [state {:keys [left operator right]}]
  (let [state' (evaluate state left)
        l (:result state')
        op-type (:type operator)]
    (cond
      (and (= :or op-type) (truthy? l)) state'
      (and (= :and op-type) (not (truthy? l))) state'
      :else (evaluate state' right))))

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

(defmethod evaluate :while-stmt
  [state {:keys [cnd body] :as while}]
  (let [state' (evaluate state cnd)]
    (if (truthy? (:result state'))
      (recur (evaluate state' body) while)
      (assoc state' :result nil))))

(defmethod evaluate :unary
  [state {:keys [operator right]}]
  (let [state' (evaluate state right)
        value  (:result state')]
    (assoc state' :result
           (case (:type operator)
             :minus (numeric-op operator - value)
             :bang  (not (truthy? value))))))


(defn- new-state []
  {:env    (env/new-env)
   :result nil
   :errors []})


(defn interpret [stmts]
  (-> (reduce (fn [state stmt]
                (try
                  (evaluate state stmt)
                  (catch clojure.lang.ExceptionInfo e
                    (err/print-errors [(ex-data e)])
                    (-> state
                        (update :errors conj (ex-data e))
                        (assoc :result nil)))))
              (new-state)
              stmts)
      :result))
