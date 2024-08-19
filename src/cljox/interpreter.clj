(ns cljox.interpreter
  (:require [clojure.string :as str]
            [cljox.error :as err]
            [cljox.environment :as env]))


(defmulti evaluate
  (fn [_ m] (:type m)))

(defprotocol LoxCallable
  (arity [this])
  (call [this state args])
  (to-string [this]))

(defprotocol ProtoInstance
  (method-bind [this method-func])
  (getter [this token])
  (setter! [this token value]))



(defn- throw-error [token msg]
  (throw (ex-info "runtime error"
                  (err/runtime-error token msg))))

(defn- declare-var [state token value]
  (env/declare-name! (:env state) (:lexeme token) value)
  state)

(defn- assign-var [{:keys [locals globals env] :as state}
                   token expr value]
  (env/assign! (if-let [dist (locals expr)]
                 (env/go-up env dist)
                 globals)
               token
               value)
  state)

(defn- lookup-var [{:keys [locals globals env] :as state}
                   token expr]
  (assoc state :result
         (env/get-var (if-let [dist (locals expr)]
                        (env/go-up env dist)
                        globals)
                      token)))

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
  [state {:keys [token value] :as asgn}]
  (let [state' (evaluate state value)]
    (assign-var state' token asgn (:result state'))))


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

(defn- eval-args [state args]
  (reduce
   (fn [s arg]
     (let [res (:result s)
           s' (evaluate s arg)]
       (assoc s' :result (conj res (:result s')))))
   (assoc state :result [])
   args))

(defmethod evaluate :call
  [state {:keys [calee args paren]}]
  (let [state' (evaluate state calee)
        calee' (:result state')
        state'' (eval-args state' args)
        args' (:result state'')]
    (cond
      (not (satisfies? LoxCallable calee'))
      (throw-error paren "can only call functions and classes")

      (not= (arity calee') (count args'))
      (throw-error paren (format "expected %d arguments, but got %d"
                                 (arity calee') (count args')))
      :else
      (call calee' state'' args'))))



(defn- declare-args [state params args]
  (reduce
   (fn [s [p a]]
     (declare-var s p a))
   state
   (zipmap params args)))

(defn- execute-func-body [state {:keys [decl closure init?]}]
  (try
    (-> state
        (eval-stmts (:body decl))
        (assoc :result (when init? (@closure "this"))))
    (catch clojure.lang.ExceptionInfo e
      (if (= "return statement" (ex-message e))
        (if init?
          (assoc state :result (@closure "this"))
          (:state (ex-data e)))
        (throw e)))))

(defn- find-method [{:keys [methods super]} name]
  (if-let [method (methods name)]
    method
    (when super
      (recur super name))))

(defrecord LoxFunction [decl closure init?]
  LoxCallable

  (arity [this]
    (-> this :decl :params count))

  (call [this state args]
    (let [current-env (:env state)]
      (-> state
          (assoc :env (env/scope-push (:closure this)))
          (declare-args (-> this :decl :params) args)
          (execute-func-body this)
          (assoc :env current-env))))

  (to-string [this]
    (format "<fn %s>" (-> this :decl :token :lexeme))))


(defrecord LoxInstance [klass fields]
  ProtoInstance

  (method-bind [this {:keys [decl closure init?]}]
    (let [env (env/scope-push closure)]
      (env/declare-name! env "this" this)
      (->LoxFunction decl env init?)))

  (getter [this token]
    (let [name (:lexeme token)
          fields @(:fields this)]
      (if (contains? fields name) ; it can exist and be `nil`
        (fields name)
        (if-let [method-func (find-method (:klass this) name)]
          (method-bind this method-func)
          (throw-error token (format "undefined property '%s'" name))))))

  (setter! [this token value]
    (let [name (:lexeme token)]
      (swap! (:fields this) assoc name value))))


(defrecord LoxClass [name super methods]
  LoxCallable

  (arity [this]
    (if-let [init (find-method this "init")]
      (arity init)
      0))

  (call [this state args]
    (let [instance (->LoxInstance this (atom {}))
          init (find-method this "init")
          state' (if init
                   (-> (method-bind instance init)
                       (call state args))
                   state)]
      (assoc state' :result instance)))

  (to-string [this]
    (:name this)))

(defn- eval-superclass [state super]
  (if super
    (let [state' (evaluate state super)]
      (if (instance? LoxClass (:result state'))
        state'
        (throw-error (:token super) "superclass must be a class")))
    (assoc state :result nil)))

(defn- create-methods [env methods]
  (reduce (fn [acc method]
            (let [method-name (-> method :token :lexeme)
                  method-func (->LoxFunction method env (= "init" method-name))]
              (conj acc {method-name method-func})))
          {}
          methods))

(defmethod evaluate :class-stmt
  [state {:keys [token super methods]}]
  (let [name (:lexeme token)
        state' (eval-superclass state super)
        superclass (:result state')
        curr-env (:env state')
        new-env (if super
                  (-> (env/scope-push curr-env)
                      (env/declare-name! "super" superclass))
                  curr-env)
        methods (create-methods new-env methods)
        klass (->LoxClass name superclass methods)]
    (declare-var state' token klass)))


(defmethod evaluate :expr-stmt
  [state expr]
  (evaluate state (:expr expr)))

(defmethod evaluate :func-decl
  [state stmt]
  (declare-var state (:token stmt) (->LoxFunction stmt (:env state) false)))

(defmethod evaluate :get-expr
  [state {:keys [object token]}]
  (let [state' (evaluate state object)
        obj (:result state')]
    (if (instance? LoxInstance obj)
      (assoc state' :result (getter obj token))
      (throw-error token "only instances have properties"))))

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

(defn- prettify [v]
  (cond
    (instance? Double v) (let [s (str v)]
                           (if (str/ends-with? s ".0")
                             (subs s 0 (- (count s) 2))
                             s))
    (instance? LoxInstance v) (str (-> v :klass :name) " instance")
    (satisfies? LoxCallable v) (to-string v)
    :else v))

(defn- print-result [state]
  (println (prettify (:result state)))
  (assoc state :result nil))

(defmethod evaluate :print-stmt
  [state {:keys [expr]}]
  (-> state
      (evaluate expr)
      print-result))

(defmethod evaluate :return-stmt
  [state {:keys [value]}]
  (let [state' (if (nil? value)
                 (assoc state :result nil)
                 (evaluate state value))]
    (throw (ex-info "return statement" {:state state'}))))

(defmethod evaluate :set-expr
  [state {:keys [object token value]}]
  (let [state' (evaluate state object)
        obj (:result state')]
    (if (instance? LoxInstance obj)
      (let [state'' (evaluate state' value)
            v (:result state'')]
        (setter! obj token v)
        state'')
      (throw-error token "only instances have fields"))))

(defmethod evaluate :super
  [state expr]
  (let [dist ((:locals state) expr)
        ob-env (env/go-up (:env state) (dec dist))
        object (@ob-env "this")
        sc-env (env/go-up ob-env 1)
        superclass (@sc-env "super")
        name (-> expr :method :lexeme)]
    (if-let [method (find-method superclass name)]
      (assoc state :result (method-bind object method))
      (throw-error (:method expr) (format "undefined property '%s'" name)))))

(defmethod evaluate :this
  [state expr]
  (lookup-var state (:kword expr) expr))

(defmethod evaluate :var-decl
  [state {:keys [token init]}]
  (let [state' (if init
                 (evaluate state init)
                 (assoc state :result nil))]
    (-> state'
        (declare-var token (:result state'))
        (assoc :result nil))))

(defmethod evaluate :variable
  [state var]
  (lookup-var state (:token var) var))

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

(def clock
  (reify LoxCallable
    (arity [_] 0)
    (call [_ state _]
      (assoc state :result (/ (System/currentTimeMillis) 1000.0)))
    (to-string [_] "<native fn>")))

(def natives
  {"clock" clock})

(defn new-interpreter []
  (let [globals (env/new-env natives)]
    {:env     globals
     :globals globals
     :locals  {}
     :result  nil
     :errors  []}))


(defn interpret [state stmts locals]
  (try
    (-> state
        (update :locals merge locals)
        (eval-stmts stmts))
    (catch clojure.lang.ExceptionInfo e
      (-> state
          (update :errors conj (ex-data e))
          (assoc :result nil)))))
