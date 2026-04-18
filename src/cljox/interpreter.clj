(ns cljox.interpreter
  (:require [clojure.string :as str]
            [cljox.error :as err]
            [cljox.environment :as env]
            [cljox.resolver :as r])
  (:import [cljox ReturnException]))


(defprotocol LoxCallable
  (arity [this])
  (call [this state args])
  (to-string [this]))

(defprotocol ProtoInstance
  (method-bind [this method-func])
  (getter [this token])
  (setter! [this token value]))

(defprotocol LoxType
  (evaluate [this state])
  (semcheck [this sc]))


(defn- throw-error [token msg]
  (throw (ex-info "runtime error"
                  (err/runtime-error token msg))))

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
  (case v
    (nil false) false
    true))

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



(defn- eval-stmts [state stmts]
  (reduce #(evaluate %2 %1) state stmts))

(defn- semcheck-stmts [sc stmts]
  (reduce #(semcheck %2 %1) sc stmts))

(defn- resolve-func [sc {:keys [params body]} func-type]
  (-> sc
      (update :func-stack conj func-type)
      r/begin-scope
      (r/declare-and-define-params params)
      (semcheck-stmts body)
      r/end-scope
      (update :func-stack pop)))



(defrecord Assignment [token value]
  LoxType
  (evaluate [asgn state]
    (let [state' (evaluate value state)]
      (assign-var state' token asgn (:result state'))))
  (semcheck [asgn sc]
    (-> (semcheck value sc)
        (r/resolve-local asgn (:lexeme token)))))


(defrecord Binary [left operator right]
  LoxType
  (evaluate [_ state]
    (let [state'  (evaluate left state)
          l       (:result state')
          state'' (evaluate right state')
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
  (semcheck [_ sc]
    (->> sc
         (semcheck left)
         (semcheck right))))


(defrecord Block [stmts]
  LoxType
  (evaluate [_ state]
    (-> state
        (update :env env/scope-push)
        (eval-stmts stmts)
        (update :env env/scope-pop)
        (assoc :result nil)))
  (semcheck [_ sc]
    (-> sc
        r/begin-scope
        (semcheck-stmts stmts)
        r/end-scope)))

(defn- eval-args [state args]
  (reduce
   (fn [s arg]
     (let [res (:result s)
           s' (evaluate arg s)]
       (assoc s' :result (conj res (:result s')))))
   (assoc state :result [])
   args))

(defrecord Call [calee paren args]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate calee state)
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

  (semcheck [_ sc]
    (let [sc' (semcheck calee sc)]
      (reduce #(semcheck %2 %1) sc' args))))


(defn- declare-var [state token value]
  (env/declare-name! (:env state) (:lexeme token) value)
  state)

(defn- declare-args [state params args]
  (reduce
   (fn [s i]
     (declare-var s (params i) (args i)))
   state
   (range (count params))))

(defn- execute-func-body [state {:keys [decl closure init?]}]
  (try
    (-> state
        (eval-stmts (:body decl))
        (assoc :result (when init? (@closure "this"))))
    (catch ReturnException e
      (if init?
         (assoc state :result (@closure "this"))
         (.state e)))))


(defn- find-method [{:keys [methods super]} name]
  (if-let [method (methods name)]
    method
    (when super
      (recur super name))))

(defrecord LoxFunction [decl closure init?]
  LoxCallable
  (arity [_]
    (-> decl :params count))
  (call [this state args]
    (let [current-env (:env state)]
      (-> state
          (assoc :env (env/scope-push closure))
          (declare-args (:params decl) args)
          (execute-func-body this)
          (assoc :env current-env))))
  (to-string [_]
    (format "<fn %s>" (-> decl :token :lexeme))))


(defrecord LoxInstance [klass fields]
  ProtoInstance
  (method-bind [this {:keys [decl closure init?]}]
    (let [env (env/scope-push closure)]
      (env/declare-name! env "this" this)
      (->LoxFunction decl env init?)))
  (getter [this token]
    (let [name (:lexeme token)
          field-name (@fields name :missing)] ; it can exist and be `nil`/`false`
      (if-not (= :missing field-name)
        field-name
        (if-let [method-func (find-method klass name)]
          (method-bind this method-func)
          (throw-error token (format "undefined property '%s'" name))))))
  (setter! [_ {:keys [lexeme]} value]
    (swap! fields assoc lexeme value)))


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
  (to-string [_] name))



(defn- eval-superclass [state super]
  (if super
    (let [state' (evaluate super state)]
      (if (instance? LoxClass (:result state'))
        state'
        (throw-error (:token super) "superclass must be a class")))
    (assoc state :result nil)))

(defn- resolve-superclass [sc super]
  (-> (semcheck super sc)
      r/begin-scope
      (r/add-to-scope "super" true)))

(defn- create-methods [env methods]
  (reduce (fn [acc method]
            (let [method-name (-> method :token :lexeme)
                  method-func (->LoxFunction method env (= "init" method-name))]
              (conj acc {method-name method-func})))
          {}
          methods))

(defn- resolve-methods [sc methods]
  (reduce (fn [sc method]
            (let [func-type (if (= "init" (-> method :token :lexeme))
                              :init
                              :method)]
              (resolve-func sc method func-type)))
          sc
          methods))


(defrecord ClassStmt [token super methods]
  LoxType
  (evaluate [_ state]
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
  (semcheck [_ sc]
    (if (and super
             (= (:lexeme token) (-> super :token :lexeme)))
      (r/add-error sc (:token super) "a class can't inherit from itself")
      (-> sc
          (update :class-stack conj (if super :subclass :class))
          (r/declare-tok token)
          (r/define-tok token)
          (cond-> super (resolve-superclass super))
          r/begin-scope
          (r/add-to-scope "this" true)
          (resolve-methods methods)
          r/end-scope
          (cond-> super r/end-scope)
          (update :class-stack pop)))))


(defrecord ExprStmt [expr]
  LoxType
  (evaluate [_ state] (evaluate expr state))
  (semcheck [_ sc] (semcheck expr sc)))


(defrecord FuncDecl [token params body]
  LoxType
  (evaluate [stmt state]
    (declare-var state token (->LoxFunction stmt (:env state) false)))
  (semcheck [node sc]
    (-> sc
        (r/declare-tok token)
        (r/define-tok token)
        (resolve-func node :func))))


(defrecord GetExpr [object token]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate object state)
          obj (:result state')]
      (if (instance? LoxInstance obj)
        (assoc state' :result (getter obj token))
        (throw-error token "only instances have properties"))))
  (semcheck [_ sc]
    (semcheck object sc)))


(defrecord Grouping [expr]
  LoxType
  (evaluate [_ state] (evaluate expr state))
  (semcheck [_ sc] (semcheck expr sc)))


(defrecord IfStmt [cnd then else]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate cnd state)]
      (cond
        (truthy? (:result state')) (evaluate then state')
        (some? else) (evaluate else state')
        :else (assoc state' :result nil))))
  (semcheck [_ sc]
    (->> sc
         (semcheck cnd)
         (semcheck then)
         (#(if else (semcheck else %) %)))))


(defrecord Literal [value]
  LoxType
  (evaluate [_ state] (assoc state :result value))
  (semcheck [_ sc] sc))


(defrecord Logical [left operator right]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate left state)
          l (:result state')
          op-type (:type operator)]
      (cond
        (or (and (= :or op-type) (truthy? l))
            (and (= :and op-type) (not (truthy? l)))) state'
        :else (evaluate right state'))))
  (semcheck [_ sc]
    (->> sc
         (semcheck left)
         (semcheck right))))

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

(defrecord PrintStmt [expr]
  LoxType
  (evaluate [_ state]
    (->> state
         (evaluate expr)
         print-result))
  (semcheck [_ sc]
    (semcheck expr sc)))


(defrecord ReturnStmt [kword value]
  LoxType
  (evaluate [_ state]
    (let [state' (if (nil? value)
                   (assoc state :result nil)
                   (evaluate value state))]
      (throw (ReturnException. state'))))
  (semcheck [_ sc]
    (cond
      (empty? (:func-stack sc)) (r/add-error sc kword "can't return from top-level code")
      (some? value) (if (= :init (peek (:func-stack sc)))
                      (r/add-error sc kword "can't return a value from an initializer")
                      (semcheck value sc))
      :else sc)))

(defrecord SetExpr [object token value]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate object state)
          obj (:result state')]
      (if (instance? LoxInstance obj)
        (let [state'' (evaluate value state')
              v (:result state'')]
          (setter! obj token v)
          state'')
        (throw-error token "only instances have fields"))))
  (semcheck [_ sc]
    (->> sc
         (semcheck value)
         (semcheck object))))

(defrecord Super [kword method]
  LoxType
  (evaluate [expr state]
    (let [dist ((:locals state) expr)
          ob-env (env/go-up (:env state) (dec dist))
          object (@ob-env "this")
          sc-env (env/go-up ob-env 1)
          superclass (@sc-env "super")
          name (-> expr :method :lexeme)]
      (if-let [method (find-method superclass name)]
        (assoc state :result (method-bind object method))
        (throw-error (:method expr) (format "undefined property '%s'" name)))))
  (semcheck [super sc]
    (case (peek (:class-stack sc))
      :subclass (r/resolve-local sc super (-> super :kword :lexeme))
      :class (r/add-error sc (:kword super) "can't use 'super' in a class with no superclass")
      nil (r/add-error sc (:kword super) "can't use 'super' outside of a class"))))

(defrecord This [kword]
  LoxType
  (evaluate [expr state]
    (lookup-var state kword expr))
  (semcheck [this sc]
    (if (empty? (:class-stack sc))
      (r/add-error sc kword "can't use 'this' outside of a class")
      (r/resolve-local sc this (:lexeme kword)))))


(defrecord Unary [operator right]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate right state)
          value  (:result state')]
      (assoc state' :result
             (case (:type operator)
               :minus (numeric-op operator - value)
               :bang  (not (truthy? value))))))
  (semcheck [_ sc]
    (semcheck right sc)))


(defrecord VarDecl [token init]
  LoxType
  (evaluate [_ state]
    (let [state' (if init
                   (evaluate init state)
                   (assoc state :result nil))]
      (-> state'
          (declare-var token (:result state'))
          (assoc :result nil))))
  (semcheck [_ sc]
   (-> sc
       (r/declare-tok token)
       (cond->> init (semcheck init))
       (r/define-tok token))))


(defrecord Variable [token]
  LoxType
  (evaluate [var state]
    (lookup-var state token var))
  (semcheck [var sc]
    (let [scope (peek (:scopes sc))
          name (:lexeme token)]
      (if (and scope (false? (scope name)))
        (r/add-error sc token "can't read local variable in its own initializer")
        (r/resolve-local sc var name)))))


(defrecord WhileStmt [cnd body]
  LoxType
  (evaluate [_ state]
    (let [state' (evaluate cnd state)]
      (if (truthy? (:result state'))
        (recur (evaluate body state'))
        (assoc state' :result nil))))
  (semcheck [_ sc]
    (->> sc
         (semcheck cnd)
         (semcheck body))))

(def clock
  (reify LoxCallable
    (arity [_] 0)
    (call [_ state _]
      (assoc state :result (/ (System/currentTimeMillis) 1000.0)))
    (to-string [_] "<native fn>")))

(def natives
  {"clock" clock})

(defn resolve-locals [stmts]
  (-> (r/new-semchecker)
      (semcheck-stmts stmts)
      ((juxt :locals :errors))))


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
