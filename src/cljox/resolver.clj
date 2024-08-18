(ns cljox.resolver
  (:require [cljox.error :as err]))


(defmulti semcheck
  (fn [_ m] (:type m)))


(defn- semcheck-stmts [sc stmts]
  (reduce semcheck sc stmts))

(defn- begin-scope [sc]
  (update sc :scopes conj {}))

(defn- end-scope [sc]
  (update sc :scopes pop))

(defn- add-to-scope [sc name ready?]
  (let [[hd & tl] (:scopes sc)
        hd' (assoc hd name ready?)]
    (assoc sc :scopes (conj tl hd'))))

(defn- add-error [sc token msg]
  (update sc :errors conj (err/parsing-error token msg)))

(defn- declare-tok [sc token]
  (let [scope (peek (:scopes sc))
        name (:lexeme token)]
    (if scope
      (if (contains? scope name)
        (add-error sc token "already a variable with this name in this scope")
        (add-to-scope sc name false))
      sc)))

(defn- define-tok [sc token]
  (let [scope (peek (:scopes sc))
        name (:lexeme token)]
    (if scope
      (add-to-scope sc name true)
      sc)))

(defn- resolve-local [sc node name]
  (loop [i 0
         scopes (:scopes sc)]
    (cond
      (empty? scopes) sc
      (contains? (peek scopes) name) (update sc :locals assoc node i)
      :else (recur (inc i) (rest scopes)))))

(defn- declare-and-define-params [sc params]
  (reduce (fn [sc param]
            (-> sc
                (declare-tok param)
                (define-tok param)))
          sc
          params))

(defn- resolve-func [sc {:keys [params body]} func-type]
  (-> sc
      (update :func-stack conj func-type)
      begin-scope
      (declare-and-define-params params)
      (semcheck-stmts body)
      end-scope
      (update :func-stack pop)))

(defn- resolve-methods [sc methods]
  (reduce (fn [sc method]
            (resolve-func sc method :method))
          sc
          methods))



(defmethod semcheck :assignment
  [sc {:keys [token value] :as node}]
  (-> sc
      (semcheck value)
      (resolve-local node (:lexeme token))))

(defmethod semcheck :binary
  [sc {:keys [left right]}]
  (-> sc
      (semcheck left)
      (semcheck right)))

(defmethod semcheck :block
  [sc {:keys [stmts]}]
  (-> sc
      begin-scope
      (semcheck-stmts stmts)
      end-scope))

(defmethod semcheck :call
  [sc {:keys [calee args]}]
  (let [sc' (semcheck sc calee)]
    (reduce semcheck sc' args)))

(defmethod semcheck :class-stmt
  [sc {:keys [token methods]}]
  (-> sc
      (declare-tok token)
      (define-tok token)
      (resolve-methods methods)))

(defmethod semcheck :expr-stmt
  [sc {:keys [expr]}]
  (semcheck sc expr))

(defmethod semcheck :func-decl
  [sc {:keys [token] :as node}]
  (-> sc
      (declare-tok token)
      (define-tok token)
      (resolve-func node :func)))

(defmethod semcheck :get-expr
  [sc {:keys [object]}]
  (semcheck sc object))

(defmethod semcheck :grouping
  [sc {:keys [expr]}]
  (semcheck sc expr))

(defmethod semcheck :if-stmt
  [sc {:keys [cnd then else]}]
  (-> sc
      (semcheck cnd)
      (semcheck then)
      (cond-> else (semcheck else))))

(defmethod semcheck :literal
  [sc _]
  sc)

(defmethod semcheck :logical
  [sc {:keys [left right]}]
  (-> sc
      (semcheck left)
      (semcheck right)))

(defmethod semcheck :print-stmt
  [sc {:keys [expr]}]
  (semcheck sc expr))

(defmethod semcheck :return-stmt
  [sc {:keys [kword value]}]
  (cond
    (empty? (:func-stack sc)) (add-error sc kword "can't return from top-level code")
    (some? value) (semcheck sc value)
    :else sc))

(defmethod semcheck :set-expr
  [sc {:keys [object value]}]
  (-> sc
      (semcheck value)
      (semcheck object)))

(defmethod semcheck :unary
  [sc {:keys [right]}]
  (semcheck sc right))

(defmethod semcheck :var-decl
  [sc {:keys [token init]}]
  (-> sc
      (declare-tok token)
      (cond-> init (semcheck init))
      (define-tok token)))

(defmethod semcheck :variable
  [sc {:keys [token] :as node}]
  (let [scope (peek (:scopes sc))
        name (:lexeme token)]
    (if (and scope (false? (scope name)))
      (add-error sc token "can't read local variable in its own initializer")
      (resolve-local sc node name))))

(defmethod semcheck :while-stmt
  [sc {:keys [cnd body]}]
  (-> sc
      (semcheck cnd)
      (semcheck body)))


(defn- new-semchecker []
  {:locals {}
   :errors []
   :scopes '()
   :func-stack '()})


(defn resolve-locals [stmts]
  (-> (new-semchecker)
      (semcheck-stmts stmts)
      ((juxt :locals :errors))))
