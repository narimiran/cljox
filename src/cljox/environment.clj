(ns cljox.environment
  (:require [cljox.error :as err]))



(defn new-env
  ([] (new-env {}))
  ([natives]
   (atom (assoc natives :enclosing nil))))


(defn- throw-error [token msg]
  (throw (ex-info "runtime error"
                  (err/runtime-error token msg))))

(defn declare-name! [env name value]
  (swap! env assoc name value)
  env)

(defn get-var [env token]
  (let [lex (:lexeme token)]
    (if (contains? @env lex) ; it can exist and be `nil`
      (@env lex)
      (if-let [enclosing (:enclosing @env)]
        (get-var enclosing token)
        (throw-error token (str "undefined variable '" (:lexeme token) "'"))))))

(defn assign! [env token value]
  (let [name (:lexeme token)]
    (if (contains? @env name)
      (declare-name! env name value)
      (if-let [enclosing (:enclosing @env)]
        (assign! enclosing token value)
        (throw-error token (str "undefined variable '" (:lexeme token) "'"))))))

(defn scope-push [env]
  (atom {:enclosing env}))

(defn scope-pop [env]
  (:enclosing @env))
