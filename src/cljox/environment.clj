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

(defn go-up [env dist]
  (if (zero? dist)
    env
    (recur (:enclosing @env) (dec dist))))

(defn get-var [env token]
  (let [lex (:lexeme token)]
    (if (contains? @env lex) ; it can exist and be `nil`
      (@env lex)
      (throw-error token (format "undefined variable '%s'" lex)))))

(defn assign! [env token value]
  (let [lex (:lexeme token)]
    (if (contains? @env lex)
      (declare-name! env lex value)
      (throw-error token (format "undefined variable '%s'" lex)))))

(defn scope-push [env]
  (atom {:enclosing env}))

(defn scope-pop [env]
  (:enclosing @env))
