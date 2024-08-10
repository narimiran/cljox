(ns cljox.environment
  (:require [cljox.error :as err]))


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
      (throw-error token (str "undefined variable '" (:lexeme token) "'")))))
