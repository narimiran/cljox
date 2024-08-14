(ns cljox.callable)

(defprotocol LoxCallable
  (arity [this])
  (call [this state args])
  (to-string [this]))
