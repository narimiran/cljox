(ns cljox.return-exception
  (:gen-class
   :name cljox.ReturnException
   :extends java.lang.RuntimeException
   :state state
   :init init
   :constructors {[Object] [String Throwable boolean boolean]}))

(defn -init [value]
  [[nil nil false false] value])
