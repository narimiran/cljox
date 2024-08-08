(ns cljox.core
  (:require
   [cljox.scanner :as scanner]
   [cljox.parser :as parser]
   [cljox.ast-printer :as ap]))


(defn- print-error [message code]
  (println message)
  (System/exit code))


(defn- run [source]
  (let [[tokens scan-errors] (scanner/scan-tokens source)
        [expr parse-errors] (parser/parse tokens)
        errors (concat scan-errors parse-errors)]
    (if (seq errors)
      errors
      (ap/pprint expr))))


(defn- run-prompt []
  (print "> ")
  (flush)
  (let [line (read-line)]
    (when line
      (run line)
      (recur))))

(defn- run-file [filename]
  (try
    (let [source (slurp filename)]
      (run source))
    (catch java.io.FileNotFoundException e
      (print-error (.getMessage e) 65))))



(defn -main [& args]
  (let [arg-num (count args)]
   (cond
     (zero? arg-num) (run-prompt)
     (= 1 arg-num) (run-file (first args))
     :else (print-error "Usage: cljox [script]" 64))))
