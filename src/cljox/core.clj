(ns cljox.core
  (:require
   [cljox.scanner :as scanner]
   [cljox.parser :as parser]
   [cljox.interpreter :as i]
   [cljox.error :as err]))


(defn- error-exit [message code]
  (println message)
  (System/exit code))



(defn- run [source]
  (let [[tokens scan-errors] (scanner/scan-tokens source)
        [stmts parse-errors] (parser/parse tokens)
        errors (concat scan-errors parse-errors)]
    (if (seq errors)
      (err/print-errors errors)
      (i/interpret stmts))))



(defn- run-prompt []
  (print "> ")
  (flush)
  (when-let [line (read-line)]
    (run line)
    (recur)))

(defn- run-file [filename]
  (try
    (let [source (slurp filename)]
      (run source))
    (catch java.io.FileNotFoundException e
      (error-exit (ex-message e) 65))))



(defn -main [& args]
  (let [arg-num (count args)]
   (cond
     (zero? arg-num) (run-prompt)
     (= 1 arg-num) (run-file (first args))
     :else (error-exit "Usage: cljox [script]" 64))))
