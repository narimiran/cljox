(ns cljox.core
  (:require
   [cljox.scanner :as scanner]
   [cljox.parser :as parser]
   [cljox.resolver :as resolver]
   [cljox.interpreter :as i]
   [cljox.error :as err]))


(defn- error-exit [message code]
  (println message)
  (System/exit code))

(defn- report-errors [state]
  (let [errors (:errors state)]
    (when (seq errors)
      (err/print-errors errors))
    (assoc state :errors [])))


(defn- run
  ([source]
   (-> (i/new-interpreter)
       (run source)
       :result))
  ([state source]
   (let [[tokens scan-errors] (scanner/scan-tokens source)
         [stmts parse-errors] (parser/parse tokens)
         errors (concat scan-errors parse-errors)]
     (if (seq errors)
       (err/print-errors errors)
       (let [[locals resolve-errors] (resolver/resolve-locals stmts)]
         (if (seq resolve-errors)
           (err/print-errors resolve-errors)
           (-> state
               (i/interpret stmts locals)
               report-errors)))))))


(defn- run-prompt []
  (loop [state (i/new-interpreter)]
    (print "> ")
    (flush)
    (when-let [line (read-line)]
      (recur (run state line)))))


(defn- run-file [filename]
  (try
    (let [state (i/new-interpreter)
          source (slurp filename)]
      (:result (run state source)))
    (catch java.io.FileNotFoundException e
      (error-exit (ex-message e) 65))))



(defn -main [& args]
  (let [arg-num (count args)]
   (cond
     (zero? arg-num) (run-prompt)
     (= 1 arg-num) (run-file (first args))
     :else (error-exit "Usage: cljox [script]" 64))))
