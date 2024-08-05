(ns cljox.core
  (:require
   [clojure.pprint :as pp]
   [cljox.scanner :as scanner]))


(defn- print-error [message code]
  (println message)
  (System/exit code))


(defn- run [source]
  (pp/pprint (scanner/scan-tokens source)))

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
