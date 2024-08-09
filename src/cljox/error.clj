(ns cljox.error)


(defn scanning-error [scanner msg]
  {:kind :scanning-error
   :line (:line scanner)
   :loc  (:current scanner)
   :msg  msg})

(defn parsing-error [token msg]
  {:kind :parsing-error
   :line (:line token)
   :loc  (:lexeme token)
   :msg  msg})

(defn runtime-error [token msg]
  {:kind :runtime-error
   :line (:line token)
   :loc  (:lexeme token)
   :msg  msg})



(defn fmt-error [{:keys [kind line loc msg]}]
  (format "[%s] Error on line %d at %s: %s" (name kind) line loc msg))


(defn print-errors [errors]
  (doseq [error errors]
    (println (fmt-error error))))
