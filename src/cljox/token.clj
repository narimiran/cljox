(ns cljox.token)

(defn create-token
  [token-type lexeme literal line]
  {:type    token-type
   :lexeme  lexeme
   :literal literal
   :line    line})
