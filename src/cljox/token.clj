(ns cljox.token)

(defn create-token
  ([token-type lexeme]
   (create-token token-type lexeme nil 1))
  ([token-type lexeme literal line]
   {:type    token-type
    :lexeme  lexeme
    :literal literal
    :line    line}))
