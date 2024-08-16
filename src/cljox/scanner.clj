(ns cljox.scanner
  (:require [cljox.error :as err]))


(defn- new-scanner [source]
  {:source  source
   :length  (count source)
   :start   0
   :current 0
   :line    1
   :errors  []
   :tokens  []})


(defn- create-token
  ([token-type lexeme]
   (create-token token-type lexeme nil 1 0))
  ([token-type lexeme literal line position]
   {:type    token-type
    :lexeme  lexeme
    :literal literal
    :line    line
    :pos     position}))

(defn- current-lexeme [{:keys [source start current]}]
  (subs source start current))

(defn- at-end? [{:keys [current length]}]
  (>= current length))

(defn- in-progress? [m]
  (not (at-end? m)))

(defn- advance [m]
  (update m :current inc))

(defn- current-char [{:keys [source current]}]
  (nth source current))

(defn- matches? [scanner expected]
  (and (in-progress? scanner)
       (= expected (current-char scanner))))


(defn- consume-comment [scanner]
  (if (or (at-end? scanner)
          (= \newline (current-char scanner)))
    scanner
    (recur (advance scanner))))


(defn- digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn- alpha? [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))
      (= \_ c)))

(defn- alphanum? [c]
  (or (alpha? c) (digit? c)))


(defn- add-error [scanner msg]
  (let [err (err/scanning-error scanner msg)]
    (update scanner :errors conj err)))


(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (let [lexeme (case token-type
                  :eof "EOF"
                  (current-lexeme scanner))
         token (create-token token-type
                             lexeme
                             literal
                             (:line scanner)
                             (:current scanner))]
     (update scanner :tokens conj token))))


(defn- add-string [scanner]
  (cond
    (at-end? scanner)
    (add-error scanner "unterminated string")

    (= \" (current-char scanner))
    (let [value (-> (current-lexeme scanner)
                    (subs 1))] ; we don't want \" here
      (add-token (advance scanner) :string value))

    :else
    (recur (advance
            (if (= \newline (current-char scanner))
              (update scanner :line inc)
              scanner)))))


(defn- consume-digits [scanner]
  (if (and (in-progress? scanner)
           (digit? (current-char scanner)))
    (recur (advance scanner))
    scanner))

(defn- consume-decimal [scanner]
  (let [asc (advance scanner)]
    (if (and (in-progress? scanner)
             (= \. (current-char scanner))
             (in-progress? asc)
             (digit? (current-char asc)))
      (consume-digits asc)
      scanner)))

(defn- add-number [scanner]
  (let [sc (-> scanner
               consume-digits
               consume-decimal)]
    (add-token sc
               :number
               (Double/parseDouble (current-lexeme sc)))))


(def reserved-words
  #{"and" "class" "else" "false" "for" "fun"
    "if" "nil" "or" "print" "return" "super"
    "this" "true" "var" "while"})

(defn- add-identifier [scanner]
  (if (or (at-end? scanner)
          (not (alphanum? (current-char scanner))))
    (let [text (current-lexeme scanner)]
      (if (reserved-words text)
        (add-token scanner (keyword text))
        (add-token scanner :identifier)))
    (recur (advance scanner))))



(defn- scan-token [scanner]
  (let [c    (current-char scanner)
        asc  (advance scanner)
        aasc (advance asc)]
    (case c
      \( (add-token asc :left-paren)
      \) (add-token asc :right-paren)
      \{ (add-token asc :left-brace)
      \} (add-token asc :right-brace)
      \, (add-token asc :comma)
      \. (add-token asc :dot)
      \- (add-token asc :minus)
      \+ (add-token asc :plus)
      \; (add-token asc :semicolon)
      \* (add-token asc :star)
      \! (if (matches? asc \=)
           (add-token aasc :bang-equal)
           (add-token asc :bang))
      \= (if (matches? asc \=)
           (add-token aasc :equal-equal)
           (add-token asc :equal))
      \< (if (matches? asc \=)
           (add-token aasc :less-equal)
           (add-token asc :less))
      \> (if (matches? asc \=)
           (add-token aasc :greater-equal)
           (add-token asc :greater))
      \/ (if (matches? asc \/)
           (consume-comment aasc)
           (add-token asc :slash))
      \" (add-string asc)
      (\space
       \return
       \tab)   asc
      \newline (update asc :line inc)
      (cond
        (digit? c) (add-number scanner)
        (alpha? c) (add-identifier scanner)
        :else (add-error scanner "unexpected character")))))


(defn- next-token [scanner]
  (assoc scanner :start (:current scanner)))


(defn scan-tokens [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (-> scanner
          (add-token :eof)
          ((juxt :tokens :errors)))
      (recur (-> scanner
                 scan-token
                 next-token)))))
