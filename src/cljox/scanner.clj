(ns cljox.scanner
  (:require [cljox.token :as token]
            [cljox.utils :as u]))


(defn- new-scanner [source]
  {:source  source
   :length  (count source)
   :start   0
   :current 0
   :line    1
   :errors  []
   :tokens  []})


(defn- current-lexeme [{:keys [source start current]}]
  (subs source start current))

(defn- matches? [scanner expected]
  (and (u/in-progress? scanner)
       (= expected (u/current-element scanner))))


(defn- consume-comment [scanner]
  (if (or (u/at-end? scanner)
          (= \newline (u/current-element scanner)))
    scanner
    (recur (u/advance scanner))))


(defn- digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn- alpha? [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))
      (= \_ c)))

(defn- alphanum? [c]
  (or (alpha? c) (digit? c)))


(defn- add-error [scanner message]
  (let [err {:kind :scanning-error
             :line (:line scanner)
             :location (:current scanner)
             :msg  message}]
    (update scanner :errors conj err)))


(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (let [text (case token-type
                :eof "EOF"
                (current-lexeme scanner))
         token (token/create-token token-type
                                   text
                                   literal
                                   (:line scanner))]
     (update scanner :tokens conj token))))


(defn- add-string [scanner]
  (cond
    (u/at-end? scanner)
    (add-error scanner "unterminated string")

    (= \" (u/current-element scanner))
    (let [value (-> (current-lexeme scanner)
                    (subs 1))] ; we don't want \" here
      (add-token (u/advance scanner) :string value))

    :else
    (recur (u/advance
            (if (= \newline (u/current-element scanner))
              (update scanner :line inc)
              scanner)))))


(defn- consume-digits [scanner]
  (if (and (u/in-progress? scanner)
           (digit? (u/current-element scanner)))
    (recur (u/advance scanner))
    scanner))

(defn- consume-decimal [scanner]
  (let [asc (u/advance scanner)]
    (if (and (u/in-progress? scanner)
             (= \. (u/current-element scanner))
             (u/in-progress? asc)
             (digit? (u/current-element asc)))
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
  (if (or (u/at-end? scanner)
          (not (alphanum? (u/current-element scanner))))
    (let [text (current-lexeme scanner)]
      (if (reserved-words text)
        (add-token scanner (keyword text))
        (add-token scanner :identifier)))
    (recur (u/advance scanner))))



(defn- scan-token [scanner]
  (let [c    (u/current-element scanner)
        asc  (u/advance scanner)
        aasc (u/advance asc)]
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
    (if (u/at-end? scanner)
      (let [sc (add-token scanner :eof)]
        [(:tokens sc) (:errors sc)])
      (recur (-> scanner
                 scan-token
                 next-token)))))
