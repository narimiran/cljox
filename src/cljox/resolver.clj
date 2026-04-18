(ns cljox.resolver
  (:require [cljox.error :as err]))


(defn begin-scope [sc]
  (update sc :scopes conj {}))

(defn end-scope [sc]
  (update sc :scopes pop))

(defn add-to-scope [sc name ready?]
  (let [[hd & tl] (:scopes sc)
        hd' (assoc hd name ready?)]
    (assoc sc :scopes (conj tl hd'))))

(defn add-error [sc token msg]
  (update sc :errors conj (err/parsing-error token msg)))

(defn declare-tok [sc token]
  (let [scope (peek (:scopes sc))
        name (:lexeme token)]
    (if scope
      (if (contains? scope name)
        (add-error sc token "already a variable with this name in this scope")
        (add-to-scope sc name false))
      sc)))

(defn define-tok [sc token]
  (let [scope (peek (:scopes sc))
        name (:lexeme token)]
    (if scope
      (add-to-scope sc name true)
      sc)))

(defn resolve-local [sc node name]
  (loop [i 0
         scopes (:scopes sc)]
    (cond
      (empty? scopes) sc
      (contains? (peek scopes) name) (update sc :locals assoc node i)
      :else (recur (inc i) (rest scopes)))))

(defn declare-and-define-params [sc params]
  (reduce (fn [sc param]
            (-> sc
                (declare-tok param)
                (define-tok param)))
          sc
          params))


(defn new-semchecker []
  {:locals {}
   :errors []
   :scopes '()
   :func-stack '()
   :class-stack '()})
