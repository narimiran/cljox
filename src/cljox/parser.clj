(ns cljox.parser
  (:require [cljox.utils :as u]
            [cljox.ast :as ast]))


;; Grammar
;; =======
;;
;; expression     → equality ;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary
;;                | primary ;
;; primary        → NUMBER | STRING | "true" | "false" | "nil"
;;                | "(" expression ")" ;



(defn- new-parser [tokens]
  {:source  tokens
   :length  (count tokens)
   :errors  []
   :expr    nil
   :current 0})


(defn- matches? [m expected]
  (and (u/in-progress? m)
       (expected (:type (u/current-element m)))))


(defn- add-expr [parser expr]
  (assoc parser :expr expr))

(defn- add-literal [parser lit]
  (-> parser
      (add-expr (ast/literal lit))
      u/advance))

(defn- add-error [parser msg]
  (let [token (u/current-element parser)
        err {:kind :parsing-error
             :line (:line token)
             :location (:lexeme token)
             :msg msg}
        parser' (update parser :errors conj err)]
     (throw (ex-info "parsing error" parser'))))

(defn- consume [parser expected msg]
  (if (matches? parser #{expected})
    (u/advance parser)
    (add-error parser msg)))


(declare expression)

(defn- primary [parser]
  (cond
    (matches? parser #{:false}) (add-literal parser false)
    (matches? parser #{:true})  (add-literal parser true)
    (matches? parser #{:nil})   (add-literal parser nil)

    (matches? parser #{:number :string})
    (add-literal parser (:literal (u/current-element parser)))

    (matches? parser #{:left-paren})
    (let [mid (expression (u/advance parser))
          parser' (consume mid :right-paren "expected ')' after expression")]
      (add-expr parser' (ast/grouping (:expr mid))))

    :else (add-error parser "expected expression")))


(defn- unary [parser]
  (if (matches? parser #{:bang :minus})
    (let [operator (u/current-element parser)
          right (unary (u/advance parser))]
      (add-expr right (ast/unary operator (:expr right))))
    (primary parser)))


(defn- binary-op [parser op-method token-types]
  (loop [left (op-method parser)]
    (if (matches? left token-types)
      (let [operator (u/current-element left)
            right (op-method (u/advance left))]
        (recur (add-expr right (ast/binary (:expr left) operator (:expr right)))))
      left)))


(defn- factor [parser]
  (binary-op parser unary #{:slash :star}))

(defn- term [parser]
  (binary-op parser factor #{:minus :plus}))

(defn- comparison [parser]
  (binary-op parser term #{:greater :greater-equal :less :less-equal}))

(defn- equality [parser]
  (binary-op parser comparison #{:bang-equal :equal-equal}))

(defn- expression [parser]
  (equality parser))



(defn parse [tokens]
  (let [parser (new-parser tokens)]
    ((juxt :expr :errors)
     (try
       (expression parser)
       (catch clojure.lang.ExceptionInfo e
         (ex-data e))))))










(comment
  (require '[cljox.scanner :as scanner])
  (require '[cljox.ast-printer :as ap])

  (defn testing [source]
    (let [[tokens errors] (scanner/scan-tokens source)]
      (if (seq errors)
        errors
        (let [[expr errors] (parse tokens)]
          (if (seq errors)
            errors
            (ap/pprint expr))))))

  (testing "1 + 2")
  (testing "1 + 2 +")
  (testing "1 * -3")
  (testing "1 + - 3 * 4")
  (testing "1 + * 3")
  (testing "2 * (3 + 4)")
  (testing "2 * (3 + * 4) + \"ab")
  (testing "c++")
  (testing "\"c\"+\"d\"")
  (testing "0 + nil")
  (testing "0 == 1 != 2")
  (testing "\"abc\" != \"def\" == \"ghi\"")
  (testing "= 3")
  (testing "== 3")
  (testing "3 + \"ab")
  (testing "3 + \"ab\ncd")
  (testing "3 + (4")
  (testing "3 + )4")
  (testing "-3 - -4"))
