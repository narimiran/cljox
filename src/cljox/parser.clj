(ns cljox.parser
  (:require [cljox.ast :as ast]
            [cljox.error :as err]))


;; Grammar
;; =======
;;

;; program        → declaration* EOF ;
;;
;; declaration    → varDecl
;;                | statement ;
;;
;; varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
;;
;; statement      → exprStmt
;;                | printStmt ;
;;
;; exprStmt       → expression ";" ;
;; printStmt      → "print" expression ";" ;
;;
;; expression     → assignment ;
;; assignment     → IDENTIFIER "=" assignment
;;                | equality ;
;;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary
;;                | primary ;
;; primary        → "true" | "false" | "nil"
;;                | NUMBER | STRING
;;                "(" expression ")"
;;                | IDENTIFIER ;



(defn- new-parser [tokens]
  {:tokens  tokens
   :errors  []
   :stmts   []
   :expr    nil
   :current 0})


(defn- current-token [{:keys [tokens current]}]
  (nth tokens current))

(defn- current-type [parser]
  (:type (current-token parser)))

(defn- advance [m]
  (update m :current inc))

(defn- at-end? [parser]
  (= :eof (current-type parser)))

(defn- matches? [parser expected]
  (and (not (at-end? parser))
       (expected (current-type parser))))


(defn- add-expr [parser expr]
  (assoc parser :expr expr))

(defn- add-literal [parser lit]
  (-> parser
      (add-expr (ast/literal lit))
      advance))

(defn- add-identifier [parser token]
  (-> parser
      (add-expr (ast/variable token))
      advance))

(defn- add-error [parser msg]
  (let [token (current-token parser)
        err (err/parsing-error token msg)]
    (update parser :errors conj err)))

(defn- throw-error [parser msg]
  (let [parser' (add-error parser msg)]
    (throw (ex-info "parsing error" parser'))))

(defn- consume [parser expected msg]
  (if (matches? parser #{expected})
    (advance parser)
    (throw-error parser msg)))


(declare expression)

(defn- primary [parser]
  (cond
    (matches? parser #{:false}) (add-literal parser false)
    (matches? parser #{:true})  (add-literal parser true)
    (matches? parser #{:nil})   (add-literal parser nil)

    (matches? parser #{:number :string})
    (add-literal parser (:literal (current-token parser)))

    (matches? parser #{:identifier})
    (add-identifier parser (current-token parser))

    (matches? parser #{:left-paren})
    (let [mid (expression (advance parser))
          parser' (consume mid :right-paren "expected ')' after expression")]
      (add-expr parser' (ast/grouping (:expr mid))))

    :else (throw-error parser "expected expression")))


(defn- unary [parser]
  (if (matches? parser #{:bang :minus})
    (let [operator (current-token parser)
          right (unary (advance parser))]
      (add-expr right (ast/unary operator (:expr right))))
    (primary parser)))


(defn- binary-op [parser op-method token-types]
  (loop [left (op-method parser)]
    (if (matches? left token-types)
      (let [operator (current-token left)
            right (op-method (advance left))]
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

(defn- assignment [parser]
  (let [left (equality parser)
        expr (:expr left)]
    (if (matches? left #{:equal})
      (let [right (advance left)
            value (assignment right)]
        (if (= :variable (:type expr))
          (add-expr value (ast/assignment (:token expr) (:expr value)))
          (throw-error left "invalid assignment target")))
      left)))

(defn- expression [parser]
  (assignment parser))

(defn- print-stmt [parser]
  (let [value (expression (advance parser))
        parser' (consume value :semicolon "expected ';' after value")]
    (add-expr parser' (ast/print-stmt (:expr value)))))

(defn- expression-stmt [parser]
  (let [value (expression parser)
        parser' (consume value :semicolon "expected ';' after expression")]
    (add-expr parser' (ast/expr-stmt (:expr value)))))

(defn- statement [parser]
  (if (matches? parser #{:print})
    (print-stmt parser)
    (expression-stmt parser)))

(defn- var-declaration [parser]
  (let [name (consume parser :identifier "expected variable name")
        has-init? (matches? name #{:equal})
        initializer (if has-init?
                      (expression (advance name))
                      name)
        semicolon (consume initializer :semicolon "expected ';' after value")]
    (add-expr semicolon
              (ast/var-decl (current-token parser)
                            (when has-init? (:expr initializer))))))


(defn- declaration [parser]
  (if (matches? parser #{:var})
    (var-declaration (advance parser))
    (statement parser)))




(defn- synchronize [parser]
  (cond
    (at-end? parser) parser
    (matches? parser #{:semicolon}) (advance parser)
    (matches? parser #{:class :fun :var :for :if :while :print :return}) parser
    :else (recur (advance parser))))


(defn parse [tokens]
  (loop [parser (new-parser tokens)]
    (if (at-end? parser)
      ((juxt :stmts :errors) parser)
      (recur
       (try
         (let [parser' (declaration parser)]
           (update parser' :stmts conj (:expr parser')))
         (catch clojure.lang.ExceptionInfo e
           (synchronize (ex-data e))))))))










(comment
  (require '[cljox.scanner :as scanner])
  (require '[cljox.ast-printer :as ap])

  (defn testing [source]
    (let [[tokens errors] (scanner/scan-tokens source)]
      (if (seq errors)
        errors
        (let [[stmts errors] (parse tokens)]
          (if (seq errors)
            errors
            (map ap/pprint stmts))))))

  (testing "print 1 + 2;")
  (testing "1 + 2; 3+ 5;")
  (testing "print 1 * -3;")
  (testing "1 + - 3 * 4;")
  (testing "1 + * 3;")
  (testing "2 * (3 + 4);")
  (testing "2 * (3 + * 4) + \"ab;")
  (testing "c++;")
  (testing "\"c\"+\"d\";")
  (testing "0 + nil;")
  (testing "0 == 1 != 2;")
  (testing "\"abc\" != \"def\" == \"ghi\";")
  (testing "= 3;")
  (testing "== 3;")
  (testing "3 + \"ab;")
  (testing "3 + \"ab\ncd;")
  (testing "3 + (4;")
  (testing "3 + )4;")
  (testing "-3 - -4;")
  (testing "3; 4+4; print 5; 66; true == true;")
  (testing "var foo;")
  (testing "var foo = 3;")
  (testing "var foo")
  (testing "foo;")
  (testing "var foo = 3")
  (testing "var foo = 3 + 4 * 5; foo; 44;")
  (testing "var foo = 3; foo = 1;")
  (testing "var foo = 3; foo = 1; print foo;"))
