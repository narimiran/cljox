(ns cljox.parser
  (:require [cljox.ast :as ast]
            [cljox.error :as err]))


;; Grammar
;; =======
;;

;; program        → declaration* EOF ;
;;
;; declaration    → classDecl
;;                | funDecl
;;                | varDecl
;;                | statement ;
;;
;; classDecl      → "class" IDENTIFIER "{" function* "}" ;
;; funDecl        → "fun" function ;
;;
;; function       → IDENTIFIER "(" parameters? ")" block ;
;;
;; parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
;;
;; varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
;;
;; statement      → exprStmt
;;                | forStmt
;;                | ifStmt
;;                | printStmt
;;                | returnStmt
;;                | whileStmt
;;                | block ;
;;
;; exprStmt       → expression ";" ;
;; forStmt        → "for" "(" ( varDecl | exprStmt | ";")
;;                  expression? ";"
;;                  expression? ")" statement ;
;; ifStmt         → "if" "(" expression ")" statement
;;                ( "else" statement )? ;
;; printStmt      → "print" expression ";" ;
;; returnStmt     → "return" expression? ";" ;
;; whileStmt      → "while" "(" expression ")" statement ;
;; block          → "{" declaration* "}" ;
;;
;; expression     → assignment ;
;; assignment     → IDENTIFIER "=" assignment
;;                | logic_or ;
;; logic_or       → logic_and ( "or" logic_and )* ;
;; logic_and      → equality ( "and" equality )* ;
;;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary | call ;
;; call           → primary ( "(" arguments? ")" )* ;
;; arguments      → expression ( "," expression )* ;
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
       (if (= clojure.lang.PersistentHashSet (type expected))
         (expected (current-type parser))
         (= expected (current-type parser)))))


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
  (if (matches? parser expected)
    (advance parser)
    (throw-error parser msg)))


(declare declaration)
(declare expression)
(declare statement)


(defn- primary [parser]
  (cond
    (matches? parser :false) (add-literal parser false)
    (matches? parser :true)  (add-literal parser true)
    (matches? parser :nil)   (add-literal parser nil)

    (matches? parser #{:number :string})
    (add-literal parser (:literal (current-token parser)))

    (matches? parser :identifier)
    (add-identifier parser (current-token parser))

    (matches? parser :left-paren)
    (let [mid (expression (advance parser))
          r-par (consume mid :right-paren "expected ')' after expression")]
      (add-expr r-par (ast/grouping (:expr mid))))

    :else (throw-error parser "expected expression")))

(defn- finish-call [parser calee]
  (loop [parser parser
         args []]
    (if (matches? parser :right-paren)
      (add-expr (advance parser) (ast/call calee (current-token parser) args))
      (let [parser' (expression parser)
            parser'' (if (>= (count args) 255)
                       (add-error parser' "cannot have more than 255 arguments")
                       parser')
            args' (conj args (:expr parser''))]
        (if (matches? parser'' :comma)
          (recur (advance parser'') args')
          (let [r-par (consume parser'' :right-paren "expect ')' after arguments")]
            (add-expr r-par (ast/call calee (current-token parser'') args'))))))))

(defn- call [parser]
  (loop [parser (primary parser)]
    (if (matches? parser :left-paren)
      (recur (finish-call (advance parser) (:expr parser)))
      parser)))

(defn- unary [parser]
  (if (matches? parser #{:bang :minus})
    (let [operator (current-token parser)
          right (unary (advance parser))]
      (add-expr right (ast/unary operator (:expr right))))
    (call parser)))


(defn- binary-op
  ([parser op-method token-types]
   (binary-op parser ast/binary op-method token-types))
  ([parser ast-func op-method token-types]
   (loop [left (op-method parser)]
     (if (matches? left token-types)
       (let [operator (current-token left)
             right (op-method (advance left))]
         (recur (add-expr right (ast-func (:expr left) operator (:expr right)))))
       left))))

(defn- factor [parser]
  (binary-op parser unary #{:slash :star}))

(defn- term [parser]
  (binary-op parser factor #{:minus :plus}))

(defn- comparison [parser]
  (binary-op parser term #{:greater :greater-equal :less :less-equal}))

(defn- equality [parser]
  (binary-op parser comparison #{:bang-equal :equal-equal}))

(defn- logic-and [parser]
  (binary-op parser ast/logical equality #{:and}))

(defn- logic-or [parser]
  (binary-op parser ast/logical logic-and #{:or}))

(defn- assignment [parser]
  (let [left (logic-or parser)
        expr (:expr left)]
    (if (matches? left :equal)
      (let [right (advance left)
            value (assignment right)]
        (if (= :variable (:type expr))
          (add-expr value (ast/assignment (:token expr) (:expr value)))
          (throw-error left "invalid assignment target")))
      left)))

(defn- expression [parser]
  (assignment parser))

(defn- var-decl [parser]
  (let [name (consume parser :identifier "expected variable name")
        has-init? (matches? name :equal)
        initializer (if has-init?
                      (expression (advance name))
                      name)
        semicolon (consume initializer :semicolon "expected ';' after value")]
    (add-expr semicolon
              (ast/var-decl (current-token parser)
                            (when has-init? (:expr initializer))))))

(defn- expression-stmt [parser]
  (let [value (expression parser)
        semi (consume value :semicolon "expected ';' after expression")]
    (add-expr semi (ast/expr-stmt (:expr value)))))

(defn- print-stmt [parser]
  (let [value (expression (advance parser))
        semi (consume value :semicolon "expected ';' after value")]
    (add-expr semi (ast/print-stmt (:expr value)))))

(defn- if-stmt [parser]
  (let [l-par (consume (advance parser) :left-paren "expected '(' after 'if'")
        condition (expression l-par)
        r-par (consume condition :right-paren "expected ')' after if condition")
        then-branch (statement r-par)
        else-branch (when (matches? then-branch :else)
                      (statement (advance then-branch)))]
    (add-expr (or else-branch then-branch)
              (ast/if-stmt (:expr condition) (:expr then-branch) (:expr else-branch)))))

(defn- return-stmt [parser]
  (let [kword (current-token parser)
        parser' (advance parser)
        val-pars (if (matches? parser' :semicolon)
                   (assoc parser' :expr nil)
                   (expression parser'))
        semi (consume val-pars :semicolon "expected ';' after return value")]
    (add-expr semi (ast/return-stmt kword (:expr semi)))))

(defn- while-stmt [parser]
  (let [l-par (consume (advance parser) :left-paren "expected '(' after 'while'")
        condition (expression l-par)
        r-par (consume condition :right-paren "expected ')' after condition")
        body (statement r-par)]
    (add-expr body
              (ast/while-stmt (:expr condition) (:expr body)))))

(defn- for-loop [parser]
  (let [l-par (consume (advance parser) :left-paren "expected '(' after 'for'")
        init  (cond
                (matches? l-par :semicolon) (advance (add-expr l-par nil))
                (matches? l-par :var) (var-decl (advance l-par))
                :else (expression-stmt l-par))
        cnd   (if (matches? init :semicolon)
                (add-literal init true)
                (expression-stmt init))
        incr  (if (matches? cnd #{:right-paren})
                (add-expr cnd nil)
                (expression cnd))
        r-par (consume incr :right-paren "expected ')' after 'for' clause")
        body  (statement r-par)
        [init-expr cnd-expr incr-expr body-expr] (map :expr [init cnd incr body])]
    (cond->> body-expr
      incr-expr (#(ast/block [% incr-expr]))
      true      (ast/while-stmt cnd-expr)
      init-expr (#(ast/block [init-expr %]))
      true      (add-expr body))))


(defn- block [parser]
  (loop [parser parser
         stmts []]
    (if (or (at-end? parser)
            (matches? parser :right-brace))
      (let [parser' (consume parser :right-brace "expected '}' after a block")]
        (add-expr parser' (ast/block stmts)))
      (let [parser' (declaration parser)
            stmts' (conj stmts (:expr parser'))]
        (recur parser' stmts')))))

(defn- parameters [parser]
  (if (matches? parser :right-paren)
    (assoc (advance parser) :expr [])
    (loop [parser parser
           params []]
      (let [parser' (consume parser :identifier "expected parameter name")
            parser'' (if (>= (count params) 255)
                       (add-error parser' "cannot have more than 255 parameters")
                       parser')
            params' (conj params (current-token parser))]
        (if (matches? parser'' :comma)
          (recur (advance parser'') params')
          (let [r-par (consume parser'' :right-paren "expected ')' after parameters")]
            (assoc r-par :expr params')))))))

(defn- func-decl [parser kind]
  (let [name-token (current-token parser)
        kind-name (name kind)
        fname (consume parser :identifier (str "expected " kind-name " name"))
        l-par (consume fname :left-paren (str "expected '(' after " kind-name " name"))
        params (parameters l-par)
        l-brace (consume params :left-brace (str "expected '{' before " kind-name " body"))
        body (block l-brace)
        stmts (-> body :expr :stmts)]
    (add-expr body (ast/func-decl name-token (:expr params) stmts))))


(defn- class-decl [parser]
  (let [name-token (current-token parser)
        cl-name (consume parser :identifier "expected class name")
        l-brace (consume cl-name :left-brace "expected '{' before class body")]
    (loop [p l-brace
           methods []]
      (if (or (at-end? p) (matches? p :right-brace))
        (let [r-brace (consume p :right-brace "expected '}' after class body")]
          (add-expr r-brace (ast/class-stmt name-token methods)))
        (let [p' (func-decl p :method)]
          (recur p' (conj methods (:expr p'))))))))


(defn- statement [parser]
  (cond
    (matches? parser :for) (for-loop parser)
    (matches? parser :if) (if-stmt parser)
    (matches? parser :left-brace) (block (advance parser))
    (matches? parser :print) (print-stmt parser)
    (matches? parser :return) (return-stmt parser)
    (matches? parser :while) (while-stmt parser)
    :else (expression-stmt parser)))

(defn- synchronize [parser]
  (cond
    (at-end? parser) parser
    (matches? parser :semicolon) (advance parser)
    (matches? parser #{:class :fun :var :for :if :while :print :return}) parser
    :else (recur (advance parser))))


(defn- declaration [parser]
  (try
    (cond
      (matches? parser :class) (class-decl (advance parser))
      (matches? parser :fun) (func-decl (advance parser) :func)
      (matches? parser :var) (var-decl (advance parser))
      :else (statement parser))
    (catch clojure.lang.ExceptionInfo e
      (synchronize (ex-data e)))))



(defn parse [tokens]
  (loop [parser (new-parser tokens)]
    (if (at-end? parser)
      ((juxt :stmts :errors) parser)
      (recur
       (let [parser' (declaration parser)]
         (update parser' :stmts conj (:expr parser')))))))










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
  (testing "var foo = 3; foo = 1; print foo;")

  (testing "var a = 3; {var a = 4; print a;} print a;")

  (testing "if (true) 3; else 4;")
  (testing "if (true) 3; 4;")
  (testing "if (7) 3; else 4;")

  (testing "true and false;")
  (testing "false and true;")
  (testing "false and true or false;")
  (testing "false and (true or false);")

  (testing "while (3 < 5) print 4; print 6;")
  (testing "while (3 < 5) {1+ 2; 3 + 4;} 5 + 4;")
  (testing "for (var a = 0;\n a < 3;\n a = a + 1) print a; ")
  (testing "for (var a = 0;\n a < 3;\n a = a + 1) print a;")
  (testing "var a = 0; for (var b = 1; a < 3; a = a + 1) print a;")
  (testing "var a = 0; for (a; a < 3; a = a + 1) { print a+1; }")
  (testing "var a = 0; for (; a < 3; a = a + 1) { print a+1; }")

  (testing "average(1, 2, 3);")
  (testing "average(1 + 2, 3);")
  (testing "average();")
  (testing "average(1)(2)(3);")
  (testing "clock();")

  (testing "fun foo(a, b) {print a + b;}")
  (testing "fun foo(a, b) {return a + b;}"))
