(ns cljox.parser
  (:require [cljox.error :as err]
            [cljox.interpreter :as i])
  (:import [cljox.interpreter Variable GetExpr]))


;; Grammar
;; =======
;;
;;
;; program        → declaration* EOF ;
;;
;; declaration    → classDecl
;;                | funDecl
;;                | varDecl
;;                | statement ;
;;
;; classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
;;                 "{" function* "}" ]));
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
;; assignment     → ( call "." )? IDENTIFIER "=" assignment
;;                | logic_or ;
;; logic_or       → logic_and ( "or" logic_and )* ;
;; logic_and      → equality ( "and" equality )* ;
;;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary | call ;
;; call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
;; arguments      → expression ( "," expression )* ;
;; primary        → "true" | "false" | "nil" | "this"
;;                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
;;                | "super" "." IDENTIFIER ;



(defn- new-parser [tokens]
  {:tokens  tokens
   :errors  []
   :stmts   []
   :expr    nil})


(defn- current-token [{:keys [tokens]}]
  (first tokens))

(defn- tok-type [parser]
  (:type (current-token parser)))

(defn- advance [m]
  (update m :tokens rest))

(defn- tok-match? [expected parser]
  (= expected (tok-type parser)))

(defn- at-end? [parser]
  (tok-match? :eof parser))

(defn- add-expr [parser expr]
  (assoc parser :expr expr))

(defn- add-literal [parser lit]
  (-> parser
      (add-expr (i/->Literal lit))
      advance))

(defn- add-identifier [parser token]
  (-> parser
      (add-expr (i/->Variable token))
      advance))

(defn- add-error [parser msg]
  (let [token (current-token parser)
        err (err/parsing-error token msg)]
    (update parser :errors conj err)))

(defn- throw-error [parser msg]
  (let [parser' (add-error parser msg)]
    (throw (ex-info "parsing error" parser'))))

(defn- consume [parser expected msg]
  (if (tok-match? expected parser)
    (advance parser)
    (throw-error parser msg)))


(declare declaration)
(declare expression)
(declare statement)


(defn- primary [parser]
  (let [tok     (current-token parser)
        parser' (advance parser)]
    (case (:type tok)
      :false      (add-literal parser false)
      :true       (add-literal parser true)
      :nil        (add-literal parser nil)
      (:number :string) (add-literal parser (:literal tok))
      :super      (let [dot-par   (consume parser' :dot "expected '.' after 'super'")
                        method    (current-token dot-par)
                        ident-par (consume dot-par :identifier "expected superclass method name")]
                    (add-expr ident-par (i/->Super tok method)))
      :this       (add-expr parser' (i/->This tok))
      :identifier (add-identifier parser tok)
      :left-paren (let [mid   (expression parser')
                        r-par (consume mid :right-paren "expected ')' after expression")]
                    (add-expr r-par (i/->Grouping (:expr mid))))
      (throw-error parser "expected expression"))))

(defn- finish-call [parser calee]
  (loop [parser parser
         args []]
    (let [tok (current-token parser)]
      (if (tok-match? :right-paren parser)
        (add-expr (advance parser) (i/->Call calee tok args))
        (let [parser' (expression parser)
              parser'' (if (>= (count args) 255)
                         (add-error parser' "cannot have more than 255 arguments")
                         parser')
              args' (conj args (:expr parser''))]
          (if (tok-match? :comma parser'')
            (recur (advance parser'') args')
            (let [r-par (consume parser'' :right-paren "expect ')' after arguments")]
              (add-expr r-par (i/->Call calee (current-token parser'') args')))))))))

(defn- call [parser]
  (loop [parser (primary parser)]
    (let [parser' (advance parser)]
      (case (tok-type parser)
        :left-paren (recur (finish-call parser' (:expr parser)))
        :dot (let [name-token (current-token parser')
                   parser'' (consume parser' :identifier "expect property name after '.'")]
               (recur (add-expr parser'' (i/->GetExpr (:expr parser'') name-token))))
        parser))))

(defn- unary [parser]
  (if (#{:bang :minus} (tok-type parser))
    (let [operator (current-token parser)
          right (unary (advance parser))]
      (add-expr right (i/->Unary operator (:expr right))))
    (call parser)))


(defn- binary-op
  ([parser op-method token-types]
   (binary-op parser i/->Binary op-method token-types))
  ([parser ast-func op-method token-types]
   (loop [left (op-method parser)]
     (if (token-types (tok-type left))
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
  (binary-op parser i/->Logical equality #{:and}))

(defn- logic-or [parser]
  (binary-op parser i/->Logical logic-and #{:or}))

(defn- assignment [parser]
  (let [left (logic-or parser)
        expr (:expr left)]
    (if (tok-match? :equal left)
      (let [right (advance left)
            value (assignment right)]
        (cond
          (instance? Variable expr) (add-expr value (i/->Assignment
                                                     (:token expr)
                                                     (:expr value)))
          (instance? GetExpr expr) (add-expr value (i/->SetExpr
                                                    (:object expr)
                                                    (:token expr)
                                                    (:expr value)))
          :else (throw-error left "invalid assignment target")))
      left)))

(defn- expression [parser]
  (assignment parser))

(defn- var-decl [parser]
  (let [vname (consume parser :identifier "expected variable name")
        has-init? (tok-match? :equal vname)
        initializer (if has-init?
                      (expression (advance vname))
                      vname)
        semicolon (consume initializer :semicolon "expected ';' after value")]
    (add-expr semicolon
              (i/->VarDecl (current-token parser)
                           (when has-init? (:expr initializer))))))

(defn- expression-stmt [parser]
  (let [value (expression parser)
        semi (consume value :semicolon "expected ';' after expression")]
    (add-expr semi (i/->ExprStmt (:expr value)))))

(defn- print-stmt [parser]
  (let [value (expression parser)
        semi (consume value :semicolon "expected ';' after value")]
    (add-expr semi (i/->PrintStmt (:expr value)))))

(defn- if-stmt [parser]
  (let [l-par (consume parser :left-paren "expected '(' after 'if'")
        condition (expression l-par)
        r-par (consume condition :right-paren "expected ')' after if condition")
        then-branch (statement r-par)
        else-branch (when (tok-match? :else then-branch)
                      (statement (advance then-branch)))]
    (add-expr (or else-branch then-branch)
              (i/->IfStmt (:expr condition) (:expr then-branch) (:expr else-branch)))))

(defn- return-stmt [parser kword]
  (let [val-pars (if (tok-match? :semicolon parser)
                   (assoc parser :expr nil)
                   (expression parser))
        semi (consume val-pars :semicolon "expected ';' after return value")]
    (add-expr semi (i/->ReturnStmt kword (:expr semi)))))

(defn- while-stmt [parser]
  (let [l-par (consume parser :left-paren "expected '(' after 'while'")
        condition (expression l-par)
        r-par (consume condition :right-paren "expected ')' after condition")
        body (statement r-par)]
    (add-expr body
              (i/->WhileStmt (:expr condition) (:expr body)))))

(defn- for-loop [parser]
  (let [l-par (consume parser :left-paren "expected '(' after 'for'")
        init  (case (tok-type l-par)
                :semicolon (advance (add-expr l-par nil))
                :var (var-decl (advance l-par))
                (expression-stmt l-par))
        cnd   (if (tok-match? :semicolon init)
                (add-literal init true)
                (expression-stmt init))
        incr  (if (tok-match? :right-paren cnd)
                (add-expr cnd nil)
                (expression cnd))
        r-par (consume incr :right-paren "expected ')' after 'for' clause")
        body  (statement r-par)
        [init-expr cnd-expr incr-expr body-expr] (map :expr [init cnd incr body])]
    (cond->> body-expr
      incr-expr (#(i/->Block [% incr-expr]))
      true      (i/->WhileStmt cnd-expr)
      init-expr (#(i/->Block [init-expr %]))
      true      (add-expr body))))


(defn- block [parser]
  (loop [parser parser
         stmts []]
    (if (or (at-end? parser)
            (tok-match? :right-brace parser))
      (let [parser' (consume parser :right-brace "expected '}' after a block")]
        (add-expr parser' (i/->Block stmts)))
      (let [parser' (declaration parser)
            stmts' (conj stmts (:expr parser'))]
        (recur parser' stmts')))))

(defn- parameters [parser]
  (if (tok-match? :right-paren parser)
    (assoc (advance parser) :expr [])
    (loop [parser parser
           params []]
      (let [parser' (consume parser :identifier "expected parameter name")
            parser'' (if (>= (count params) 255)
                       (add-error parser' "cannot have more than 255 parameters")
                       parser')
            params' (conj params (current-token parser))]
        (if (tok-match? :comma parser'')
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
    (add-expr body (i/->FuncDecl name-token (:expr params) stmts))))


(defn- class-decl [parser]
  (let [name-token (current-token parser)
        cl-name (consume parser :identifier "expected class name")
        cl-name' (advance cl-name)
        super? (tok-match? :less cl-name)
        super (if super?
                (consume cl-name' :identifier "expected superclass name")
                cl-name)
        superclass (when super? (i/->Variable (current-token cl-name')))
        l-brace (consume super :left-brace "expected '{' before class body")]
    (loop [p l-brace
           methods []]
      (if (or (at-end? p) (tok-match? :right-brace p))
        (let [r-brace (consume p :right-brace "expected '}' after class body")]
          (add-expr r-brace (i/->ClassStmt name-token superclass methods)))
        (let [p' (func-decl p :method)]
          (recur p' (conj methods (:expr p'))))))))


(defn- statement [parser]
  (let [parser' (advance parser)]
    (case (tok-type parser)
      :for        (for-loop parser')
      :if         (if-stmt parser')
      :left-brace (block parser')
      :print      (print-stmt parser')
      :return     (return-stmt parser' (current-token parser))
      :while      (while-stmt parser')
      (expression-stmt parser))))

(defn- synchronize [parser]
  (let [ttype (tok-type parser)]
    (cond
      (at-end? parser) parser
      (= :semicolon ttype) (advance parser)
      (#{:class :fun :var :for :if :while :print :return} ttype) parser
      :else (recur (advance parser)))))

(defn- declaration [parser]
  (let [parser' (advance parser)]
    (try
      (case (tok-type parser)
        :class (class-decl parser')
        :fun   (func-decl parser' :func)
        :var   (var-decl parser')
        (statement parser))
      (catch clojure.lang.ExceptionInfo e
        (synchronize (ex-data e))))))



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
