(ns build
  (:require [clojure.tools.build.api :as b]))


(def main-ns 'cljox.core)

(def class-dir "target/classes")
(def uber-file "target/cljox.jar")

(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile-return-ex [_]
  (b/compile-clj {:basis basis
                  :src-dirs ["src/cljox/return_exception.clj"]
                  :class-dir class-dir}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src"]
               :target-dir class-dir})
  (compile-return-ex nil)
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main main-ns}))
