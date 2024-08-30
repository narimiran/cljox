# Cljox

A Clojure implementation of `jlox` interpreter for the Lox programming language from the [Crafting Interpreters](https://craftinginterpreters.com/) book by Bob Nystrom.

Instead of the original Java-style, the code here is written in a functional style, with some ideas ~~stolen~~ inspired by [Ceronman's Cloxure implementation](https://github.com/ceronman/cloxure).
This was my first time to use Clojure's protocols, and multimethods are such a no-brainer instead of the visitor pattern.

I tried to keep the git history as clean as possible (nobody will ever know about the existence of all that fighting with subtle bugs, interactive rebases, fixups, etc., right?), with each git commit stating the chapter of the book it is related to, in case somebody stumbles upon this repo and wants to have a Clojure code to read in parallel with the book content.


A note about performance:
The execution speed was not my goal, and it shows.
Run the benchmarks at your own risk.
