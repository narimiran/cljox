# Cljox

A Clojure implementation of `jlox` interpreter for the Lox programming language from the [Crafting Interpreters](https://craftinginterpreters.com/) book by Bob Nystrom.

Instead of the original Java-style, the code here is written in a functional style, with some ideas ~~stolen~~ inspired by [Ceronman's Cloxure implementation](https://github.com/ceronman/cloxure).
This was my first time to use Clojure's protocols, ~~and multimethods are such a no-brainer\*~~ instead of the visitor pattern. (\*A multimethod version is available in earlier commits.)

I tried to keep the git history as clean as possible (nobody will ever know about the existence of all that fighting with subtle bugs, interactive rebases, fixups, etc., right?), with each git commit stating the chapter of the book it is related to, in case somebody stumbles upon this repo and wants to have a Clojure code to read in parallel with the book content.\
After the last chapter, there are commits dealing with performance.


## Performance

~~The execution speed was not my goal, and it shows.~~\
In 2026, I decided to re-visit this project and, thanks to the [great writeup](https://raghavio.com/posts/implementing-lox-in-clojure/) by [raghavio](https://github.com/raghavio), the current performance is two orders of magnitude better than in the original implementation.

Some numbers from the recent commits:
- calculating 30th Fibonacci number ([fib.lox](https://github.com/munificent/craftinginterpreters/blob/master/test/benchmark/fib.lox), lower is better): 154 s -> 4.6 s -> 4.2 s -> 2.8 s
- [zoo_batch.lox](https://github.com/munificent/craftinginterpreters/blob/master/test/benchmark/zoo_batch.lox) (higher is better): 0.59 M, 99 -> 7.56 M, 1260 -> 8.77 M, 1462 -> 11.14 M, 1857



## Running

After I made a switch to a much faster implementation of `ReturnException`, you need to compile its class before running any code:

```
clojure -T:build compile-return-ex
```

After that, you can:

- start an interpreter:
  ```
  clj -M -m cljox.core
  ```

- run a file:
  ```
  clj -M -m cljox.core <path-to-file>
  ```


Alternatively, you can build a `jar` of a whole project and run it via Java:

```
clojure -T:build uber

java -jar target/cljox.jar [<path-to-file>]
```
