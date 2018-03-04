# datalog

A simple, unoptimized, purely functional Datalog implementation, inspired by miniKanren.

Includes naive, semi-naive, and backward-chaining implementations.

## References

Ceri, Stefano, Georg Gottlob, and Letizia Tanca. "What you always wanted to know about Datalog (and never dared to ask)." IEEE transactions on knowledge and data engineering 1.1 (1989): 146-166.

Ullman, Principles of Database and Knowledge-Base Systems Volume I, ch. 3

http://minikanren.org/

## Syntax

### Facts (atoms)
Entity-Attribute-Value (EAV) triples.

Example:
```clojure
[Zeus :father Cronus]
```
### Variables (lvars)
Symbols that start with a '?'.

Example:
```clojure
?x
```

### Rules
Vectors of atoms. The first element is the head of the rule and the rest are the body.

Example:
```clojure
[[?x :grandparent ?y] [?x :parent ?z] [?z :parent ?y]]
```
"x has grandparent y, if x has parent z, and z has parent y."

## Usage

See datalog_test.clj.

## License

Copyright © 2018 Austin Haas

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
