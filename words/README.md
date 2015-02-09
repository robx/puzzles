A solver for a word placement puzzle.

Rules
-----

Place the given words in the grid in any straight direction so that they
form a loop. I.e., the start and end letter of each word is also the start
or end letter of precisely one other word. Given letters must be part of
the solution.

Example
-------

Puzzle:

```
+----+
|    |  AN   WIN
|   E|  AUNT WET
|N   |  TINT
|    |
+----+
```

Solution:

```
+----+
|AN T|
|U IE|
|NN W|
|T   |
+----+
```
