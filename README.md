# Normalization
Database normalization in Prolog

This script implements the two most common algorithms for database normalization, BCNF decomposition and 3NF synthesis. It was written as an exercise while studying for an exam in a databases class.

The `main` predicate normalizes an example database given in the textbook _Database Systems_ by Kifer, Bernstein, and Lewis.

Example output:
```
$ ./norm.pl
# BCNF Decomposition
Schema: [[a,d,e],[a,f,h],[b,c,f,g]]
Dependencies: [ ([a]->[d,e]), ([b,h]->[c,f,g]), ([f]->[a,h])]
Decomposition:
  - table: [a,b,c,d,e,f,g,h]
    - violation: [a] -> [d,e]
    - table: [a,d,e]
    - table: [a,b,c,f,g,h]
      - violation: [f] -> [a,h]
      - table: [a,f,h]
      - table: [b,c,f,g]

# 3NF Synthesis
Minimal cover:	[ ([a]->[d,e]), ([b,h]->[c,f,g]), ([f]->[a,h])]
Initial Tables:	[[a,d,e],[b,c,f,g,h],[a,f,h]]
Global Key:	[b,c,f,g,h]
```
