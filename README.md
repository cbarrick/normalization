# Normalization
Database normalization in Prolog

This script implements the two most common algorithms for database normalization, BCNF decomposition and 3NF synthesis. It was written as an exercise while studying for an exam in a databases class.

The `norm_example` predicate normalizes an example database given in the textbook _Database Systems_ by Kifer, Bernstein, and Lewis.

Example output:

    $ ./norm.pl
    # BCNF Decomposition
    - Schema:	`[[a,d,e],[a,f,h],[b,c,f,g]]`
    - Dependencies:	`[ ([a]->[d,e]), ([b,h]->[c,f,g]), ([f]->[a,h])]`
    - Decomposition:
    ```
    [a,b,c,d,e,f,g,h]
    ├── violation: [a]->[d,e]
    ├── [a,d,e]
    └── [a,b,c,f,g,h]
        ├── violation: [f]->[a,h]
        ├── [a,f,h]
        └── [b,c,f,g]
    ```

    # 3NF Synthesis
    - Schema:	`[[a,d,e],[b,c,f,g,h],[a,f,h]]`
    - Dependencies:	`[ ([a]->[d,e]), ([b,h]->[c,f,g]), ([f]->[a,h])]`
    - Global Key:	`[b,c,f,g,h]`
