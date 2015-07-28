# Normalization
Database normalization in Prolog

The library also includes a main predicate that normalizes an example database.

Example output:
```sh
$ ./norm.pl
BCNF Decomp: [[cid,commenttext,commenttime,pid],[cid,posttext,posttime,posttitle,uid],[email,password,role,uid]]
3NF Synthesis: [[cid,commenttext,commenttime,uid,pid],[cid,uid],[pid,posttitle,posttext,posttime,uid],[role,email,password,uid]]
```
