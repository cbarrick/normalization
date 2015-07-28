# Normalization
Database normalization in Prolog

The library also includes a main predicate that normalizes an example database.

Example output:
```sh
$ ./norm.pl
BCNF Decomp: [[cid,commenttext,commenttime],[cid,pid,uid],[email,password,role,uid],[pid,posttext,posttime,posttitle]]
3NF Synthesis: [[cid,commenttext,commenttime],[cid,email,password,role,uid,pid],[pid,posttitle,posttext,posttime],[role,email,password,uid]]
```
