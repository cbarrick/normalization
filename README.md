# Normalization
Database normalization in Prolog

This script implements the two most common algorithms for database normalization, BCNF decomposition and 3NF synthesis. This script was written as an exercise while studying for an exam in a databases class.

The `main` predicate normalizes a simple database for a blog application that some classmates and I created as a term project in that class.

Example output:
```sh
$ ./norm.pl
BCNF Decomp:
- table: [cid,commenttext,commenttime,email,password,pid,posttext,posttime,posttitle,role,uid]
  - violation: [cid] -> [commenttext,commenttime,pid]
  - table: [cid,commenttext,commenttime,pid]
  - table: [cid,email,password,posttext,posttime,posttitle,role,uid]
    - violation: [uid] -> [email,password,role]
    - table: [email,password,role,uid]
    - table: [cid,posttext,posttime,posttitle,uid]
Result: [[cid,commenttext,commenttime,pid],[cid,posttext,posttime,posttitle,uid],[email,password,role,uid]]

3NF Synthesis:
Minimal cover:
	[ ([cid]->[commenttext,commenttime,pid]), ([pid]->[posttext,posttime,posttitle]), ([uid]->[email,password,role])]
Initial Tables:
	[[cid,commenttext,commenttime,pid],[pid,posttext,posttime,posttitle],[email,password,role,uid]]
Global Key:
	[cid,uid]
Tables:
	[[cid,commenttext,commenttime,pid],[cid,uid],[email,password,role,uid],[pid,posttext,posttime,posttitle]]
```
