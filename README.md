# Normalization
Database normalization in Prolog

This script implements the two most common algorithms for database normalization, BCNF decomposition and 3NF synthesis. This script was written as an exercise while studying for an exam in a databases class.

The `main` predicate normalizes a simple database for a blog application that some classmates and I created as a term project in that class.

Example output:
```sh
$ BCNF Decomp:
- table: [cid,commenttext,commenttime,email,password,pid,posttext,posttime,posttitle,role,uid]
  - violation: [pid] -> [email,password,pid,posttext,posttime,posttitle,role,uid]
  - table: [email,password,pid,posttext,posttime,posttitle,role,uid]
    - violation: [uid] -> [email,password,role,uid]
    - table: [email,password,role,uid]
    - table: [pid,posttext,posttime,posttitle,uid]
  - table: [cid,commenttext,commenttime,pid]
Result: [[cid,commenttext,commenttime,pid],[email,password,role,uid],[pid,posttext,posttime,posttitle,uid]]

3NF Synthesis:
Minimal cover:
	[ ([cid]->[commenttext,commenttime,pid,uid]), ([pid]->[posttext,posttime,posttitle,uid]), ([uid]->[email,password,role])]
Initial Tables:
	[[cid,commenttext,commenttime,pid,uid],[pid,posttext,posttime,posttitle,uid],[email,password,role,uid]]
Global Key:
	[cid,commenttext,commenttime,pid,uid]
Tables:
	[[cid,commenttext,commenttime,pid,uid],[pid,posttext,posttime,posttitle,uid],[email,password,role,uid]]
```
