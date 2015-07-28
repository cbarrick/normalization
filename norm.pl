#!/usr/bin/env swipl -g main -t halt

:- use_module(library(ordsets)).


%! cover(+F, -Cover)
% Generates a normalized minimal cover of the functional dependencies `F`.
cover(F, Minimal) :-
	% normalize F, ensuring right-hand-sides are sorted
	findall(X->Y, (
		member(Z->Y, F),
		list_to_ord_set(Z, X)
	), NormalF),

	% expand the right-hand-sides into singletons
	setof(X->[Y], Z^(
		member(X->Z, NormalF),
		member(Y, Z)
	), Singleton),

	% eliminate extraneous rules
	findall(X->[Y], (
		select(X->[Y], Singleton, Rest),
		ord_union(X, [Y], XY),
		\+ (
			member(Key->_, Rest),
			closure(Key, Rest, Closure),
			ord_subset(XY, Closure)
		)
	), MinimalSingleton),

	% merge right-hand-sides, ensuring they are sorted
	findall(X->Y, (
		setof(Z, member(X->[Z], MinimalSingleton), Y)
	), Minimal).


%! flatten_fds(+F, -R)
% Flattens a set of functional dependencies `F` into a set of attributes `R`.
flatten_fds(F, R) :-
	setof(X, Lhs^Rhs^(
		member(Lhs->Rhs, F),
		(member(X, Lhs) ; member(X, Rhs))
	), R).


%! closure(+Attrs, +F, -Closure)
% True when `Closure` is the set of all functional dependants of `Attrs`, with
% respect to the functional dependencies `F`.
closure(Attrs, F, Closure) :-
	member(X->Y, F),
	ord_subset(X, Attrs),
	\+ ord_subset(Y, Attrs),
	!,
	ord_union(Attrs, Y, Next),
	closure(Next, F, Closure).

closure(Attrs, _, Attrs).


%! key(+Attrs, +F, -Key)
% True when `Key` is a key for the attributes `Attrs` with respect to the
% functional dependencies `F`.
key(Attrs, F, Key) :-
	select(_, Attrs, Rest),
	closure(Rest, F, Closure),
	ord_subset(Attrs, Closure),
	!,
	key(Rest, F, Key).

key(Attrs, _, Attrs).


%! bcnf(+R, +F)
% True when the relation `R` is in Boyce–Codd normal form with respect to the
% functional dependencies `F`.
bcnf(R, F) :-
	forall(member(X->Y, F), (
		ord_subset(X, R),
		ord_intersection(Y, R, [_|_])
	->
		closure(X, F, XClose),
		ord_subset(R, XClose)
	;
		true
	)).


%! bcnf_decomp(+F, -Decomp, -Plan)
% Generates a schema using BCNF decomposition with respect to the functional
% dependencies in `F`. `Plan` is a compound describing the decomposition
% process. Use `write_bcnf_decomp/1` to write the plan in a human format.
bcnf_decomp(F, Decomp, Plan) :-
	% a minimal cover isn't strictly needed, but the generated cover is
	% guaranteed to be sorted, so we can use ordered set operations.
	once(cover(F, Cover)),
	flatten_fds(Cover, R),
	bcnf_decomp_(R, Cover, Decomp, Plan).

bcnf_decomp_([], _, [[]], plan([])) :- !.
bcnf_decomp_([X], _, [[X]], plan([X])) :- !.
bcnf_decomp_([X,Y], _, [[X,Y]], plan([X,Y])) :- !.
bcnf_decomp_(R, F, [R], plan(R)) :- bcnf(R, F), !.
bcnf_decomp_(R, F, Decomp, plan(R, X->Y, Plan0, Plan1)) :-
	member(X->Y, F),
	ord_subset(X, R),
	ord_intersection(Y, R, YPrime),
	YPrime \= [],
	closure(X, F, XClose),
	\+ ord_subset(R, XClose),
	ord_union([X, YPrime], XY),
	ord_subtract(R, Y, RMinusY),
	debug(bcnf_decomp, "splitting ~w on ~w: ~w", [R, X->Y, [XY,RMinusY]]),
	bcnf_decomp_(XY, F, D0, Plan0),
	bcnf_decomp_(RMinusY, F, D1, Plan1),
	ord_union(D0, D1, Decomp).


%! write_bcnf_decomp(+Plan)
% Write the BCNF decomposition plan in a human format.
write_bcnf_decomp(Plan) :- write_bcnf_decomp_(Plan, 0).

write_bcnf_decomp_(plan(Leaf), Depth) :-
	write_bcnf_decomp_indent_(Depth),
	format("- table: ~w\n", [Leaf]).

write_bcnf_decomp_(plan(Attrs, X->Y, Left, Right), Depth) :-
	write_bcnf_decomp_indent_(Depth),
	format("- table: ~w\n", [Attrs]),
	write_bcnf_decomp_indent_(Depth+1),
	format("- violation: ~w -> ~w\n", [X,Y]),
	write_bcnf_decomp_(Left, Depth+1),
	write_bcnf_decomp_(Right, Depth+1).

write_bcnf_decomp_indent_(0) :- !.
write_bcnf_decomp_indent_(Depth) :-
	write("  "),
	Next is Depth - 1,
	write_bcnf_decomp_indent_(Next).




%! synthesis(+F, -Tables, -Plan)
% Generates a schema using 3NF synthesis with respect to the functional
% dependencies in `F`. `Plan` is a compound describing the synthesis
% process. Use `write_synthesis/1` to write the plan in a human format.
synthesis(F, Tables, plan(Cover, InitialTables, Key, Tables)) :-
	once(cover(F, Cover)),
	flatten_fds(Cover, R),
	findall(XY, (
		select(X->Y, F, Rest),
		ord_union(X, Y, XY),
		\+ (
			member(X1->Y1, Rest),
			ord_union(X1, Y1, Other),
			ord_subset(XY, Other)
		)
	), InitialTables),
	(
		member(Superkey, InitialTables),
		closure(Superkey, Cover, Closure),
		Closure = R,
		Key = Superkey,
		Tables = InitialTables
	->true;
		once(key(R, Cover, Key)),
		sort([Key|InitialTables], Tables)
	).


%! write_synthesis(+Plan)
% Write the 3NF synthesis plan in a human format.
write_synthesis(plan(Cover, InitialTables, Key, Tables)) :-
	format("Minimal cover:\n\t~w\n", [Cover]),
	format("Initial Tables:\n\t~w\n", [InitialTables]),
	format("Global Key:\n\t~w\n", [Key]),
	format("Tables:\n\t~w\n", [Tables]).


%! main(X)
% Run a bcnf decomposition on our project data.
main :-
	F = [
		[uid] -> [role, email, password],
		[pid] -> [posttitle, posttext, posttime, uid],
		[cid] -> [commenttext, commenttime, uid, pid]
	],

	bcnf_decomp(F, Decomp, DecompPlan),
	format("BCNF Decomp:\n"),
	write_bcnf_decomp(DecompPlan),
	format("Result: ~w\n", [Decomp]),

	nl,

	synthesis(F, Synth, SynthPlan),
	format("3NF Synthesis:\n", [Synth]),
	write_synthesis(SynthPlan),
	!.
