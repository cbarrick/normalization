#!/usr/bin/env swipl -g norm_example -t halt

:- use_module(library(ordsets)).


%! cover(+F, -Cover)
% Generates a normalized minimal cover of the functional dependencies `F`.
cover(F, Minimal) :-
	% normalize elements of F by sorting
	setof(X->Y, W^Z^(
		member(W->Z, F),
		sort(W, X),
		sort(Z, Y)
	), A),

	% decompose the right-hand-sides into singletons
	findall(X->[Y], (
		member(X->Z, A),
		member(Y, Z),
		\+ ord_memberchk(Y, X)
	), B),

	% minimize left-hand-sides
	findall(X->[Y], (
		member(Z->[Y], B),
		findall(W, (
			select(W, Z, Rest),
			closure(Rest, B, Closure),
			\+ member(Y, Closure)
		), X)
	), C),

	% eliminate redundant rules
	findall(X->[Y], (
		select(X->[Y], C, Rest),
		\+ (
			closure(X, C, Closure),
			closure(X, Rest, Closure)
		)
	), D),

	% recombine rules
	findall(X->Y, (
		setof(Z, member(X->[Z], D), Y)
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


%! bcnf_decomp(+F, -Schema, -Plan)
% Generates a schema using BCNF decomposition with respect to the functional
% dependencies in `F`. `Plan` is a compound describing the decomposition
% process. Use `write_bcnf_decomp/1` to write the plan in a human format.
bcnf_decomp(F, Schema, plan(Schema, Cover, DecompTree)) :-
	% a minimal cover isn't strictly needed, but the generated cover is sorted
	% thus we can use ordered set operations for efficiency
	cover(F, Cover),
	flatten_fds(Cover, R),
	bcnf_decomp_(R, Cover, Schema, DecompTree).

bcnf_decomp_([], _, [[]], leaf([])) :- !.
bcnf_decomp_([X], _, [[X]], leaf([X])) :- !.
bcnf_decomp_([X,Y], _, [[X,Y]], leaf([X,Y])) :- !.
bcnf_decomp_(R, F, Schema, node(R, X->Y, Plan0, Plan1)) :-
	member(X->Y, F),
	ord_subset(X, R),
	member(Z, Y),
	member(Z, R),
	closure(X, F, Closure),
	\+ ord_subset(R, Closure),
	!,
	ord_union(X, Y, XY),
	bcnf_decomp_(XY, F, D0, Plan0),
	append(D0, D1, Schema),
	ord_subtract(R, Y, RMinusY),
	bcnf_decomp_(RMinusY, F, D1, Plan1).
bcnf_decomp_(R, _, [R], leaf(R)).


%! write_bcnf_decomp(+Plan)
% Write the BCNF decomposition plan in a human format.
write_bcnf_decomp(plan(Schema, Cover, DecompTree)) :-
	format("- Schema:\t`~w`\n", [Schema]),
	format("- Dependencies:\t`~w`\n", [Cover]),
	format("- Decomposition:\n"),
	format("```\n"),
	write_bcnf_decomp_(DecompTree, 0),
	format("```\n").

write_bcnf_decomp_(leaf(Leaf), _) :-
	format("~w\n", [Leaf]).

write_bcnf_decomp_(node(Attrs, X->Y, Left, Right), Depth) :-
	Less is Depth-1,
	More is Depth+1,
	format("~w\n", [Attrs]),
	forall(between(0, Less, _), write("    ")),
	format("├── violation: ~w\n", [X->Y]),
	forall(between(0, Less, _), write("    ")),
	format("├── "),
	write_bcnf_decomp_(Left, More),
	forall(between(0, Less, _), write("    ")),
	format("└── "),
	write_bcnf_decomp_(Right, More).


%! synthesis(+F, -Tables, -Plan)
% Generates a schema using 3NF synthesis with respect to the functional
% dependencies in `F`. `Plan` is a compound describing the synthesis
% process. Use `write_synthesis/1` to write the plan in a human format.
synthesis(F, Tables, plan(Tables, Cover, Key)) :-
	% synthesize tables
	cover(F, Cover),
	findall(XY, (
		member(X->Y, Cover),
		ord_union(X, Y, XY)
	), InitialTables),

	% check for losslessness
	% if the initial tables are lossy, include an extra key table
	flatten_fds(Cover, R),
	(
		member(Key, InitialTables),
		closure(Key, Cover, R),
		Tables = InitialTables
	->true;
		key(R, Cover, Key),
		ord_union(InitialTables, [Key], Tables)
	).


%! write_synthesis(+Plan)
% Write the 3NF synthesis plan in a human format.
write_synthesis(plan(Tables, Cover, Key)) :-
	format("- Schema:\t`~w`\n", [Tables]),
	format("- Dependencies:\t`~w`\n", [Cover]),
	format("- Global Key:\t`~w`\n", [Key]).


%! norm_example(X)
% Run a bcnf decomposition on example data.
norm_example :-
	F = [
		[a,b,h] -> [c],
		[a] -> [d,e],
		[b,g,h] -> [f],
		[f] -> [a,d,h],
		[b,h] -> [g,e]
	],

	format("# BCNF Decomposition\n"),
	bcnf_decomp(F, _, DecompPlan),
	write_bcnf_decomp(DecompPlan),

	nl,

	format("# 3NF Synthesis\n"),
	synthesis(F, _, SynthPlan),
	write_synthesis(SynthPlan).
