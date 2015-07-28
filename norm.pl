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


%! bcnf_decomp(+F, -Decomp)
% Generates a schema using BCNF decomposition with respect to the functional
% dependencies in `F`.
bcnf_decomp(F, Decomp) :-
	% a minimal cover isn't strictly needed, but the generated cover is
	% guaranteed to be sorted, so we can use ordered set operations.
	once(cover(F, Cover)),
	flatten_fds(Cover, R),
	bcnf_decomp_(R, Cover, Decomp).

bcnf_decomp_([], _, [[]]) :- !.
bcnf_decomp_([X], _, [[X]]) :- !.
bcnf_decomp_([X,Y], _, [[X,Y]]) :- !.
bcnf_decomp_(R, F, [R]) :- bcnf(R, F), !.
bcnf_decomp_(R, F, Decomp) :-
	member(X->Y, F),
	ord_subset(X, R),
	ord_intersection(Y, R, YPrime),
	YPrime \= [],
	closure(X, F, XClose),
	\+ ord_subset(R, XClose),
	ord_union([X, YPrime], XY),
	ord_subtract(R, Y, RMinusY),
	debug(bcnf_decomp, "splitting ~w on ~w: ~w", [R, X->Y, [XY,RMinusY]]),
	bcnf_decomp_(XY, F, D0),
	bcnf_decomp_(RMinusY, F, D1),
	ord_union(D0, D1, Decomp).


%! synthesize(+F, -Tables)
% Generates a schema using 3NF synthesis with respect to the functional
% dependencies in `F`.
synthesize(F, Tables) :-
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
		Tables = InitialTables
	->true;
		once(key(R, Cover, Key)),
		sort([Key|InitialTables], Tables)
	).


%! main(X)
% Run a bcnf decomposition on our project data.
main :-
	F = [
		[uid] -> [role, email, password],
		[pid] -> [posttitle, posttext, posttime, uid],
		[cid] -> [commenttext, commenttime, uid, pid]
	],

	bcnf_decomp(F, Decomp),
	format('BCNF Decomp: ~w\n', [Decomp]),
	synthesize(F, Synth),
	format('3NF Synthesis: ~w\n', [Synth]),
	!.
