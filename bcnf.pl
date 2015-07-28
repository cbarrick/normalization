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
% Generates a BCNF decomposition given the functional dependencies in `F`.
bcnf_decomp(F, Decomp) :-
	% a minimal cover isn't strictly needed, but the generated cover is
	% guaranteed to be sorted, so we can use ordered set operations.
	cover(F, Cover),
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


%! main(X)
% Run the bcnf decomposition example from the textbook.
% X is the result of decomposition.
main(X) :-
	R = [a,b,c,d,e,f,g,h],
	F = [
		[a,b,h] -> [c],
		[a]     -> [d,e],
		[b,g,h] -> [f],
		[f]     -> [a,d,h],
		[b,h]   -> [e,g]
	],
	bcnf_decomp(R, F, X).
