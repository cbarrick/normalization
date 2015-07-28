:- use_module(library(ordsets)).

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


%! bcnf_decomp(+R, +F, -Decomp)
% Decompose the relation `R` with respect to the functional dependencies `F`
% using the BCNF decomposition algorithm.
bcnf_decomp([], _, [[]]) :- !.
bcnf_decomp([X], _, [[X]]) :- !.
bcnf_decomp([X,Y], _, [[X,Y]]) :- !.
bcnf_decomp(R, F, [R]) :- bcnf(R, F), !.
bcnf_decomp(R, F, Decomp) :-
	member(X->Y, F),
	ord_subset(X, R),
	ord_intersection(Y, R, YPrime),
	YPrime \= [],
	closure(X, F, XClose),
	\+ ord_subset(R, XClose),
	ord_union([X, YPrime], XY),
	ord_subtract(R, Y, RMinusY),
	debug(bcnf_decomp, "splitting ~w on ~w: ~w", [R, X->Y, [XY,RMinusY]]),
	bcnf_decomp(XY, F, D0),
	bcnf_decomp(RMinusY, F, D1),
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
