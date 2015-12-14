pred1(X, Y):-
	X > 5,
	!,
	Y is 3.

pred1(_, []).
