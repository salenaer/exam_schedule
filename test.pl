pred1(X, Y):-
	X > 5,
	!,
	Y is 3.

pred1(_, []).

isLarge(X, 2):-
	X > 5,!.

isLarge(_, 3).