restrictive_between(Lower, Higher, Value):-
	Max is Higher - 1,
	between(Lower, Max, Value).

