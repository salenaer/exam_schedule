:-consult(is_valid).
:-consult(cost).

find_optimal(Schedule):-
	preprocess,
	findall(X, is_valid_raw(X), List),
	are_best_schedules(List, [], 9001, Best),
    retract_preprocess,
    member(Schedule, Best).

are_best_schedules([], CurrentBest, _, CurrentBest):-!.
are_best_schedules([Schedule|Schedules], CurrentBest, CurrentCost, Best):-
	cost_raw(Schedule, Cost),
	next_step(Cost, Schedule, Schedules, CurrentBest, CurrentCost, Best).

next_step(Cost, Schedule, Schedules, _, CurrentCost, Best):-
	Cost < CurrentCost,
	!,
	are_best_schedules(Schedules, [Schedule], Cost, Best).

next_step(Cost, Schedule, Schedules, CurrentBest, Cost, Best):-
	!,
	are_best_schedules(Schedules, [Schedule|CurrentBest], Cost, Best).

next_step(Cost, _, Schedules, CurrentBest, CurrentCost, Best):-
	Cost > CurrentCost,
	are_best_schedules(Schedules, CurrentBest, CurrentCost, Best).