:-module(find_optimal, [find_optimal/1]).
:-use_module(is_valid, [is_valid_raw/1]).
:-use_module(cost, [cost_raw/2]).

find_optimal(Schedule):-
	basic:preprocess,
	findall(X, is_valid_raw(X), List),
	are_best_schedules(List, [], 9001, Best),
    basic:retract_preprocess,
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