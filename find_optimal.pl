%Sander Lenaerts
%29/12/2015
%Finds the optimal schedule by: 
% 		generating all valid schedules,
% 		calculating their cost 
% 		picking the smallest onces

:-module(find_optimal, [find_optimal/1]).
:-use_module(is_valid, [is_valid/1]).
:-use_module(cost, [cost/2]).

%find_optimal(-Schedule)
find_optimal(Schedule):-
	basic:preprocess,
	findall(X, is_valid(X), List),
	are_best_schedules(List, Best),
    basic:retract_preprocess,
    member(Schedule, Best).

%is_optimal(+Schedule)
%check if some schedules is an optimal schedule
is_optimal(Schedule) :-
	find_optimal(Schedule).

%are_best_schedules(+GradedSchedules, -BestSchedules)
are_best_schedules(Schedules, Best):-
	are_best_schedules(Schedules, [], 9001, Best).

%are_best_schedules(+GradedSchedules, +BestSchedules, +BestSchedulePrice, -BestSchedules)
%use iterator to keep track of best schedules found so far
are_best_schedules([], CurrentBest, _, CurrentBest):-!.
are_best_schedules([Schedule|Schedules], CurrentBest, CurrentCost, Best):-
	cost(Schedule, Cost),
	next_step(Cost, Schedule, Schedules, CurrentBest, CurrentCost, Best).

%Calculate the next step in the iteration
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