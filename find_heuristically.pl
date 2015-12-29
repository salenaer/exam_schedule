%Sander Lenaerts
%29/12/2015
%problem is NP hard making it impossible to brute force on large instances
%use heuristic to direct search

:-module(find_heuristically, [find_heuristically/2, find_heuristically/1]).
:-use_module(is_valid, [is_valid/1]).
:-use_module(cost, [cost/2]).

:-dynamic exams/1.

%find_heuristically(-Schedule)
%default search takes around 120 seconds
find_heuristically(Schedule):-
	find_heuristically(Schedule, 80).

%find_heuristically(-Schedule, +Time)
%search for the optimal schedule for Time seconds
find_heuristically(Schedule, Time):-
	 get_time(StartTime),
	 basic:preprocess,
	 EndTime is StartTime + Time,
	 is_valid:is_valid(StartingSchedule), %find one valid schedule
	 cost:cost(StartingSchedule, Cost),
	 is_improvement([gradedSchedule(StartingSchedule, Cost)], Schedule, EndTime),
	 basic:retract_preprocess.

%is_improvement(+Schedule, -HeuriticallyOptimalSchedule, +Time)
%If time is up return the current schedule
%otherwise improve the current schedules and loop.
is_improvement(Current, Current, EndTime):-
	get_time(Now),
	Now > EndTime,
	!.

is_improvement(Current, List, EndTime):-
	maplist(randomish, Current, RandomSchedules),
	new_best(Current, RandomSchedules, NewBest),
	is_improvement(NewBest, List, EndTime).

%------------------------------------store only k best ---------------------------------------------
smaller(gradedSchedule(Schedule1, Cost1), gradedSchedule(_, Cost2), gradedSchedule(Schedule1, Cost1)):-
	Cost1 < Cost2.

smaller(gradedSchedule(_, Cost1), gradedSchedule(Schedule2, Cost2), gradedSchedule(Schedule2, Cost2)):-
	Cost1 >= Cost2.

%only keep the 10 largest in the set
insert(10, _,_, []):-!.
insert(_, [], A, A):-!.
insert(_, A, [], A):-!.
insert(Element, [WeightedNew|WeightedNews], [Schedule|Schedules], [WeightedNew|Rest]):-
	smaller(WeightedNew, Schedule, WeightedNew),
	!,
	NewElement is Element + 1,
	insert(NewElement, WeightedNews, [Schedule|Schedules], Rest).

insert(Element, [WeightedNew|WeightedNews], [Schedule|Schedules], [Schedule|Rest]):-
	NewElement is Element + 1,
	insert(NewElement, [WeightedNew|WeightedNews], Schedules, Rest).	

map_cost([], []):-!.
map_cost([Schedule|Schedules], [gradedSchedule(Schedule, Cost)|Rest]):-
	cost(Schedule, Cost),
	map_cost(Schedules, Rest).

new_best(Schedules, NewSchedules, Best):-
	map_cost(NewSchedules, NewWeighted),
	sort(2, @=<, NewWeighted, NewSortedWeighted),
	insert(0, NewSortedWeighted, Schedules, Best).

%--------------------------------improve schedule --------------------------------------------------
randomish(gradedSchedule(schedule(Events), _), schedule([event(EID, RID, Day, Start)|ScheduledEvents])):-
	random_permutation(Events, RandomEvents),
	basic:delete_first(RandomEvents, Event, ScheduledEvents),
	randomize_event(Event, event(EID, RID, Day, Start)),
	is_valid:good_extension(RID, EID, Day, Start, ScheduledEvents).

randomize_event(event(EID, RID, Day, Start), event(EID, RID2, Day2, Start2)):-
	first_day(FirstDay),
	last_day(LastDay),
	findall(Days, between(FirstDay, LastDay, Days), PossibleDays),
	random_permutation(PossibleDays, RandomDays), %source of randomness (member will always take the first day from the list first)
	member(Day2, RandomDays), %pick some day at random from the list
	availability(RID2, Day2, From, Till), % choose a room that is available on this day
	duration(EID, Duration),
	LastPossibleStartingHour is Till - Duration, 
	findall(Hour, between(From, LastPossibleStartingHour, Hour), PossibleHours),
	random_permutation(PossibleHours, RandomHours),
	member(Start2, RandomHours),
	(Day \== Day2; Start \== Start2; RID \== RID2).