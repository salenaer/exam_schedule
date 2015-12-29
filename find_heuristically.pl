:-module(find_heuristically, [find_heuristically/2, randomish/2]).
:-use_module(is_valid, [is_valid_raw/1]).
:-use_module(cost, [cost/2]).

:-dynamic exams/1.

find_heuristically(Schedule, Time):-
	 get_time(StartTime),
	 basic:preprocess,
	 EndTime is StartTime + Time,
	 is_valid:is_valid_raw(StartingSchedule), %find one valid schedule
	 cost:cost(StartingSchedule, Cost),
	 is_improvement([weightedSchedule(StartingSchedule, Cost)], Schedule, EndTime),
	 basic:retract_preprocess.

is_improvement(Current, Current, EndTime):-
	get_time(Now),
	Now > EndTime,
	!.

is_improvement(Current, List, EndTime):-
	maplist(randomish, Current, RandomSchedules),
	new_best(Current, RandomSchedules, NewBest),
	is_improvement(NewBest, List, EndTime).

%------------------------------------store only k best ---------------------------------------------
smaller(weightedSchedule(Schedule1, Cost1), weightedSchedule(_, Cost2), weightedSchedule(Schedule1, Cost1)):-
	Cost1 < Cost2.

smaller(weightedSchedule(_, Cost1), weightedSchedule(Schedule2, Cost2), weightedSchedule(Schedule2, Cost2)):-
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
map_cost([Schedule|Schedules], [weightedSchedule(Schedule, Cost)|Rest]):-
	cost(Schedule, Cost),
	print(Cost),
	write("\n"),
	map_cost(Schedules, Rest).

new_best(Schedules, NewSchedules, Best):-
	map_cost(NewSchedules, NewWeighted),
	sort(2, @=<, NewWeighted, NewSortedWeighted),
	insert(0, NewSortedWeighted, Schedules, Best).

%--------------------------------improve schedule --------------------------------------------------
randomish(weightedSchedule(schedule(Events), _), schedule([event(EID, RID, Day, Start)|ScheduledEvents])):-
	random_permutation(Events, RandomEvents),
	basic:delete_first(RandomEvents, Event, ScheduledEvents),
	mutate_event(Event, event(EID, RID, Day, Start)),
	is_valid:good_extension(RID, EID, Day, Start, ScheduledEvents).

mutate_event(event(EID, RID, Day, Start), event(EID, RID, Day2, Start2)):-
	first_day(FirstDay),
	last_day(LastDay),
	findall(Days, between(FirstDay, LastDay, Days), Range),
	random_permutation(Range, DayPermutation),
	member(Day2, DayPermutation),
	availability(RID, Day2, StartHour, EndHour),
	duration(EID, Duration),
	Max is EndHour - Duration,
	findall(Hour, between(StartHour, Max, Hour), Hours),
	random_permutation(Hours, HourPermutation),
	member(Start2, HourPermutation),
	(Day =\= Day2; Start =\= Start2).

%randomish( weightedSchedule(schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 12), event(e5, r2, 4, 10)]), 4.5), X).