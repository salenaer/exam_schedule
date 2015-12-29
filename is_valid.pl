:-module(is_valid, [is_valid/1, is_valid_raw/1, good_extension/5, capacity_match/2]).
:-use_module(basic).

:-dynamic basic:exam_with_students/3.
:-dynamic basic:exam_conflicts/2.

is_valid(Schedule):-
	basic:preprocess,
	is_valid_raw(Schedule).

is_valid_raw(schedule(Events)):-
	findall(EID, has_exam(_, EID), Exams),
	is_valid(Events, Exams, []).

%is_valid(NotCheckedSchedule, ExamsToSchedule, AlreadyCheckedSchedule)
is_valid([], [], _).
is_valid([event(EID, RID, Day, Start)|Events], Exams, PlannedExams):-
	delete_first(Exams, EID, UpdatedExams), %choose one exam from the list while removing it from the list
	good_extension(RID, EID, Day, Start, PlannedExams),
	is_valid(Events, UpdatedExams, [event(EID, RID, Day, Start)|PlannedExams]).

good_extension(RID, EID, Day, Start, PlannedExams):-
	capacity_match(RID, EID),
	times_match(RID, EID, Day, Start),
	not(conflicts(event(EID, RID, Day, Start), PlannedExams)).

times_match(RID, EID, Day, Start):-
	duration(EID, Duration),
	availability(RID,Day,From,Till),
	LastPossibleStartingHour is Till - Duration, 
	between(From, LastPossibleStartingHour, Start).

capacity_match(RID, EID):-
	room(RID, _),
	capacity(RID, RoomCapacity),
	basic:exam_with_students(EID, _, NumberOfStudents),
	RoomCapacity >= NumberOfStudents.

conflicts(event(EID, RID, Day, Start), [event(EID2, RID2, Day, Start2)|_]):-
	is_end(EID, Start, End),
	is_end(EID2, Start2, End2),
	overlapping(Start, End, Start2, End2),
	(RID == RID2; 
		basic:exam_conflicts(EID, EID2)).

conflicts(Event, [_|Events]):-
	conflicts(Event, Events). 

%findall(X, is_valid(X), List), length(List, Length).
%small set 9936 valid schedulas