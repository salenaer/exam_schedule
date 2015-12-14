:-consult(data/small).
%:-consult(data/largeLong).
:-consult(basic).

:-dynamic exam_with_students/3.
:-dynamic exam_conflicts/2.

is_valid(schedule(Events)):-
	preprocess(),
	findall(EID, has_exam(_, EID), Exams),
	is_valid(Events, Exams, []).
	retract_preprocess().

%is_valid(NotCheckedSchedule, ExamsToSchedule, AlreadyCheckedSchedule)
is_valid([], [], _).
is_valid([event(EID, RID, Day, Start)|Events], Exams, PlannedExams):-
	delete_first(Exams, EID, UpdatedExams), %choose one exam from the list while removing it from the list
	room(RID, _),
	capacity_match(RID, EID),
	times_match(RID, EID, Day, Start),
	not(conflicts(event(EID, RID, Day, Start), PlannedExams)),
	is_valid(Events, UpdatedExams, [event(EID, RID, Day, Start)|PlannedExams]).

times_match(RID, EID, Day, Start):-
	duration(EID, Duration),
	availability(RID,Day,From,Till),
	between(From, Till, Start),
	RoomFree is Till - Start,
	Duration =< RoomFree.

capacity_match(RID, EID):-
	capacity(RID, RoomCapacity),
	exam_with_students(EID, _, NumberOfStudents),
	RoomCapacity >= NumberOfStudents.

conflicts(event(EID, RID, Day, Start), [event(EID2, RID2, Day, Start2)|_]):-
	is_end(EID, Start, End),
	is_end(EID2, Start2, End2),
	between(Start, End, Hour),
	between(Start2, End2, Hour),
	(RID == RID2;
		exam_conflicts(EID, EID2)).

conflicts(Event, [_|Events]):-
	conflicts(Event, Events). 