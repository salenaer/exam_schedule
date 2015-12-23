:-consult(basic).

pritty_print(schedule(Events)):-
	sort_events(Events, [], SortedEvents),
	print_events(SortedEvents).

pritty_print(schedule(Events), Student):-
	exclude_events(Student, Events, SomeEvents),
	pritty_print(schedule(SomeEvents)).


%------------------------------printing--------------------------------
print_events([]).
print_events([event(EID, RID, Day, Start)|Events]):-
	print_day(Day),
	print_room(RID),
	print_event(EID, Start),
	print_events(Events, Day, RID).

print_events([], _, _).
print_events([event(EID, RID, Day, Start)|Events], Day, RID):-
	print_event(EID, Start),
	print_events(Events, Day, RID),
	!.

%different room
print_events([event(EID, RID, Day, Start)|Events], Day, _):-
	write("\n"),
	print_room(RID),
	print_event(EID, Start),
	print_events(Events, Day, RID),
	!.

%different day
print_events([event(EID, RID, Day, Start)|Events], _, _):-
	write("\n"),
	print_day(Day),
	print_room(RID),
	print_event(EID, Start),
	print_events(Events, Day, RID).

print_day(Day):-
	write("*** Day "),
	write(Day),
	write(" ***\n").

print_room(RID):-
	room(RID, RoomName),
	write(RoomName),
	write(": \n").

print_event(EID, Start):-
	duration(EID, Duration),
	exam(EID, ExamName),
	has_exam(CID,EID),
	teaches(LID,CID),
	lecturer(LID,Lecturer),
	End is Start + Duration,
	write(Start),
	write(":00-"),
	write(End),
	write(":00 "),
	write(ExamName),
	write(" ("),
	write(Lecturer),
	write(")\n").

%------------------------------excluding--------------------------------
exclude_events(_, [], []).

exclude_events(SID, [event(EID, RID,Day,Start)|Events], [event(EID, RID,Day,Start)|SomeEvents]):-
	has_exam(CID, EID),
	follows(SID, CID),
	!,
	exclude_events(SID, Events, SomeEvents).

exclude_events(SID, [event(_, _,_,_)|Events], SomeEvents):-
	exclude_events(SID, Events, SomeEvents).

%schedule([event(e1, r2, 1, 10), event(e2, r1, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 12), event(e5, r2, 4, 10)])
%schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r2, 3, 11), event(e4, r2, 3, 13), event(e5, r2, 4, 10)]) 
%schedule([event(e1, r2, 1, 10), event(e2, r2, 3, 12), event(e3, r1, 2, 10), event(e4, r1, 5, 10), event(e5, r2, 4, 10)]) 