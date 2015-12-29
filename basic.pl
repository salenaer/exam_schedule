%Sander Lenaerts
%29/12/2015
%To prevent code duplication in the rest of the project


:-module(basic, [delete_first/3, overlapping/4, sort_events/2, is_end/3, repeat/3, student_of_exam/2,
				 teacher_of_exam/2, is_on_site/2, preprocess/0, is_conflict/2, retract_preprocess/0,
				 restrictive_between/3 ,full_cleanup/0]).
:-dynamic preprocessed/0.
:-dynamic exam_with_students/3.

%----------------generic predicates----------------------------------------------------------
%delete_first(+List, -DeletedElement, -RestOfList)
%cut because we only want to delete the first occurrence of some element
delete_first([Element|List], Element, List):-!. 
delete_first([A|List], Element, [A|Out]):-delete_first(List, Element, Out).

%element_of(?Elem, +List)
element_of(Elem, [Elem|_]):- !.
element_of(Elem, [_|Tail]):-
	element_of(Elem, Tail).

%set_of(+List, +List)
%used to check if two lists share an element
set_of([A|_], List2):-
	element_of(A, List2), !.
set_of([_|As], List2):-
	set_of(As, List2).

%repeat(+Element, +k, -List)
%repeat element k times
repeat(Elem, Lenght, List) :-
    length(List, Lenght), 
    maplist(=(Elem), List).

%restrictive_between(+Lower, +Higher, -Value)
%restrictive_between = lower <= value < higher
restrictive_between(Lower, Higher, Value):-
	Max is Higher - 1,
	between(Lower, Max, Value).

%overlapping(+Start1, +End1, +Start2, +End2)
%true if period1 and period2 overlap
overlapping(Start1, End1, Start2, End2):-
	restrictive_between(Start1, End1, Hour),
    restrictive_between(Start2, End2, Hour),
    !. %once two periods overlap with one hour, they overlap=>no need to backtrack

%--------------------------------------sorting-----------------------------------------------------------------------
%we want to sort events for easy processing and printing. 
%sort by day then room then starting hour

%sort_events(+List, -SortedList)
sort_events(List, Result):-
	sort_events(List, [], Result).
sort_events([], Itt, Itt).
sort_events([Event|Events], Itt, SortedEvents):-
	insert_events(Event, Itt, UpdatedItterator),
	sort_events(Events, UpdatedItterator, SortedEvents).

%cut to prevent trying other branches
insert_events(Event, [], [Event]):-!. 
insert_events(Event, [I|Is], [Event, I|Is]):-
	event_smaller(Event, I), 
	!.
insert_events(Event, [I|Is], [I|List]):-
	insert_events(Event, Is, List).

event_smaller(event(_, _, Day1, _), event(_, _, Day2, _)):-
	Day1<Day2.
event_smaller(event(_, RID1, Day1, _), event(_, RID2, Day2, _)):-
	Day1=Day2,
	RID1@<RID2.
event_smaller(event(_, RID1, Day1, Start1), event(_, RID2, Day2, Start2)):-
	Day1=Day2,
	RID1=RID2,
	Start1<Start2.

%--------------------------------------domain specific predicates------------------------------------------------------
%is_end(+EID, +Start, -End)
is_end(EID, Start, End):-
	duration(EID, Duration),
	End is Start + Duration.

%teacher_of_exam(+EID, -LID)
teacher_of_exam(EID, LID):-
	has_exam(CID, EID),
	teaches(LID, CID).

%student_of_exam(+EID, -SID)
student_of_exam(EID, SID):-
	has_exam(CID, EID),
	follows(SID, CID).

%is_on_site(-PID, +EID)
%generate all students and teacher who follow exam
is_on_site(PID, EID):-
	has_exam(CID, EID),
    (	follows(PID, CID);
    	teaches(PID, CID)).

%global flag to prevent double asserting
preprocess:-
	preprocessed, !. 

preprocess:-
	assert_all_students(),
	assert_all_conflicts(),
	asserta(preprocessed).

%assert for each exam: 
%1) the students following the exam
%2) the number of students following this exam
assert_all_students:-
	findall(EID, (exam(EID, _), assert_students(EID)), _).

assert_students(EID):-
	has_exam(CID, EID),
	findall(SID, follows(SID, CID), Students),
	length(Students, NumberOfStudents),
	asserta(exam_with_students(EID, Students, NumberOfStudents)).

%assert all exams that cannot happen at same time
assert_all_conflicts:-
	findall(EID, (exam(EID, _), assert_conflicts(EID)), _).

assert_conflicts(EID1):-
	exam(EID2, _),
	EID1\=EID2,
	is_conflict(EID1, EID2).

is_conflict(EID1, EID2):-
	teacher_of_exam(EID1, LID),
	teacher_of_exam(EID2, LID), % check if teacher is the same
	asserta(exam_conflicts(EID1, EID2)),
	!.

is_conflict(EID1, EID2):-
	exam_with_students(EID1, Students1, _),
	exam_with_students(EID2, Students2, _),
	set_of(Students1, Students2),
	asserta(exam_conflicts(EID1, EID2)),
	!.

retract_preprocess:-
	retractall(exam_conflicts(_,_)),
	retractall(exam_with_students(_,_,_)),
	retract(preprocessed).

%retracts all dynamic data to allow restoring knowledge base to consistent state. 
full_cleanup:-
	(	retractall(event(_,_,_,_,_)),
		retract(events_asserted),
		retract_preprocess);
	retract_preprocess.