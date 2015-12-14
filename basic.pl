%:-consult(data/small).
:-dynamic preprocessed/0.

%----------------abstract functions----------------------------------------------------------
for_each(_,[]). 
for_each(F,[A|As]):-  
	call(F,A),  
	for_each(F,As).

delete_first([Element|List], Element, List):-!. %otherwise prolog will also look for other options
delete_first([A|List], Element, [A|Out]):-delete_first(List, Element, Out).

gensubset([], []).
gensubset([A|B], [A|Out]):-gensubset(B, Out).
gensubset([_|B], Out):-gensubset(B, Out).

element_of(Elem, [Elem|_]):- !.
element_of(Elem, [_|Tail]):-
	element_of(Elem, Tail).

set_of([A|_], List2):-
	element_of(A, List2), !.
set_of([_|As], List2):-
	set_of(As, List2).

%--------------------------------------sorting-----------------------------------------------------------------------

sort_schedule(schedule(Events), SortedEvents):-
	sort_events(Events, [], SortedEvents).

sort_events([], Itt, Itt).
sort_events([Event|Events], Itt, SortedEvents):-
	insert_events(Event, Itt, UpdatedItterator),
	sort_events(Events, UpdatedItterator, SortedEvents).

insert_events(Event, [], [Event]).
insert_events(Event, [I|Is], [Event, I|Is]):-
	event_smaller(Event, I), !.
insert_events(Event, [I|Is], [I|List]):-
	insert_events(Event, Is, List).

%is A < B => does A happen before B
event_smaller(event(_, _, Day1, _), event(_, _, Day2, _)):-
	Day1<Day2.
event_smaller(event(_, RID1, Day1, _), event(_, RID2, Day2, _)):-
	Day1=Day2,
	RID1@<RID2.
event_smaller(event(_, RID1, Day1, Start1), event(_, RID2, Day2, Start2)):-
	Day1=Day2,
	RID1=RID2,
	Start1<Start2.

%--------------------------------------domain specific functions------------------------------------------------------
is_end(EID, Start, End):-
	duration(EID, Duration),
	End is Start + Duration-1.

teacher_of_exam(EID, LID):-
	has_exam(CID, EID),
	teaches(LID, CID).

preprocess:-
	preprocessed, !. %if flag is set stop 

preprocess:-
	%combo not and fail makes sure every exam is asserted
	assert_all_students(),
	assert_all_conflicts(),
	asserta(preprocessed). % set global flag

assert_all_students:-
	findall(EID, (exam(EID, _), assert_students(EID)), _).

assert_students(EID):-
	has_exam(CID, EID),
	findall(SID, follows(SID, CID), Students),
	length(Students, NumberOfStudents),
	asserta(exam_with_students(EID, Students, NumberOfStudents)).

assert_all_conflicts:-
	findall(EID, (exam(EID, _), assert_conflicts(EID)), _).

assert_conflicts(EID1):-
	exam(EID2, _),
	EID1\=EID2,
	is_conflict(EID1, EID2).

%mogelijke optimalisatie, assert ook omgekeerde relatie => zoek enkel in de rechter deel van de lijst
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


%  bagof(Y, exam_conflicts(Exam,Y), List).