%:-consult(data/small).
:-dynamic preprocessed/0.
%abstract functions
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




%domain specific functions
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
	retractall(exam_conflicts(X,Y)),
	retractall(exam_with_students(A,B,C)),
	retract(preprocessed).


%  bagof(Y, exam_conflicts(Exam,Y), List).