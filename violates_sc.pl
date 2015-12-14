:-consult(basic).
:-consult(data/small).

violates_sc(schedule(Events), SoftConstraints):-
	preprocess(),
	%violates_sc_lunch_break(Events, LunchBreakCs),
	%violates_sc_no_exam_in_period(Events, NoExamCs),
	%append(LunchBreakCs, NoExamCs, SoftConstraints),
	violates_sc_not_in_period(Events, SoftConstraints),
	retract_preprocess().

violates_sc_lunch_break([], []).
violates_sc_lunch_break([event(EID, _, _, Start)|Events], [List|SoftConstraints]):-
	is_end(EID, Start, End),
	between(Start, End, 12),
	has_exam(CID, EID),
	findall(sc_lunch_break(PID, EID, Penalty), (is_on_site(PID, CID), sc_lunch_break(PID,Penalty)) , List),
	!,
	violates_sc_lunch_break(Events, SoftConstraints).
violates_sc_lunch_break([_|Events], SoftConstraints):-
	violates_sc_lunch_break(Events, SoftConstraints).

is_on_site(PID, CID):-
	follows(PID, CID);
	teaches(PID, CID).

violates_sc_no_exam_in_period([],[]).
violates_sc_no_exam_in_period([event(EID, _, Day, Start)|Events], [sc_no_exam_in_period(LID,Day,From,Till,Penalty)|SoftConstraints]):-
	teacher_of_exam(EID, LID),
	sc_no_exam_in_period(LID,Day,From,Till,Penalty),
	is_end(EID, Start, End),
	between(Start, End, Hour),
	between(From, Till, Hour),
	!,
	violates_sc_no_exam_in_period(Events, SoftConstraints).
violates_sc_no_exam_in_period([_|Events], SoftConstraints):-
	violates_sc_no_exam_in_period(Events, SoftConstraints).

violates_sc_not_in_period([],[]).
violates_sc_not_in_period([event(EID, _, Day, Start)|Events], [List|SoftConstraints]):-
	exam_with_students(EID, Students, _),
	teacher_of_exam(EID, LID),
	%persons = LID|Students => for each of these check if constraint exists if so add to list.
	is_end(EID, Start, End),
	violations_not_in_period([LID|Students], Start, End, EID, Day, List),
	length(List, Length),
	Length > 0,
	violates_sc_not_in_period(Events, SoftConstraints).
violates_sc_not_in_period([_|Events], SoftConstraints):-
	violates_sc_not_in_period(Events, SoftConstraints).

violations_not_in_period([], _, _, _, _, []).
violations_not_in_period([PID|PIDs], Start, End, EID, Day, [sc_not_in_period(PID,EID,Day,From,Till,Penalty)|List]):-
	sc_not_in_period(PID,EID,Day,From,Till,Penalty),
	between(Start, End, Hour),
	between(From, Till, Hour),
	!,
	violations_not_in_period(PIDs, Start, End, EID, Day, List).
violations_not_in_period([_|PIDs], Start, End, EID, Day, List):-
	violations_not_in_period(PIDs, Start, End, EID, Day, List).

% schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]) .
% violates_sc(schedule([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]), X).