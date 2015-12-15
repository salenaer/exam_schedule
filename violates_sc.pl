:-consult(basic).
:-consult(data/small).

:-dynamic exam_with_students/3.

violates_sc(schedule(Events), SoftConstraints):-
	preprocess(),
	sort_events(Events, [], SortedEvents),
	violates_simple_sc(SortedEvents, SoftConstraints),
	%violates_combo_sc(SortedEvents, ComboConstraints),
	%append(SimpleConstraints, ComboConstraints, SoftConstraints),
	retract_preprocess().

%------------------------simple constraints, can be deduced from one event at a time--------------------------

%stop condition
violates_simple_sc([],[]).
%teacher doesn't want exam on day X
violates_simple_sc([Event|Events], SoftConstraints):-
	violates_sc_no_exam_in_period(Event, TeacherPeriodConstraint),
	!,
	violates_simple_sc(Events, Constraints),
	violates_simple_list_sc(Event, ListConstraints),
	append([TeacherPeriodConstraint|ListConstraints], Constraints, SoftConstraints).
%teacher doens't mind
violates_simple_sc([Event|Events], SoftConstraints):-
	violates_simple_sc(Events, Constraints),
	violates_simple_list_sc(Event, ListConstraints),
	append(ListConstraints, Constraints, SoftConstraints).

violates_simple_list_sc(Event, ListConstraints):-
	violates_sc_lunch_break(Event, LunchConstraint),
	violates_sc_not_in_period(Event, PersonPeriodConstraint),
	append(LunchConstraint, PersonPeriodConstraint, ListConstraints).


%return potentially empty list of persons who like a lunchbreak while event is planned
violates_sc_lunch_break(event(EID, _, _, Start), SoftConstraints):-
	is_end(EID, Start, End),
	between(Start, End, 12),
	has_exam(CID, EID),
	findall(sc_lunch_break(PID, EID, Penalty), (is_on_site(PID, CID), sc_lunch_break(PID,Penalty)) , SoftConstraints).
violates_sc_lunch_break(_, []).
is_on_site(PID, CID):-
	follows(PID, CID);
	teaches(PID, CID).

%gives back atom or fails
violates_sc_no_exam_in_period(event(EID, _, Day, Start), sc_no_exam_in_period(LID,Day,From,Till,Penalty)):-
	teacher_of_exam(EID, LID),
	sc_no_exam_in_period(LID,Day,From,Till,Penalty),
	is_end(EID, Start, End),
	between(Start, End, Hour),
	between(From, Till, Hour),
	!.

violates_sc_not_in_period(event(EID, _, Day, Start), SoftConstraints):-
	exam_with_students(EID, Students, _),
	teacher_of_exam(EID, LID),
	%persons = LID|Students => for each of these check if constraint exists if so add to list.
	is_end(EID, Start, End),
	violations_not_in_period([LID|Students], Start, End, EID, Day, SoftConstraints),
	!.
violates_sc_not_in_period(_, []).

violations_not_in_period([], _, _, _, _, []).
violations_not_in_period([PID|PIDs], Start, End, EID, Day, [sc_not_in_period(PID,EID,Day,From,Till,Penalty)|List]):-
	sc_not_in_period(PID,EID,Day,From,Till,Penalty),
	between(Start, End, Hour),
	between(From, Till, Hour),
	!,
	violations_not_in_period(PIDs, Start, End, EID, Day, List).
violations_not_in_period([_|PIDs], Start, End, EID, Day, List):-
	violations_not_in_period(PIDs, Start, End, EID, Day, List).





%------------------------combo constraints, can only be deduced from multible events--------------------------
violates_sc_same_day_b2b(PID, Events, Broken):-
	violates_sc_same_day_b2b(PID, Events, 0, [],[], Broken), !.

violates_sc_same_day_b2b(_, [], _, _, _, []).
%exam still at same day
violates_sc_same_day_b2b(PID, [event(EID, _, Day, _)|Events], Day, TodayExams, YesterdayExams, List):-
	has_exam(CID, EID),
	is_on_site(PID, CID),
	!, %don't try matching with the other day and not following pattern
	sc_same_day(PID,SameDayPenalty), 
	sc_b2b(PID,B2BPenalty),
	same_day_broken(PID, SameDayPenalty, EID, TodayExams, SameDayBroken),
	back_to_back_broken(PID, B2BPenalty, EID, YesterdayExams, B2BBroken),
	violates_sc_same_day_b2b(PID, Events, Day, [EID|TodayExams], YesterdayExams, Rest),
	append([SameDayBroken, B2BBroken, Rest], List).

%when I find a new day, update TodayExams and YesterdayExams, reuses previous predicate.
violates_sc_same_day_b2b(PID, [event(EID, RID, Day, Start)|Events], OldDay, TodayExams, _, List):-
	Day =\= OldDay,
	Difference is Day - OldDay,
	!,
	(Difference == 1->
		violates_sc_same_day_b2b(PID, [event(EID, RID, Day, Start)|Events], Day, [], TodayExams, List);
		violates_sc_same_day_b2b(PID, [event(EID, RID, Day, Start)|Events], Day, [], [], List)).	

violates_sc_same_day_b2b(PID, [_|Events], Day, TodayExams, YesterdayExams, List):-
	violates_sc_same_day_b2b(PID, Events, Day, TodayExams, YesterdayExams, List).	
	

same_day_broken(_, _, _, [], []).
same_day_broken(PID, Penalty, EID2, [EID1|EIDS], [sc_same_day(PID,EID1,EID2,Penalty)|List]):-
	same_day_broken(PID, Penalty, EID2, EIDS, List).
back_to_back_broken(_, _, _, [], []).
back_to_back_broken(PID, Penalty, EID2, [EID1|EIDS], [sc_b2b(PID,EID1,EID2,Penalty)|List]):-
	back_to_back_broken(PID, Penalty, EID2, EIDS, List).

% schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]) .
% violates_sc(schedule([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]), X).
%violates_sc_same_day_b2b(s2, [event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)], X).