:-module(violates_sc, [violates_sc/2]).
:-use_module(basic).

:-dynamic basic:exam_with_students/3.
:-dynamic events_asserted/0.
:-dynamic event/5.

violates_sc(schedule(Events), SoftConstraints):-
    assert_events(Events),
    sort_events(Events, [], SortedEvents),
    lunch_break_violations(Lunch_break_violations),
    no_exam_in_period_violations(No_exam_in_period_violations),
    not_in_period_violations(Not_in_period_violations),
    same_day_violations(Same_day_violations),
    back_to_back_violations(Back_to_back_violations),
    study_period_violations(SortedEvents, Study_period_violations),
    correction_time_violations(SortedEvents, Correction_time_violations),
    append([Lunch_break_violations, No_exam_in_period_violations, Not_in_period_violations, 
        Same_day_violations, Back_to_back_violations, Study_period_violations, Correction_time_violations], SoftConstraints),
    retract_events.

%-------------------------assert_events-------------------------------------------------------------------------
assert_events(_):-
    events_asserted.
assert_events([]):-
    asserta(events_asserted).
assert_events([event(EID, RID, Day, Start)|Events]):-
    is_end(EID, Start, End),
    asserta(event(EID, RID, Day, Start, End)),
    assert_events(Events).
retract_events:-
    retractall(event(_, _, _, _, _)),
    retract(events_asserted).

%------------------------lunch break--------------------------
during_lunch_break(EID):-
    event(EID, _, _, Start, End),
    restrictive_between(Start, End, 12). %a exam ending at 12 oclock doest prevent lunchbreak

lunch_break_violations(SoftConstraints):-
    findall(sc_lunch_break(PID, EID, Penalty), 
           (    sc_lunch_break(PID,Penalty),
                is_on_site(PID, EID), %the person needs to follow/teach the course
                during_lunch_break(EID)),
            SoftConstraints).

%------------------------not in period and no exam in period--------------------------

no_exam_in_period_violations(SoftConstraints):-
    findall(sc_no_exam_in_period(LID,EID,Day, From,Till,Penalty),
           (    sc_no_exam_in_period(LID,Day,From,Till,Penalty),
                event(EID, _, Day, Start, End),
                teacher_of_exam(EID, LID),
                overlapping(Start, End, From, Till)),
            SoftConstraints).

not_in_period_violations(SoftConstraints):-
    findall(sc_not_in_period(PID,EID,Day, From,Till,Penalty),
           (    sc_not_in_period(PID,EID, Day,From,Till,Penalty),
                event(EID, _, Day, Start, End),
                is_on_site(PID, EID),
                overlapping(Start, End, From, Till)),
            SoftConstraints).

%------------------------same day and b2b--------------------------
same_day_violations(SoftConstraints):-
    findall(sc_same_day(PID,EID1,EID2,Penalty),
           (    sc_same_day(PID,Penalty),
                event(EID1, _, Day, _, _),
                is_on_site(PID, EID1),
                event(EID2, _, Day, _, _),
                EID1 @< EID2, %otherwise we punish twice for the same crime
                is_on_site(PID, EID2)),
           SoftConstraints).

back_to_back_violations(SoftConstraints):-
    findall(sc_b2b(PID,EID1,EID2,Penalty),
           (    sc_b2b(PID,Penalty),
                event(EID1, _, Day, Start, _),
                is_on_site(PID, EID1),
                event(EID2, _, Day, _, Start),
                is_on_site(PID, EID2)),
           SoftConstraints).

%--------------------------------------------study period--------------------------------------------------------
study_period_violations(Events, Constraints):-
    first_day(FirstDay),
    findall(Constraint, 
            (   student(SID, _),
                study_period_violations(SID, Events, FirstDay, 0, Constraint)),
                ListConstraints),
    append(ListConstraints, Constraints).
    
study_period_violations(SID, [event(EID, _, Day, _)|Events], LocedUntil, DaysTooLittle, Constraints):-
    has_exam(CID, EID),
    follows(SID, CID),
    !,
    sc_study_time(EID, StudyDays), %how many days do I need to study
    StudyTime is Day - LocedUntil, %how much time do I have to study
    (StudyTime >= StudyDays -> 
        (   NewLocedUntil is LocedUntil + StudyDays,
            study_period_violations(SID, Events, NewLocedUntil, DaysTooLittle, Constraints));
        (   DaysTooLittleForThisExam is StudyDays - StudyTime,
            TotalDayTooLittle is DaysTooLittle+DaysTooLittleForThisExam,
            study_period_violations(SID, Events, Day, TotalDayTooLittle, Constraints))).

study_period_violations(SID, [_|Events], LocedUntil, DaysTooLittle, Constraints):-
    study_period_violations(SID, Events, LocedUntil, DaysTooLittle, Constraints),
    !.

study_period_violations(SID, [], _, DaysTooLittle, [sc_study_time(SID,DaysTooLittle,TotalPenalty)]):-
    DaysTooLittle > 0,
    !,
    sc_study_penalty(SID,Penalty),
    TotalPenalty is Penalty * DaysTooLittle.

study_period_violations(_, [], _, _, []).

%--------------------------------------------correction time--------------------------------------------------------
correction_time_violations(Events, Constraints):-
    first_day(FirstDay),
    findall(Constraint, 
            (   lecturer(LID, _),
                correction_time_violations(LID, Events, FirstDay, 0, Constraint)),
            ListConstraints),
    append(ListConstraints, Constraints).
    
correction_time_violations(LID, [event(EID, _, Day, _)|Events], LastExamDay, DaysNeeded, Constraints):-
    has_exam(CID, EID),
    teaches(LID, CID),
    !,
    sc_correction_time(EID, CorrectionDays), %how many days do I need to correct
    newDaysNeeded(LastExamDay, DaysNeeded, Day, CorrectionDays, NewDaysNeeded),
    correction_time_violations(LID, Events, Day, NewDaysNeeded, Constraints).

correction_time_violations(LID, [_|Events], LastExamDay, DaysNeeded, Constraints):-
    correction_time_violations(LID, Events, LastExamDay, DaysNeeded, Constraints),
    !.

correction_time_violations(LID, [], LastExamDay, DaysNeeded, [sc_correction_time(LID,DaysNeeded,TotalPenalty)]):-
    last_day(LastDay),
    newDaysNeeded(LastExamDay, DaysNeeded, LastDay, 0, NewDaysNeeded),
    NewDaysNeeded>0,
    !,
    sc_correction_penalty(LID,Penalty),
    TotalPenalty is Penalty * NewDaysNeeded.

correction_time_violations(_, [], _, _, []).

newDaysNeeded(_, 0, _, CurrentDaysNeeded, CurrentDaysNeeded):-
    !.

newDaysNeeded(LastExamDay, PreviousDaysNeeded, CurrentExamDay, CurrentDaysNeeded, NewDaysNeeded):-
    Difference is CurrentExamDay-LastExamDay,
    DaysStillNeeded is max(PreviousDaysNeeded - Difference, 0),
    NewDaysNeeded is  DaysStillNeeded + CurrentDaysNeeded.


% schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]) .
% violates_sc(schedule([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]), X).
% study_period_violations(s2, [event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)], X).
% s2 => [e1, e2, e3, e5].
% =>      2,  1,  1,  1
% study_period_violations([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)], X).
% assert_events([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]).
% findall(event(EID, RID, Day, Start, End), event(EID, RID, Day, Start, End), Schedule).

%member(event(E1,R1,D,_,H),Events),
%member(event(E2,R2,D,H,_),Events),
