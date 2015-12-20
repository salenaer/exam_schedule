:-consult(basic).
:-consult(data/small).

:-dynamic exam_with_students/3.
:-dynamic events_asserted/0.

violates_sc(schedule(Events), SoftConstraints):-
    preprocess,
    sort_events(Events, [], SortedEvents),
    violates_simple_sc(SortedEvents, SoftConstraints),
    %violates_combo_sc(SortedEvents, ComboConstraints),
    %append(SimpleConstraints, ComboConstraints, SoftConstraints),
    retract_preprocess.

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
           (    during_lunch_break(EID),
                is_on_site(PID, EID), %the person needs to follow/teach the course
                sc_lunch_break(PID,Penalty)),
            SoftConstraints).

%------------------------not in period and no exam in period--------------------------
no_exam_in_period_violations(SoftConstraints):-
    findall(sc_no_exam_in_period(LID,Day,From,Till,Penalty),
           (    event(EID, _, _, Start, End),
                teacher_of_exam(EID, LID),
                sc_no_exam_in_period(LID,Day,From,Till,Penalty),
                between(Start, End, Hour),
                between(From, Till, Hour)),
            SoftConstraints).

not_in_period_violations(SoftConstraints):-
    findall(sc_not_in_period(PID,Day,From,Till,Penalty),
           (    event(EID, _, _, Start, End),
                is_on_site(PID, EID),
                sc_not_in_period(PID,EID, Day,From,Till,Penalty),
                between(Start, End, Hour),
                between(From, Till, Hour)),
            SoftConstraints).

%------------------------same day and b2b--------------------------
same_day_violations(SoftConstraints):-
    findall(sc_same_day(PID,EID1,EID2,Penalty),
           (    event(EID1, _, Day, _, _),
                is_on_site(PID, EID1),
                event(EID2, _, Day, _, _),
                EID1 @< EID2, %otherwise we punish twice for the same crime
                is_on_site(PID, EID2)),
           SoftConstraints).

back_to_back_violations(SoftConstraints):-
    findall(sc_b2b(PID,EID1,EID2,Penalty),
           (    event(EID1, _, Day, Start, _),
                is_on_site(PID, EID1),
                events(EID2, _, Day, _, End)),
           SoftConstraints).


%----------------------------------------------------------------------------------------------------
study_period_constraints():-


% schedule([event(e1, r2, 1, 10), event(e2, r2, 2, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]) .
% violates_sc(schedule([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]), X).
% violates_sc_study_period(s2, [event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)], X).
% s2 => [e1, e2, e3, e5].
% =>      2,  1,  1,  1

% assert_events([event(e1, r2, 1, 10), event(e2, r2, 1, 10), event(e3, r1, 3, 10), event(e4, r1, 3, 11), event(e5, r2, 3, 12)]).
% findall(event(EID, RID, Day, Start, End), event(EID, RID, Day, Start, End), Schedule).

%member(event(E1,R1,D,_,H),Events),
%member(event(E2,R2,D,H,_),Events),
