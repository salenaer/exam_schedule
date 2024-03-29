%Sander Lenaerts
%29/12/2015
%Calculate the cost of given valid schedule

:-module(cost, [cost/2]).
:-use_module(basic).
:-use_module(violates_sc, [violates_sc/2]).

%cost(+Schedule, -Cost)
cost(Schedule,Cost):-
	violates_sc(Schedule, Violations),
	get_cost(Violations, 0, 0, Cost).

%get_cost(+violations, +studentCost, +TeacherCost, -TotalCost)
%violations are of one destinct time => no need to backtrackt
%takes the correct part of the functor and add it to the correct iterator

%we have no more violations normalize the iterators and calculate the final cost.
get_cost([], StudentItt, LecturerItt, Cost):-
	findall(SID, student(SID, _), Students),
	findall(LID, lecturer(LID, _), Lecturers),
	length(Students, NumberOfStudents),
	length(Lecturers, NumberOfLecturers),
	StudentCost is StudentItt/NumberOfStudents,
	LecturerCost is LecturerItt/NumberOfLecturers,
	Cost is (StudentCost + LecturerCost)/2.

get_cost([sc_lunch_break(PID, _, Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	increase_cost(PID, StudentItt, LecturerItt, Penalty, NewStudentItt, NewLecturerItt),
	get_cost(Violations, NewStudentItt, NewLecturerItt, Cost),
	!.

get_cost([sc_no_exam_in_period(_,_,_,_,_,Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	NewLecturerItt is LecturerItt + Penalty,
	get_cost(Violations, StudentItt, NewLecturerItt, Cost),
	!.

get_cost([sc_not_in_period(PID,_,_,_,_,Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	increase_cost(PID, StudentItt, LecturerItt, Penalty, NewStudentItt, NewLecturerItt),
	get_cost(Violations, NewStudentItt, NewLecturerItt, Cost),
	!.

get_cost([sc_same_day(PID,_,_,Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	increase_cost(PID, StudentItt, LecturerItt, Penalty, NewStudentItt, NewLecturerItt),
	get_cost(Violations, NewStudentItt, NewLecturerItt, Cost),
	!.

get_cost([sc_b2b(PID,_,_,Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	increase_cost(PID, StudentItt, LecturerItt, Penalty, NewStudentItt, NewLecturerItt),
	get_cost(Violations, NewStudentItt, NewLecturerItt, Cost),
	!.

get_cost([sc_study_time(_,_,Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	NewStudentItt is StudentItt + Penalty,
	get_cost(Violations, NewStudentItt, LecturerItt, Cost),
	!.

get_cost([sc_correction_time(_,_,Penalty)|Violations], StudentItt, LecturerItt, Cost):-
	NewLecturerItt is LecturerItt + Penalty,
	get_cost(Violations, StudentItt, NewLecturerItt, Cost).

increase_cost(PID, StudentItt, LecturerItt, Penalty, NewStudentItt, LecturerItt):-
	student(PID, _),
	!,
	NewStudentItt is StudentItt + Penalty.

increase_cost(PID, StudentItt, LecturerItt, Penalty, StudentItt, NewLecturerItt):-
	lecturer(PID, _),
	NewLecturerItt is LecturerItt + Penalty.