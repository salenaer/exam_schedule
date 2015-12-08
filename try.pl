:-consult(basic).

goodRoom(room(RID,RoomName), exam(EID, ExamName)):-
	room(RID,RoomName), %to check if name matches rome
	exam(EID, ExamName), %to check if name matches exam
	timesMatch(room(RID,_), exam(EID, _)),
	capacityMatch(room(RID,_), exam(EID, _)).

timesMatch(room(RID, _), exam(EID, _)):-
	duration(EID, ExamDuration),
	availability(RID,_,From,Till),
	RoomAvailabilty is Till-From,
	ExamDuration < RoomAvailabilty.

capacityMatch(room(RID, _), exam(EID, _)):-
	capacity(RID, RoomCapacity),
	has_exam(CID, EID),
	findall(SID, follows(SID,CID), Students),
	length(Students, NumberOfStudents),
	RoomCapacity >= NumberOfStudents.

isTimeSlot(RID, date(Day, Hour)):-
	availability(RID, Day, From, Till),
	isTimeSlot(From, Till, Hour).

isTimeSlot(From, Till, Hour):-
	Hour is From,
	Hour < Till.

isTimeSlot(From, Till, Hour):-
	Hour is From + 1,
	Hour < Till,
	isTimeSlot(Hour, Till, Hour).

%twee examens vinden op hetzelfde moment plaats als: het begin van het eerste examen + de lengte van het eerste examen
% later is dan de start van het tweede examen.
sameTime(event(EID1,_,Day,Start1), event(EID2,_,Day,Start2)):-
	Start1 <= Start2,
	duration(EID1, ExamDuration),
	Start1 + ExamDuration > Start2.

sameTime(event(EID1,_,Day,Start1), event(EID2,_,Day,Start2)):-
	Start1 > Start2,
	duration(EID2, ExamDuration),
	Start2 + ExamDuration > Start1.

sameTeacher(EID1, EID2):-
	teacherOfExam(EID1, LID), teacherOfExam(EID2, LID).

sameStudent(EID1, EID2):-
	has_exam(CID1, EID1),
	has_exam(CID2, EID2),
	findall(StudendID, follows(SID1, CID1), Students1),

sameStudentIn(CID2, StudentList):-
	follows(SID2, CID2),
	elementOf(SID2, StudentList).

%two events clash when they fire at same time and (have same teacher OR have same student OR have same room)
examsClash(event(EID1,RID1,Day,Start1), event(EID2,RID2,Day,Start2)):-
	sameTime(event(EID1,RID1,Day,Start1), event(EID2,RID2,Day,Start2)),
	(RID1 = RID2 ;   %same room
		sameTeacher(EID1, EID2);
		sameStudent(EID1, EID2)).

		