include "common.iol"

cset {
studentName:
	StartExamRequest.studentName
	DecisionMessage.studentName
	MakeQuestionRequest.studentName,
examName:
	StartExamRequest.examName
	DecisionMessage.examName
	MakeQuestionRequest.examName
}

execution { concurrent }

outputPort Student {
Interfaces: StudentInterface
}

inputPort ExamInput {
Location: Location_Exam
Protocol: sodep
Interfaces: ExamInterface
}

main
{
	startExam( examRequest );
	Student << examRequest.student;
	makeQuestion( question )( answer ) {
		makeQuestion@Student( question )( answer )
	};
	[ pass( message ) ] {
		sendMessage@Student( "You passed!" )
	}
	[ fail( message ) ] {
		sendMessage@Student( "You failed!" )
	}
}