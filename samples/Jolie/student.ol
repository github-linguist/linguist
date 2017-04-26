include "common.iol"
include "ui/swing_ui.iol"
include "console.iol"

outputPort Exam {
Location: Location_Exam
Protocol: sodep
Interfaces: ExamInterface
}

inputPort StudentInput {
Location: "socket://localhost:8001/"
Protocol: sodep
Interfaces: StudentInterface
}

main
{
	request.studentName = "John";
	request.examName = "SPLG";
	request.student.location = "socket://localhost:8001/";
	request.student.protocol = "sodep";
	startExam@Exam( request );
	makeQuestion( question )( answer ) {
		showYesNoQuestionDialog@SwingUI( question.question )( answer )
	};
	sendMessage( message );
	println@Console( message )()
}