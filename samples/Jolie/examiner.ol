include "common.iol"
include "ui/swing_ui.iol"
include "console.iol"

outputPort Exam {
Location: Location_Exam
Protocol: sodep
Interfaces: ExamInterface
}

main
{
	question.studentName = "John";
	question.examName = "SPLG";
	question.question = "Random question";
	makeQuestion@Exam( question )( answer );
	showYesNoQuestionDialog@SwingUI( "Do you want to accept answer " + answer + " ?" )( decision );

	message.studentName = "John";
	message.examName = "SPLG";
	if ( decision == 0 ) {
		pass@Exam( message )
	} else {
		fail@Exam( message )
	}
}
