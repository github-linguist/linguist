include "types/Binding.iol"

constants {
	Location_Exam = "socket://localhost:8000"
}

type StartExamRequest:void {
	.examName:string
	.studentName:string
	.student:Binding
}

type MakeQuestionRequest:void {
	.question:string
	.examName:string
	.studentName:string
}

type DecisionMessage:void {
	.studentName:string
	.examName:string
}

interface ExamInterface {
OneWay:
	startExam(StartExamRequest),
	pass(DecisionMessage), fail(DecisionMessage)
RequestResponse:
	makeQuestion(MakeQuestionRequest)(int)
}

interface StudentInterface {
OneWay:
	sendMessage(string)
RequestResponse:
	makeQuestion(MakeQuestionRequest)(int)
}