init
	new Demo( "Demonstration class" ).run()

class Demo
	_message:string = ""

	construct ( message:string = "Optional argument - no message passed in constructor" )
		_message = message

	def run()
		print( _message )
		
