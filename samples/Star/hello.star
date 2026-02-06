StarName Galaxy.Welcome;

// Main constellation demonstrating Star Language syntax v1.0
Constellation Greeter {
    Public String Message;
    
    // Constructor-like setup
    Public StarFunction Init(String name) {
        this.Message = "Hello, " + name + "! Welcome to the cosmos.";
    }

    Public StarFunction Speak() {
        EmitLn(this.Message);
    }
}

StarFunction Main() {
    // Variable declarations
    Int count = 10;
    Bool isReady = true;

    If (isReady) {
        Greeter g = new Greeter();
        g.Init("Cadet");
        g.Speak();
        
        While (count > 0) {
            Emit("T-minus " + count + "... ");
            count = count - 1;
        }
        EmitLn("Blastoff! 🚀");
    }
}
