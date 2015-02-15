package {
    public class MyClass {

        private var myVariable:int;  // Note: instance variables are usually "private"

        /**
         * The constructor
         */
        public function MyClass() {
            // creates a new instance
        }

        /**
         * A method
         */
        public function someMethod():void {
            this.myVariable = 1; // Note: "this." is optional
            // myVariable = 1; works also
        }
    }
}
