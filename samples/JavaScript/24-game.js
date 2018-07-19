String.prototype.replaceAll = function(patt,repl) { var that = this;
              	                                    that     = that.replace(patt,repl);
						    if (that.search(patt) != -1) {
							that = that.replaceAll(patt,repl);
						    }
						    return that;
						  };

function validChars(input) { var regInvalidChar = /[^\d\+\*\/\s-\(\)]/;
                             return input.search(regInvalidChar) == -1;
                           }

function validNums(str, nums) {
    var arr, l;
    arr = str.replaceAll(/[^\d\s]/," ").replaceAll("  "," ").trim().split(" ").sort();
    l   = arr.length;

    while(l--) { arr[l] = Number(arr[l]); }

    return _.isEqual(arr,nums.sort());
}

function validEval(input) { try { eval(input); } catch (e) { return false; };
			    return true;
			  }

var input;

while(true){ var numbers = [];
             var i = 4;
             while(i--) { numbers.push(Math.floor(Math.random()*8+1));
                        }

             input = prompt("Your numbers are:\n"
                           + numbers.join(" ")
                           + "\nEnter expression. (use only + - * / and parens).\n"
                           + "'x' to exit."
                           );

             if (input === 'x') break;

               !validChars(input)                 ? alert("Invalid chars used, try again. Use only:\n + - * / ( )")
             : !validNums(input,numbers)          ? alert("Wrong numbers used, try again.")
	     : !validEval(input)                  ? alert("Could not evaluate input, try again.")
             : eval(input) != 24                  ? alert("Wrong answer:" + eval(input) + "\nTry again.")
             : alert(input + "== 24. Congrats!!")
             ;
           }
