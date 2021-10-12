// https://github.com/aaronclinger/casalib/blob/a948190f3c9f781dfb1420fb24d36e7c1deeacf8/AS2/code/org/casalib/util/NumberUtil.as

/*
	CASA Lib for ActionScript 2.0
	Copyright (c) 2008, Aaron Clinger & Contributors of CASA Lib
	All rights reserved.
	
	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions are met:
	
	- Redistributions of source code must retain the above copyright notice,
	  this list of conditions and the following disclaimer.
	
	- Redistributions in binary form must reproduce the above copyright notice,
	  this list of conditions and the following disclaimer in the documentation
	  and/or other materials provided with the distribution.
	
	- Neither the name of the CASA Lib nor the names of its contributors
	  may be used to endorse or promote products derived from this software
	  without specific prior written permission.
	
	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
	AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
	ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
	LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
	CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
	SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
	CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
	ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
	POSSIBILITY OF SUCH DAMAGE.
*/

/**
	@author Aaron Clinger
	@author David Nelson
	@version 08/24/07
*/

class org.casalib.util.NumberUtil {
	
	/**
		Evaluates <code>val1</code> and <code>val2</code> and returns the smaller value. Unlike <code>Math.min</code> this method will return the defined value if the other value is <code>undefined</code>.
		
		@param val1: A number or expression to compare.
		@param val2: A number or expression to compare.
		@return Returns the smallest value, or the value out of the two that is defined.
	*/
	public static function min(val1:Number, val2:Number):Number {
		if (val1 == undefined || val2 == undefined)
			return (val2 == undefined) ? val1 : val2;
		
		return Math.min(val1, val2);
	}
	
	/**
		Evaluates <code>val1</code> and <code>val2</code> and returns the larger value. Unlike <code>Math.max</code> this method will return the defined value if the other value is <code>undefined</code>.
		
		@param val1: A number or expression to compare.
		@param val2: A number or expression to compare.
		@return Returns the largest value, or the value out of the two that is defined.
	*/
	public static function max(val1:Number, val2:Number):Number {
		if (val1 == undefined || val2 == undefined) 
			return (val2 == undefined) ? val1 : val2;
		
		return Math.max(val1, val2);
	}
	
	/**
		Creates a random integer within the defined range.
		
		@param min: The minimum number the random integer can be.
		@param min: The maximum number the random integer can be.
		@return Returns a random integer within the range.
	*/
	public static function randomInteger(min:Number, max:Number):Number {
		return min + Math.floor(Math.random() * (max + 1 - min));
	}
	
	/**
		Determines if the number is even.
		
		@param num: A number to determine if it is divisible by <code>2</code>.
		@return Returns <code>true</code> if the number is even; otherwise <code>false</code>.
	*/
	public static function isEven(num:Number):Boolean {
		return (num & 1) == 0;
	}
	
	/**
		Determines if the number is odd.
		
		@param num: A number to determine if it is not divisible by <code>2</code>.
		@return Returns <code>true</code> if the number is odd; otherwise <code>false</code>.
	*/
	public static function isOdd(num:Number):Boolean {
		return !NumberUtil.isEven(num);
	}
	
	/**
		Determines if the number is an integer.
		
		@param num: A number to determine if it contains no decimal values.
		@return Returns <code>true</code> if the number is an integer; otherwise <code>false</code>.
	*/
	public static function isInteger(num:Number):Boolean {
		return (num % 1) == 0;
	}
	
	/**
		Determines if the number is prime.
		
		@param num: A number to determine if it is only divisible by 1 and itself.
		@return Returns <code>true</code> if the number is prime; otherwise <code>false</code>.
	*/
	public static function isPrime(num:Number):Boolean {
		if (num == 1 || num == 2)
			return true;
		
		if (NumberUtil.isEven(num))
			return false;
		
		var s:Number = Math.sqrt(num);
		for (var i:Number = 3; i <= s; i++)
			if (num % i == 0)
				return false;
		
		return true;
	}
	
	/**
		Rounds a number's decimal value to a specific place.
		
		@param num: The number to round.
		@param place: The decimal place to round.
		@return Returns the value rounded to the defined place. 
		@example
			<code>
				trace(NumberUtil.roundToPlace(3.14159, 2)); // Traces 3.14
				trace(NumberUtil.roundToPlace(3.14159, 3)); // Traces 3.142
			</code>
	*/
	public static function roundDecimalToPlace(num:Number, place:Number):Number {
		var p:Number = Math.pow(10, Math.round(place));
		
		return Math.round(num * p) / p;
	}
	
	/**
		Determines if the value is included within a range.
		
		@param num: Number to determine if it's included in the range.
		@param startValue: First value of the range.
		@param endValue: Second value of the range.
		@return Returns <code>true</code> if the number falls within the range; otherwise <code>false</code>.
	*/
	public static function isBetween(num:Number, startValue:Number, endValue:Number):Boolean {
		return !(num < Math.min(startValue, endValue) || num > Math.max(startValue, endValue));
	}
	
	/**
		Determines if value falls within a range; if not it is snapped to the nearest range value.
		
		@param num: Number to determine if it's included in the range.
		@param startValue: First value of the range.
		@param endValue: Second value of the range.
		@return Returns either the number as passed, or its value once snapped to nearest range value.
	*/
	public static function makeBetween(num:Number, startValue:Number, endValue:Number):Number {
		return Math.min(Math.max(num, Math.min(startValue, endValue)), Math.max(startValue, endValue));
	}
	
	/**
		Creates evenly spaced numerical increments between two numbers.
		
		@param begin: The starting value.
		@param end: The ending value.
		@param steps: The number of increments between the starting and ending values.
		@return Returns an Array comprised of the increments between the two values.
		@example
			<code>
				trace(NumberUtil.createStepsBetween(0, 5, 4)); // Traces 1,2,3,4
				trace(NumberUtil.createStepsBetween(1, 3, 3)); // Traces 1.5,2,2.5
			</code>
	*/
	public static function createStepsBetween(begin:Number, end:Number, steps:Number): /*Number*/ Array {
		steps++;
		
		var i:Number = 0;
		var stepsBetween: /*Number*/ Array = new Array();
		var increment:Number = (end - begin) / steps;
		
		while (++i < steps)
			stepsBetween.push((i * increment) + begin);
		
		return stepsBetween;
	}
	
	/**
		Formats a number.
		
		@param numberToFormat: The number you wish to format.
		@param minLength: The minimum length of the number.
		@param thouDelim: <strong>[optional]</strong> The character used to seperate thousands; defaults to none.
		@param fillChar: <strong>[optional]</strong> The leading character used to make the number the minimum length; defaults to <code>0</code>.
		@return Returns the formated number as a String.
		@example
			<code>
				trace(NumberUtil.format(1234567, 8, ",")); // Traces 01,234,567
			</code>
	*/
	public static function format(numberToFormat:Number, minLength:Number, thouDelim:String, fillChar:String):String {
		var num:String = numberToFormat.toString();
		var len:Number = num.length;
		
		if (thouDelim != undefined) {
			var numSplit:Array = num.split('');
			var counter:Number = 3;
			var i:Number       = numSplit.length;
			
			while (--i > 0) {
				counter--;
				if (counter == 0) {
					counter = 3;
					numSplit.splice(i, 0, thouDelim);
				}
			}
			
			num = numSplit.join('');
		}
		
		if (minLength != undefined) {
			if (len < minLength) {
				minLength -= len;
				
				var addChar:String = (fillChar == undefined) ? '0' : fillChar;
				
				while (minLength--)
					num = addChar + num;
			}
		}
		
		return num;
	}
	
	/**
		Finds the English ordinal suffix for the number given.
		
		@param num: Number to find the ordinal suffix of.
		@return Returns the suffix for the number, 2 characters.
		@example
			<code>
				trace(32 + NumberUtil.getOrdinalSuffix(32)); // Traces 32nd
			</code>
	*/
	public static function getOrdinalSuffix(num:Number):String {
		if (num >= 10 && num <= 20)
			return 'th';
			
		switch (num % 10) {
			case 0 :
			case 4 :
			case 5 :
			case 6 :
			case 7 :
			case 8 :
			case 9 :
				return 'th';
			case 3 :
				return 'rd';
			case 2 :
				return 'nd';
			case 1 :
				return 'st';
		}
	}
	
	/**
		Adds a leading zero for numbers less than ten.
		
		@param num: Number to add leading zero.
		@return Number as a String; if the number was less than ten the number will have a leading zero.
	*/
	public static function addLeadingZero(num:Number):String {
		return (num < 10) ? '0' + num : num.toString();
	}
	
	private function NumberUtil() {} // Prevents instance creation
}