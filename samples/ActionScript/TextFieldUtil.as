// https://github.com/aaronclinger/casalib/blob/a948190f3c9f781dfb1420fb24d36e7c1deeacf8/AS2/code/org/casalib/util/TextFieldUtil.as

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
	@author Mike Creighton
	@version 08/02/07
*/

class org.casalib.util.TextFieldUtil {
	
	/**
		Determines if textfield has more text than can be displayed at once.
		
		@param target_txt: Textfield or {@link CoreTextField} to check for text overflow.
		@return Returns <code>true</code> if textfield has overflow text; otherwise <code>false</code>.
	*/
	public static function hasOverFlow(target_txt:Object):Boolean {
		return target_txt.maxscroll > 1;
	}
	
	/**
		Removes text overflow on a plain text textfield with the option of an ommission indicator.
		
		@param target_txt: Textfield or {@link CoreTextField} to remove overflow.
		@param omissionIndicator: <strong>[optional]</strong> Text indication that an omission has occured; normally <code>"..."</code>; defaults to no indication.
		@return Returns the omitted text; if there was no text ommitted function returns a empty String (<code>""</code>).
	*/
	public static function removeOverFlow(target_txt:Object, omissionIndicator:String):String {
		if (!TextFieldUtil.hasOverFlow(target_txt))
			return '';
		
		if (omissionIndicator == undefined)
			omissionIndicator = '';
		
		var originalCopy:String        = target_txt.text;
		var lines:Array                = target_txt.text.split('. ');
		var isStillOverflowing:Boolean = false;
		var words:Array;
		var lastSentence:String;
		var sentences:String;
		var overFlow:String;
		
		while (TextFieldUtil.hasOverFlow(target_txt)) {
			lastSentence    = String(lines.pop());
			target_txt.text = (lines.length == 0) ? '' : lines.join('. ') + '. ';
		}
		
		sentences         = (lines.length == 0) ? '' : lines.join('. ') + '. ';
		words             = lastSentence.split(' ');
		target_txt.text  += lastSentence;
		
		while (TextFieldUtil.hasOverFlow(target_txt)) {
			if (words.length == 0) {
				isStillOverflowing = true;
				break;
			} else {
				words.pop();
				
				if (words.length == 0)
					target_txt.text = sentences.substr(0, -1) + omissionIndicator;
				else
					target_txt.text = sentences + words.join(' ') + omissionIndicator;
			}
		}
		
		if (isStillOverflowing) {
			words = target_txt.text.split(' ');
			
			while (TextFieldUtil.hasOverFlow(target_txt)) {
				words.pop();
				target_txt.text = words.join(' ') + omissionIndicator;
			}
		}
		
		overFlow = originalCopy.substring(target_txt.text.length);
		
		return (overFlow.charAt(0) == ' ') ? overFlow.substring(1) : overFlow;
	}
	
	private function TextFieldUtil() {} // Prevents instance creation
}
