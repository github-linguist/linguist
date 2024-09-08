script "com.livecode.library.minifyScript"

on extensionInitialize
   if the target is not me then
      pass "extensionInitialize"
   end if

   set the _ideOverride of me to true
   insert the script of me into back
end extensionInitialize

on extensionFinalize
   if the target is not me then
      pass "extensionFinalize"
   end if

   remove the script of me from back
end extensionFinalize

/**
Type: library

Title: Minify script

Author: torocruzand

Version: 0.0.1

SVGIcon: m21.706 5.292-2.999-2.999A.996.996 0 0 0 18 2H6a.997.997 0 0 0-.707.293L2.294 5.292A.996.996 0 0 0 2 6v13c0 1.103.897 2 2 2h16c1.103 0 2-.897 2-2V6a.994.994 0 0 0-.294-.708zM6.414 4h11.172l1 1H5.414l1-1zM12 18l-5-5h3v-3h4v3h3l-5 5z

OS: Mac, Windows, Linux, iOS, Android

Platforms: desktop, mobile, web

Summary: The “minify script” library is a tool for LiveCode that reduces stack
file sizes by eliminating comments and spaces from control code, enhancing
efficiency but reducing human readability.

Description:
The “minify script” library is designed to reduce the size of stack files
in LiveCode. This library removes comments and spaces from the code of controls,
resulting in smaller stack sizes. However, this also makes the code more
difficult for humans to read. Therefore, it’s important to maintain a version
of the code that includes all comments and other elements for readability and
future reference.
*/

/**
Minifies the script of a control.

Syntax: minifyScript <pLongId>

Description:
Use the <minifyScript> command to minify the script of a control.
This command sets the script of the control to the minified version
of the script.

Parameters:
pLongId (string): The long ID of the control.

Example:
minifyScript the long id of button "button 1", true
-- The script of button 1 is minified and obfuscated
*/
command minifyScript pLongId
   if there is a not pLongId then
       return "control not found"
   end if

   try
      set the script of pLongId to getMinifiedScript(pLongId)
   end try
end minifyScript


/**
Minifies a script and returns the minified version.

Syntax: getMinifiedScript <pLongId>

Description:
Use the <getMinifiedScript> function to minify a script.
This function removes comments and unnecessary whitespace from the script and
returns the minified version.

Parameters:
pLongId (string): The long ID of the control.

Returns:
The minified script.

Example:
put getMinifiedScript(the long id of button "button 1", true) into tMinifiedScript
-- The minified script is stored in tMinifiedScript
*/
function getMinifiedScript pLongId
  local tScript
  local tCommentOneLineChars = "//,--,#"
  local tCurrentLine = 0
  local tNumItems = 0
  local tBreak = 0
  local tCommentOneLine
  local tInsideQuote = false
  local tClosure = 0
  local tMinifiedScript = ""

  # Valid that it is an object.
  if there is a not pLongId then return empty

  # I take the object code
  put word 1 to - 1 of (the script of pLongId) into tScript

  # If you do not have empty return code.
  if tScript is empty then return empty

  set the itemDel to quote
  replace comma with cr in tCommentOneLineChars

  repeat for each line tLine in tScript
    add 1 to tCurrentLine
    put the num of items of tLine into tNumItems

    if tBreak is 0 then
      put word 1 to - 1 of tLine into tLine
      if tLine is empty then next repeat

      # Comment of a line.
      # To avoid doing everything for pleasure.
      if tLine contains "//" or tLine contains "-" or tLine contains "#" then

        repeat for each line tCommentChar in tCommentOneLineChars
          put offset(tCommentChar, tLine) into tCommentOneLine

          if tCommentOneLine is 0 then next repeat

          # If it's in quotes.
          put _insideQuote(tCommentOneLine, tLine) into tInsideQuote

          if tInsideQuote is not 1 then
            if tInsideQuote mod 2 is not 0 then
              delete char tCommentOneLine to - 1 of tLine
            end if
          else
            delete char tCommentOneLine to - 1 of tLine
          end if

          if tLine is empty then exit repeat
        end repeat

        if tLine is empty then next repeat
      end if

      # looking for the opening of multiline comments.
      put offSet("/*", tLine) into tBreak
      if tBreak is not 0 then

        # I have quotes.
        put _insideQuote(tBreak, tLine) into tInsideQuote

        # if it is not within the first item.
        if tInsideQuote is not 1 then
          # It's in quotes.
          if tInsideQuote mod 2 is not 0 then
            delete char tBreak to - 1 of tLine
          else
            put 0 into tBreak
          end if
        else

          # If it is not before the quotation marks or does not have
          # quotation marks, I am looking for the closure on this line.
          put offSet("*/", tLine) into tClosure

          if tClosure is 0 then
            delete char tBreak to - 1 of tLine
          else
            put tBreak + tClosure into tClosure
            delete char tBreak to tClosure of tLine
            put 0 into tBreak
          end if
        end if

        if tLine is empty then next repeat
      end if

      put tLine & return after tMinifiedScript
    else
      # Here I look for the closure of the multiline.
      put offSet("*/", tLine) into tClosure

      if tClosure is 0 then
        next repeat
      else
        put tClosure + 1 into tBreak
        delete char 1 to tBreak of tLine

        # I have quotes.
        put 0 into tBreak
        if tLine is empty then next repeat
        put tLine & return after tMinifiedScript
      end if
    end if
  end repeat

  # Delete the blank lines.
  filter tMinifiedScript without empty
  put tMinifiedScript into tScript
  put empty into tMinifiedScript

  repeat for each line tLine in tScript
    if "\" is last char of last word of tLine then
      delete  last char of last word of tLine
      put tLine & space after tMinifiedScript
    else
      put tLine & cr after tMinifiedScript
    end if
  end repeat
  
  # Return the code without comment.
  return tMinifiedScript
end getMinifiedScript


private function _insideQuote pNumChar, pText
   set the itemDel to quote
   return the num of items of the char 1 to pNumChar of pText
end _insideQuote
