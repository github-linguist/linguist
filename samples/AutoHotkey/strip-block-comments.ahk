code =
(
 /**
   * Some comments
   * longer comments here that we can parse.
   *
   * Rahoo
   */
   function subroutine() {
    a = /* inline comment */ b + c ;
   }
   /*/ <-- tricky comments */

   /**
    * Another comment.
    */
    function something() {
    }
)
;Open-Close Comment delimiters
	openC:="/*"
	closeC:="*/"
;Make it "Regex-Safe"
	openC:=RegExReplace(openC,"(\*|\^|\?|\\|\+|\.|\!|\{|\}|\[|\]|\$|\|)","\$0")
	closeC:=RegExReplace(closeC,"(\*|\^|\?|\\|\+|\.|\!|\{|\}|\[|\]|\$|\|)","\$0")
;Display final result
	MsgBox % sCode := RegExReplace(code,"s)(" . openC . ").*?(" . closeC . ")")
