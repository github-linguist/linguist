def remove_comments!(str, comment_start='/*', comment_end='*/')
  while start_idx = str.index(comment_start)
    end_idx = str.index(comment_end, start_idx + comment_start.length) + comment_end.length - 1
    str[start_idx .. end_idx] = ""
  end
  str
end

def remove_comments(str, comment_start='/*', comment_end='*/')
  remove_comments!(str.dup, comment_start, comment_end)
end

example = <<END_OF_STRING
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
END_OF_STRING

puts remove_comments example
