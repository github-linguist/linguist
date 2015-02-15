> (apply str (take-while #(not (#{\# \;} %)) "apples # comment"))
"apples "
