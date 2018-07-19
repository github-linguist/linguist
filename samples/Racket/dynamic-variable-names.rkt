-> (begin (printf "Enter some name: ")
          (namespace-set-variable-value! (read) 123))
Enter some name: bleh
-> bleh
123
