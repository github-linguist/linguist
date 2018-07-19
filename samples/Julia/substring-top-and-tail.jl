julia> "My String"[2:end] # without first character
"y String"

julia> "My String"[1:end-1] # without last character
"My Strin"

julia> "My String"[2:end-1] # without first and last characters
"y Strin"
