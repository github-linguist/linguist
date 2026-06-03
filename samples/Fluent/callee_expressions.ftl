## Callees in placeables.

function-callee-placeable = {FUNCTION()}
term-callee-placeable = {-term()}

# ERROR Messages cannot be parameterized.
message-callee-placeable = {message()}
# ERROR Equivalent to a MessageReference callee.
mixed-case-callee-placeable = {Function()}
# ERROR Message attributes cannot be parameterized.
message-attr-callee-placeable = {message.attr()}
# ERROR Term attributes may not be used in Placeables.
term-attr-callee-placeable = {-term.attr()}
# ERROR Variables cannot be parameterized.
variable-callee-placeable = {$variable()}


## Callees in selectors.

function-callee-selector = {FUNCTION() ->
   *[key] Value
}
term-attr-callee-selector = {-term.attr() ->
   *[key] Value
}

# ERROR Messages cannot be parameterized.
message-callee-selector = {message() ->
   *[key] Value
}
# ERROR Equivalent to a MessageReference callee.
mixed-case-callee-selector = {Function() ->
   *[key] Value
}
# ERROR Message attributes cannot be parameterized.
message-attr-callee-selector = {message.attr() ->
   *[key] Value
}
# ERROR Term values may not be used as selectors.
term-callee-selector = {-term() ->
   *[key] Value
}
# ERROR Variables cannot be parameterized.
variable-callee-selector = {$variable() ->
   *[key] Value
}
