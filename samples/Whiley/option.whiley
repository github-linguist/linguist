package std

/**
 * Define none as a constrained form of null.
 */
public type None is null where true

/**
 * Define option component separately so that it can be used for
 * pattern matching.
 */
public type Some<T> is { T value } where true

/**
 * Encode option as either nothing, or some value.
 */
public type Option<T> is None | Some<T>

/**
 * Static value for None
 */
public final None None = null

/**
 * Constructor for Some values.
 */
public function Some<T>(T value) -> (Some<T> r)
// Value return matches value given
ensures r.value == value:
    //
    return { value: value }

/**
 * Check whether option is Some value containing the given value.
 */
public function contains<T>(Option<T> option, T value) -> (bool r)
// If option is some value, then check whether matches given value
ensures (option is Some<T>) ==> (r <==> (option.value == value))
// Otherwise, fail
ensures (option is None) ==> !r:
    //
    if option is None:
        return false
    else:
        return option.value == value

/**
 * Unwrap the option value using a default for the None case.
 */
public function unwrap<T>(Option<T> option, T dEfault) -> (T r)
// If option is some value, then return that value
ensures (option is Some<T>) ==> (option.value == r)
// Otherwise, return default value
ensures (option is None) ==> (r == dEfault):
    //
    if option is None:
        return dEfault
    else:
        return option.value

/**
 * Map from one kind of option to another using a given projection
 * from values of the source type to those of the target type.
 */
public function map<S,T>(Option<S> option, function(S)->(T) fn) -> (Option<T> result)
// If option is some value, then result is mapping of that value
ensures (option is Some<S>) ==> (result is Some<T> && result.value == fn(option.value))
// Otherwise, result is none
ensures (option is None) ==> (result is None):
    //
    if option is None:
        return None
    else:
        return Some<T>(fn(option.value))


/**
 * Filter the value of an option.  That is, if the option has some
 *  value for which the filter is true, keep this.  Otherwise, return
 * None.
 */
public function filter<T>(Option<T> option, function(T)->(bool) filter) -> (Option<T> r)
ensures (option is Some<T> && filter(option.value)) <==> (r is Some<T> && r.value == option.value):
    //
    if !(option is None) && filter(option.value):
        return option
    else:
        return None
    
    