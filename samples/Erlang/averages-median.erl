-module(median).
-import(lists, [nth/2, sort/1]).
-compile(export_all).

median(Unsorted) ->
    Sorted = sort(Unsorted),
    Length = length(Sorted),
    Mid = Length div 2,
    Rem = Length rem 2,
    (nth(Mid+Rem, Sorted) + nth(Mid+1, Sorted)) / 2.
