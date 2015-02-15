:- object(thing,
    instantiates(thing)).
:- end_object.

:- object(animal,
    specializes(thing)).
    ...
:- end_object.

:- object(dog,
    specializes(animal)).
    ...
:- end_object.

:- object(cat,
    specializes(animal)).
    ...
:- end_object.

:- object(lab,
    specializes(dog)).
    ...
:- end_object.

:- object(collie,
    specializes(dog)).
    ...
:- end_object.
