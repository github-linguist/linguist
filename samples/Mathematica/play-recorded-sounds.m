a = Import["sound1.flac","FLAC"]; b = Import["sound2.flac","FLAC"];

ListPlay[a, {t, 0, 10}]; ListPlay[b, {t, 0, 10}];

ListPlay[{a,b}, {t, 0, 10}];

Stopping before the end can be done using the GUI or by reducing the parameter range of the ListPlay function.

While[True,ListPlay[{a,b}, {t, 0, 10}];]

ListPlay[{0.5*a, 0.3*b}, {t, 0, 10}];
