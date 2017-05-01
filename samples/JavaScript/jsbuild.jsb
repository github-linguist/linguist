jsb.library('mylibrary', jsb.STATIC_LIBRARY, function(libObject) {
    libObject.outputName = 'mylibrary';
    libObject.cflags = [ '-Wall' ];
    libObject.ldflags = [ '-pthread' ];
    libObject.includePaths = [ 'src/include' ];
    libObject.sources = [ 
        'src/main.cpp',
        'src/app.cpp'
    ];
});

jsb.build();
