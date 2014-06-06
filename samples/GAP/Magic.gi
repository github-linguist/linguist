#############################################################################
##
##  Magic.gi                                         AutoDoc package
##
##  Copyright 2013, Max Horn, JLU Giessen
##                  Sebastian Gutsche, University of Kaiserslautern
##
#############################################################################

# Check if a string has the given suffix or not. Another
# name for this would "StringEndsWithOtherString".
# For example, AUTODOC_HasSuffix("file.gi", ".gi") returns
# true while AUTODOC_HasSuffix("file.txt", ".gi") returns false.
BindGlobal( "AUTODOC_HasSuffix",
function(str, suffix)
    local n, m;
    n := Length(str);
    m := Length(suffix);
    return n >= m and str{[n-m+1..n]} = suffix;
end );

# Given a string containing a ".", , return its suffix,
# i.e. the bit after the last ".". For example, given "test.txt",
# it returns "txt".
BindGlobal( "AUTODOC_GetSuffix",
function(str)
    local i;
    i := Length(str);
    while i > 0 and str[i] <> '.' do i := i - 1; od;
    if i < 0 then return ""; fi;
    return str{[i+1..Length(str)]};
end );

# Check whether the given directory exists, and if not, attempt
# to create it.
BindGlobal( "AUTODOC_CreateDirIfMissing",
function(d)
    local tmp;
    if not IsDirectoryPath(d) then
        tmp := CreateDir(d); # Note: CreateDir is currently undocumented
        if tmp = fail then
            Error("Cannot create directory ", d, "\n",
                  "Error message: ", LastSystemError().message, "\n");
            return false;
        fi;
    fi;
    return true;
end );


# Scan the given (by name) subdirs of a package dir for
# files with one of the given extensions, and return the corresponding
# filenames, as relative paths (relative to the package dir).
#
# For example, the invocation
#   AUTODOC_FindMatchingFiles("AutoDoc", [ "gap/" ], [ "gi", "gd" ]);
# might return a list looking like
#  [ "gap/AutoDocMainFunction.gd", "gap/AutoDocMainFunction.gi", ... ]
BindGlobal( "AUTODOC_FindMatchingFiles",
function (pkg, subdirs, extensions)
    local d_rel, d, tmp, files, result;

    result := [];

    for d_rel in subdirs do
        # Get the absolute path to the directory in side the package...
        d := DirectoriesPackageLibrary( pkg, d_rel );
        if IsEmpty( d ) then
            continue;
        fi;
        d := d[1];
        # ... but also keep the relative path (such as "gap")
        d_rel := Directory( d_rel );

        files := DirectoryContents( d );
        Sort( files );
        for tmp in files do
            if not AUTODOC_GetSuffix( tmp ) in [ "g", "gi", "gd", "autodoc" ] then
                continue;
            fi;
            if not IsReadableFile( Filename( d, tmp ) ) then
                continue;
            fi;
            Add( result, Filename( d_rel, tmp ) );
        od;
    od;
    return result;
end );


# AutoDoc(pkg[, opt])
#
## Make this function callable with the package_name AutoDocWorksheet.
## Which will then create a worksheet!
InstallGlobalFunction( AutoDoc,
function( arg )
    local pkg, package_info, opt, scaffold, gapdoc, maketest,
          autodoc, pkg_dir, doc_dir, doc_dir_rel, d, tmp,
          title_page, tree, is_worksheet, position_document_class, i, gapdoc_latex_option_record;
    
    pkg := arg[1];
    
    if LowercaseString( pkg ) = "autodocworksheet" then
        is_worksheet := true;
        package_info := rec( );
        pkg_dir := DirectoryCurrent( );
    else
        is_worksheet := false;
        package_info := PackageInfo( pkg )[ 1 ];
        pkg_dir := DirectoriesPackageLibrary( pkg, "" )[1];
    fi;

    if Length(arg) >= 2 then
        opt := arg[2];
    else
        opt := rec();
    fi;

    # Check for certain user supplied options, and if present, add them
    # to the opt record.
    tmp := function( key )
        local val;
        val := ValueOption( key );
        if val <> fail then
            opt.(key) := val;
        fi;
    end;
    
    tmp( "dir" );
    tmp( "scaffold" );
    tmp( "autodoc" );
    tmp( "gapdoc" );
    tmp( "maketest" );
    
    #
    # Setup the output directory
    #
    if not IsBound( opt.dir ) then
        doc_dir := "doc";
    elif IsString( opt.dir ) or IsDirectory( opt.dir ) then
        doc_dir := opt.dir;
    else
        Error( "opt.dir must be a string containing a path, or a directory object" );
    fi;
    
    if IsString( doc_dir ) then
        # Record the relative version of the path
        doc_dir_rel := Directory( doc_dir );

        # We intentionally do not use
        #   DirectoriesPackageLibrary( pkg, "doc" )
        # because it returns an empty list if the subdirectory is missing.
        # But we want to handle that case by creating the directory.
        doc_dir := Filename(pkg_dir, doc_dir);
        doc_dir := Directory(doc_dir);

    else
        # TODO: doc_dir_rel = ... ?
    fi;

    # Ensure the output directory exists, create it if necessary
    AUTODOC_CreateDirIfMissing(Filename(doc_dir, ""));
    
    # Let the developer know where we are generating the documentation.
    # This helps diagnose problems where multiple instances of a package
    # are visible to GAP and the wrong one is used for generating the
    # documentation.
    # TODO: Using Info() instead of Print?
    Print( "Generating documentation in ", doc_dir, "\n" );

    #
    # Extract scaffolding settings, which can be controlled via
    # opt.scaffold or package_info.AutoDoc. The former has precedence.
    #
    if not IsBound(opt.scaffold) then
        # Default: enable scaffolding if and only if package_info.AutoDoc is present
        if IsBound( package_info.AutoDoc ) then
            scaffold := rec( );
        fi;
    elif IsRecord(opt.scaffold) then
        scaffold := opt.scaffold;
    elif IsBool(opt.scaffold) then
        if opt.scaffold = true then
            scaffold := rec();
        fi;
    else
        Error("opt.scaffold must be a bool or a record");
    fi;

    # Merge package_info.AutoDoc into scaffold
    if IsBound(scaffold) and IsBound( package_info.AutoDoc ) then
        AUTODOC_APPEND_RECORD_WRITEONCE( scaffold, package_info.AutoDoc );
    fi;
    
    if IsBound( scaffold ) then
        AUTODOC_WriteOnce( scaffold, "TitlePage", true );
        AUTODOC_WriteOnce( scaffold, "MainPage", true );
    fi;

    
    #
    # Extract AutoDoc settings
    #
    if not IsBound(opt.autodoc) and not is_worksheet then
        # Enable AutoDoc support if the package depends on AutoDoc.
        tmp := Concatenation( package_info.Dependencies.NeededOtherPackages,
                              package_info.Dependencies.SuggestedOtherPackages );
        if ForAny( tmp, x -> LowercaseString(x[1]) = "autodoc" ) then
            autodoc := rec();
        fi;
    elif IsRecord(opt.autodoc) then
        autodoc := opt.autodoc;
    elif IsBool(opt.autodoc) and opt.autodoc = true then
        autodoc := rec();
    fi;
    
    if IsBound(autodoc) then
        if not IsBound( autodoc.files ) then
            autodoc.files := [ ];
        fi;
        
        if not IsBound( autodoc.scan_dirs ) and not is_worksheet then
            autodoc.scan_dirs := [ "gap", "lib", "examples", "examples/doc" ];
        elif not IsBound( autodoc.scan_dirs ) and is_worksheet then
            autodoc.scan_dirs := [ ];
        fi;
        
        if not IsBound( autodoc.level ) then
            autodoc.level := 0;
        fi;
        
        PushOptions( rec( level_value := autodoc.level ) );
        
        if not is_worksheet then
            Append( autodoc.files, AUTODOC_FindMatchingFiles(pkg, autodoc.scan_dirs, [ "g", "gi", "gd" ]) );
        fi;
    fi;

    #
    # Extract GAPDoc settings
    #
    if not IsBound( opt.gapdoc ) then
        # Enable GAPDoc support by default
        gapdoc := rec();
    elif IsRecord( opt.gapdoc ) then
        gapdoc := opt.gapdoc;
    elif IsBool( opt.gapdoc ) and opt.gapdoc = true then
        gapdoc := rec();
    fi;
    
    #
    # Extract test settings
    #
    
    if IsBound( opt.maketest ) then
        if IsRecord( opt.maketest ) then
            maketest := opt.maketest;
        elif opt.maketest = true then
            maketest := rec( );
        fi;
    fi;
    
    if IsBound( gapdoc ) then

        if not IsBound( gapdoc.main ) then
            gapdoc.main := pkg;
        fi;

        # FIXME: the following may break if a package uses more than one book
        if IsBound( package_info.PackageDoc ) and IsBound( package_info.PackageDoc[1].BookName ) then
            gapdoc.bookname := package_info.PackageDoc[1].BookName;
        elif not is_worksheet then
            # Default: book name = package name
            gapdoc.bookname := pkg;

            Print("\n");
            Print("WARNING: PackageInfo.g is missing a PackageDoc entry!\n");
            Print("Without this, your package manual will not be recognized by the GAP help system.\n");
            Print("You can correct this by adding the following to your PackageInfo.g:\n");
            Print("PackageDoc := rec(\n");
            Print("  BookName  := ~.PackageName,\n");
            #Print("  BookName  := \"", pkg, "\",\n");
            Print("  ArchiveURLSubset := [\"doc\"],\n");
            Print("  HTMLStart := \"doc/chap0.html\",\n");
            Print("  PDFFile   := \"doc/manual.pdf\",\n");
            Print("  SixFile   := \"doc/manual.six\",\n");
            Print("  LongTitle := ~.Subtitle,\n");
            Print("),\n");
            Print("\n");
        fi;

        if not IsBound( gapdoc.files ) then
            gapdoc.files := [];
        fi;

        if not IsBound( gapdoc.scan_dirs ) and not is_worksheet then
            gapdoc.scan_dirs := [ "gap", "lib", "examples", "examples/doc" ];
        fi;
        
        if not is_worksheet then
            Append( gapdoc.files, AUTODOC_FindMatchingFiles(pkg, gapdoc.scan_dirs, [ "g", "gi", "gd" ]) );
        fi;

        # Attempt to weed out duplicates as they may confuse GAPDoc (this
        # won't work if there are any non-normalized paths in the list).
        gapdoc.files := Set( gapdoc.files );
        
        # Convert the file paths in gapdoc.files, which are relative to
        # the package directory, to paths which are relative to the doc directory.
        # For this, we assume that doc_dir_rel is normalized (e.g.
        # it does not contains '//') and relative.
        d := Number( Filename( doc_dir_rel, "" ), x -> x = '/' );
        d := Concatenation( ListWithIdenticalEntries(d, "../") );
        gapdoc.files := List( gapdoc.files, f -> Concatenation( d, f ) );
    fi;
    
    
    # read tree
    # FIXME: shouldn't tree be declared inside of an 'if IsBound(autodoc)' section?
    tree := DocumentationTree( );
    
    if IsBound( autodoc ) then
        if IsBound( autodoc.section_intros ) then
            AUTODOC_PROCESS_INTRO_STRINGS( autodoc.section_intros : Tree := tree );
        fi;
    
        AutoDocScanFiles( autodoc.files : PackageName := pkg, Tree := tree );
    fi;
    
    if is_worksheet then
        # FIXME: We use scaffold and autodoc here without checking whether
        # they are bound. Does that mean worksheets always use them?
        if IsRecord( scaffold.TitlePage ) and IsBound( scaffold.TitlePage.Title ) then
            pkg := scaffold.TitlePage.Title;

        elif IsBound( tree!.TitlePage.Title ) then
            pkg := tree!.TitlePage.Title;

        elif IsBound( autodoc.files ) and Length( autodoc.files ) > 0  then
            pkg := autodoc.files[ 1 ];
            
            while Position( pkg, '/' ) <> fail do
                Remove( pkg, 1 );
            od;
            
            while Position( pkg, '.' ) <> fail do
                Remove( pkg, Length( pkg ) );
            od;

        else
            Error( "could not figure out a title." );
        fi;
        
        if not IsString( pkg ) then
            pkg := JoinStringsWithSeparator( pkg, " " );
        fi;
        
        gapdoc.main := ReplacedString( pkg, " ", "_" );
        gapdoc.bookname := ReplacedString( pkg, " ", "_" );
    fi;
    
    #
    # Generate scaffold
    #
    gapdoc_latex_option_record := rec( );
    
    if IsBound( scaffold ) then
        ## Syntax is [ "class", [ "options" ] ]
        if IsBound( scaffold.document_class ) then
            position_document_class := PositionSublist( GAPDoc2LaTeXProcs.Head, "documentclass" );
            
            if IsString( scaffold.document_class ) then
                scaffold.document_class := [ scaffold.document_class ];
            fi;
            
            if position_document_class = fail then
                Error( "something is wrong with the LaTeX header" );
            fi;
            
            GAPDoc2LaTeXProcs.Head := Concatenation(
                  GAPDoc2LaTeXProcs.Head{[ 1 .. PositionSublist( GAPDoc2LaTeXProcs.Head, "{", position_document_class ) ]},
                  scaffold.document_class[ 1 ],
                  GAPDoc2LaTeXProcs.Head{[ PositionSublist( GAPDoc2LaTeXProcs.Head, "}", position_document_class ) .. Length( GAPDoc2LaTeXProcs.Head ) ]} );
            
            if Length( scaffold.document_class ) = 2 then
                
                GAPDoc2LaTeXProcs.Head := Concatenation(
                      GAPDoc2LaTeXProcs.Head{[ 1 .. PositionSublist( GAPDoc2LaTeXProcs.Head, "[", position_document_class ) ]},
                      scaffold.document_class[ 2 ],
                      GAPDoc2LaTeXProcs.Head{[ PositionSublist( GAPDoc2LaTeXProcs.Head, "]", position_document_class ) .. Length( GAPDoc2LaTeXProcs.Head ) ]} );
            fi;
        fi;
        
        if IsBound( scaffold.latex_header_file ) then
            GAPDoc2LaTeXProcs.Head := StringFile( scaffold.latex_header_file );
        fi;
        
        if IsBound( scaffold.gapdoc_latex_options ) then
            if IsRecord( scaffold.gapdoc_latex_options ) then
                for i in RecNames( scaffold.gapdoc_latex_options ) do
                    if not IsString( scaffold.gapdoc_latex_options.( i ) )
                       and IsList( scaffold.gapdoc_latex_options.( i ) )
                       and LowercaseString( scaffold.gapdoc_latex_options.( i )[ 1 ] ) = "file" then
                        scaffold.gapdoc_latex_options.( i ) := StringFile( scaffold.gapdoc_latex_options.( i )[ 2 ] );
                    fi;
                od;
                
                gapdoc_latex_option_record := scaffold.gapdoc_latex_options;
            fi;
        fi;
        
        if not IsBound( scaffold.includes ) then
            scaffold.includes := [ ];
        fi;

        if IsBound( autodoc ) then
            # If scaffold.includes is already set, then we add
            # AutoDocMainFile.xml to it, but *only* if it not already
            # there. This way, package authors can control where
            # it is put in their includes list.
            if not "AutoDocMainFile.xml" in scaffold.includes then
                Add( scaffold.includes, "AutoDocMainFile.xml" );
            fi;
        fi;

        if IsBound( scaffold.bib ) and IsBool( scaffold.bib ) then
            if scaffold.bib = true then
                scaffold.bib := Concatenation( pkg, ".bib" );
            else
                Unbind( scaffold.bib );
            fi;
        elif not IsBound( scaffold.bib ) then
            # If there is a doc/PKG.bib file, assume that we want to reference it in the scaffold.
            if IsReadableFile( Filename( doc_dir, Concatenation( pkg, ".bib" ) ) ) then
                scaffold.bib := Concatenation( pkg, ".bib" );
            fi;
        fi;
        
        AUTODOC_WriteOnce( scaffold, "index", true );

        if IsBound( gapdoc ) then
            if AUTODOC_GetSuffix( gapdoc.main ) = "xml" then
                scaffold.main_xml_file := gapdoc.main;
            else
                scaffold.main_xml_file := Concatenation( gapdoc.main, ".xml" );
            fi;
        fi;

        # TODO: It should be possible to only rebuild the title page. (Perhaps also only the main page? but this is less important)
        if IsBound( scaffold.TitlePage ) then
            if IsRecord( scaffold.TitlePage ) then
                title_page := scaffold.TitlePage;
            else
                title_page := rec( );
            fi;
            
            AUTODOC_WriteOnce( title_page, "dir", doc_dir );
            AUTODOC_APPEND_RECORD_WRITEONCE( title_page, tree!.TitlePage );
            
            if not is_worksheet then
                AUTODOC_APPEND_RECORD_WRITEONCE( title_page, ExtractTitleInfoFromPackageInfo( pkg ) );
            fi;
            
            CreateTitlePage( title_page );
        fi;
        
        if IsBound( scaffold.MainPage ) and scaffold.MainPage <> false then
            scaffold.dir := doc_dir;
            scaffold.book_name := pkg;
            CreateMainPage( scaffold );
        fi;
    fi;
    
    #
    # Run AutoDoc
    #
    if IsBound( autodoc ) then
        WriteDocumentation( tree, doc_dir );
    fi;
    
    
    #
    # Run GAPDoc
    #
    if IsBound( gapdoc ) then

        # Ask GAPDoc to use UTF-8 as input encoding for LaTeX, as the XML files
        # of the documentation are also in UTF-8 encoding, and may contain characters
        # not contained in the default Latin 1 encoding.
        SetGapDocLaTeXOptions( "utf8", gapdoc_latex_option_record );

        MakeGAPDocDoc( doc_dir, gapdoc.main, gapdoc.files, gapdoc.bookname, "MathJax" );

        CopyHTMLStyleFiles( Filename( doc_dir, "" ) );

        # The following (undocumented) API is there for compatibility
        # with old-style gapmacro.tex based package manuals. It
        # produces a manual.lab file which those packages can use if
        # they wish to link to things in the manual we are currently
        # generating. This can probably be removed eventually, but for
        # now, doing it does not hurt.
        
        # FIXME: It seems that this command does not work if pdflatex
        #        is not present. Maybe we should remove it.
        
        if not is_worksheet then
            GAPDocManualLab( pkg );
        fi;

    fi;
    
    if IsBound( maketest ) then
        
        AUTODOC_WriteOnce( maketest, "filename", "maketest.g" );
        AUTODOC_WriteOnce( maketest, "folder", pkg_dir );
        AUTODOC_WriteOnce( maketest, "scan_dir", doc_dir );
        AUTODOC_WriteOnce( maketest, "files_to_scan", gapdoc.files );

        if IsString( maketest.folder ) then
            maketest.folder := Directory( maketest.folder );
        fi;
        
        if IsString( maketest.scan_dir ) then
            maketest.scan_dir := Directory( maketest.scan_dir );
        fi;
        
        AUTODOC_WriteOnce( maketest, "commands", [ ] );
        AUTODOC_WriteOnce( maketest, "book_name", gapdoc.main );
        
        CreateMakeTest( maketest );
    fi;

    return true;
end );
