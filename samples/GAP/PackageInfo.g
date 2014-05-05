#############################################################################
##  
##  PackageInfo.g for the package `cvec'                      Max Neunhoeffer
##
##  (created from Frank Lübeck's PackageInfo.g template file)
##  

SetPackageInfo( rec(

PackageName := "cvec",
Subtitle := "Compact vectors over finite fields",
Version := "2.5.1",
Date := "04/04/2014", # dd/mm/yyyy format

##  Information about authors and maintainers.
Persons := [
  rec( 
    LastName      := "Neunhoeffer",
    FirstNames    := "Max",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "neunhoef@mcs.st-and.ac.uk",
    WWWHome       := "http://www-groups.mcs.st-and.ac.uk/~neunhoef/",
    PostalAddress := Concatenation( [
                       "School of Mathematics and Statistics\n",
                       "University of St Andrews\n",
                       "Mathematical Institute\n",
                       "North Haugh\n",
                       "St Andrews, Fife KY16 9SS\n",
                       "Scotland, UK" ] ),
    Place         := "St Andrews",
    Institution   := "University of St Andrews"
  ),
],

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "deposited"     for packages for which the GAP developers agreed 
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages 
##    "other"         for all other packages
##
# Status := "accepted",
Status := "deposited",

##  You must provide the next two entries if and only if the status is 
##  "accepted" because is was successfully refereed:
# format: 'name (place)'
# CommunicatedBy := "Mike Atkinson (St. Andrews)",
#CommunicatedBy := "",
# format: mm/yyyy
# AcceptDate := "08/1999",
#AcceptDate := "",

PackageWWWHome := "http://neunhoef.github.io/cvec/",
README_URL     := Concatenation(~.PackageWWWHome, "README"),
PackageInfoURL := Concatenation(~.PackageWWWHome, "PackageInfo.g"),
ArchiveURL     := Concatenation("https://github.com/neunhoef/cvec/",
                                "releases/download/v", ~.Version,
                                "/cvec-", ~.Version),
ArchiveFormats := ".tar.gz .tar.bz2",

##  Here you  must provide a short abstract explaining the package content 
##  in HTML format (used on the package overview Web page) and an URL 
##  for a Webpage with more detailed information about the package
##  (not more than a few lines, less is ok):
##  Please, use '<span class="pkgname">GAP</span>' and
##  '<span class="pkgname">MyPKG</span>' for specifing package names.
##  
AbstractHTML := 
  "This package provides an implementation of compact vectors over finite\
   fields. Contrary to earlier implementations no table lookups are used\
   but only word-based processor arithmetic. This allows for bigger finite\
   fields and higher speed.",

PackageDoc := rec(
  BookName  := "cvec",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Compact vectors over finite fields",
),

Dependencies := rec(
  GAP := ">=4.5.5",
  NeededOtherPackages := [
    ["GAPDoc", ">= 1.2"],
    ["IO", ">= 4.1"],
    ["orb", ">= 4.2"],
  ],
  SuggestedOtherPackages := [],
  ExternalConditions := []
),

AvailabilityTest := function()
  if not "cvec" in SHOW_STAT() and
     Filename(DirectoriesPackagePrograms("cvec"), "cvec.so") = fail then
    #Info(InfoWarning, 1, "cvec: kernel cvec functions not available.");
    return fail;
  fi;
  return true;
end,

##  *Optional*, but recommended: path relative to package root to a file which 
##  contains as many tests of the package functionality as sensible.
#TestFile := "tst/testall.g",

##  *Optional*: Here you can list some keyword related to the topic 
##  of the package.
Keywords := []

));


