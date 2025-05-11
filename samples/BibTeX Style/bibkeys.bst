% BibTeX bibliography style bibkeys (used to be citekeys) 
% by David Kotz kotz@dartmouth.edu
% March 1994
% modified (distantly) from
  %   BibTeX standard bibliography style `alpha'
   % Version 0.99b (8-Dec-10 release) for BibTeX versions 0.99a or later.
   % Copyright (C) 1984, 1985, 1988, 2010 Howard Trickey and Oren Patashnik.
   % Unlimited copying and redistribution of this file are permitted as long as
   % it is unmodified.  Modifications (and redistribution of modified versions)
   % are also permitted, but only if the resulting file is renamed to something
   % besides btxbst.doc, plain.bst, unsrt.bst, alpha.bst, and abbrv.bst.
   % This restriction helps ensure that all standard styles are identical.
   % The file btxbst.doc has the documentation for this style.

	 ENTRY { author } {} {}

	 FUNCTION {article} {}
	 FUNCTION {book} {}
	 FUNCTION {booklet} {}
	 FUNCTION {inbook} {}
	 FUNCTION {incollection} {}
	 FUNCTION {inproceedings} {}
	 FUNCTION {conference} {}
	 FUNCTION {manual} {}
	 FUNCTION {mastersthesis} {}
	 FUNCTION {misc} {}
	 FUNCTION {phdthesis} {}
	 FUNCTION {proceedings} {}
	 FUNCTION {techreport} {}
	 FUNCTION {unpublished} {}
	 FUNCTION {default.type} { misc }
	 
	 MACRO {jan} {"January"}
	 MACRO {feb} {"February"}
	 MACRO {mar} {"March"}
	 MACRO {apr} {"April"}
	 MACRO {may} {"May"}
	 MACRO {jun} {"June"}
	 MACRO {jul} {"July"}
	 MACRO {aug} {"August"}
	 MACRO {sep} {"September"}
	 MACRO {oct} {"October"}
	 MACRO {nov} {"November"}
	 MACRO {dec} {"December"}
	 
	 READ
	 
	 FUNCTION {presort}
	 { cite$
		 #1 entry.max$ substring$
		 'sort.key$ :=
	 }
	 
	 ITERATE {presort}
	 
	 SORT
	 
	 FUNCTION {cite.key.only} { cite$ write$ newline$ }
	 
	 ITERATE {cite.key.only}
	 