CREATE OR REPLACE PACKAGE BODY PL_FPDF AS
/*******************************************************************************
* Logiciel : PL_FPDF                                                           *
* Version :  0.9.3                                                             *
* Date :     13/06/2006                                                        *
* Auteur :   Pierre-Gilles Levallois                                           *
* Licence :  GPL                                                               *
*                                                                              *
********************************************************************************
* Cette librairie PL/SQL est un portage de la version 1.53 de FPDF, célèbre    *
* classe PHP développée par Olivier PLATHEY (http://www.fpdf.org/)             *
********************************************************************************
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
********************************************************************************/

/*******************************************************************************
   TODO :
    - Known bugs :
    
    CHANGELOG : 
    0.9.2 -> 0.9.3 : 
        - Added Sample on setHeaderProc and setFooterProc procedures.
        - Added parameter implementation to thes procedures.
        - Modify Header and footer procedure behaviour to get parameter values
        - declared subtype 'word' ans type 'tv4000a' in the specs.
        
    0.9.1 -> 0.9.2 : 
        - Added procedure helloword Example.
        - Added procedure testImg Example.

*******************************************************************************/

-- Privates types 
subtype flag is boolean;
subtype car is varchar2(1);
subtype phrase is varchar2(255);
--@youcef
subtype txt is varchar2(32767); -- old value: 2000 bug: ORA-20100: 
subtype bigtext is varchar2(32767);
subtype margin is number;

-- type tv1 is table of varchar2(1) index by binary_integer;
type tbool is table of boolean index by binary_integer;
type tn is table of number index by binary_integer;
type tv4000 is table of varchar2(4000) index by binary_integer;
type tv32k is table of varchar2(32767) index by binary_integer;

type charSet is table of pls_integer index by car;

type recFont is record ( i    word, 
	 		 		   	 n pls_integer, 
	 		 		   	 type word,
						 name word,
						 dsc  tv4000,
						 up   word,
						 ut   word,
						 cw   charSet,
						 enc  word,
						 file word,
						 diff word,
						 length1 word,
						 length2 word);

type fontsArray is table of recFont index by phrase;

type recImage is record ( n number,  	 	  	 		-- indice d'insertion dans le document
	 		  	 		  i number,  	 	 			-- ?
	 		  	 		  w number,  		 			-- width
	 		  	 		  h number,  		 			-- height
						  cs txt,				    -- colorspace
						  bpc txt,				 	-- Bit per color
	 		  	 		  f txt,  	 			 	-- File Format
						  parms txt,			 	-- pdf parameter for this image
						  pal txt,				 	-- colors palette informations 
						  trns tn,			 	 	-- transparency 
						  data blob	 			 	-- Data
 );

type imagesArray is table of recImage index by txt;

type recFormat is record ( largeur number, 
	 		 		       hauteur number);
						 
type rec2chp is record ( zero txt, 
	 		 		   	 un txt); 

type rec5 is record ( zero txt, 
	 		 		  un txt,
					  deux txt,
					  trois txt,
					  quatre txt);
					  
type LinksArray is table of rec5;

type Array2dim is table of rec2chp;

type ArrayCharWidths is table of charSet index by word;

-- Private properties 
 page number;               -- current page number
 n number;                  -- current object number
 offsets tv4000;            -- array of object offsets
 pdfDoc tv32k;             -- buffer holding in-memory final PDF document. 									   
 imgBlob blob;              -- allows creation of persistent blobs for images
 pages tv32k;               -- array containing pages 
 state word;                -- current document state
 b_compress flag := false;  -- compression flag 
 DefOrientation car;        -- default orientation
 CurOrientation car;        -- current orientation
 OrientationChanges tbool;    -- array indicating orientation changes
 k number;                  -- scale factor (number of points in user unit)
 fwPt number;
 fhPt number;         		-- dimensions of page format in points
 fw number;
 fh number;             	-- dimensions of page format in user unit
 wPt number;
 hPt number;           		-- current dimensions of page in points
 w number;
 h number;               	-- current dimensions of page in user unit
 lMargin margin;            -- left margin
 tMargin margin;            -- top margin
 rMargin margin;            -- right margin
 bMargin margin;            -- page break margin
 cMargin margin;            -- cell margin
 x number;
 y number;               	-- current position in user unit for cell positioning
 lasth number;              -- height of last cell printed 
 LineWidth number;          -- line width in user unit
 CoreFonts tv4000a;          	-- array of standard font names
 fonts fontsArray;  
                                -- array of used fonts
 FontFiles fontsArray;          	-- array of font files
 diffs tv4000;              	-- array of encoding differences 
 images imagesArray;             	-- array of used images
 PageLinks LinksArray;          -- array of links in pages
 links Array2dim;              -- array of internal links
 FontFamily word;         	-- current font family
 FontStyle word;          	-- current font style
 underline flag;          	-- underlining flag
 CurrentFont recFont;        -- current font info
 FontSizePt number;         -- current font size in points
 FontSize number;           -- current font size in user unit
 DrawColor phrase;          -- commands for drawing color
 FillColor phrase;          -- commands for filling color
 TextColor phrase;          -- commands for txt color
 ColorFlag flag;          	-- indicates whether fill and txt colors are different
 ws word;                 	-- word spacing 
 AutoPageBreak flag;      	-- automatic page breaking
 PageBreakTrigger number;   -- threshold used to trigger page breaks
 InFooter flag;           	-- flag set when processing footer
 ZoomMode word;           	-- zoom display mode
 LayoutMode word;         	-- layout display mode
 title txt;              	-- title
 subject txt;            	-- subject
 author txt;             	-- author
 keywords txt;           	-- keywords
 creator txt;            	-- creator
 AliasNbPages word;       	-- alias for total number of pages
 PDFVersion word;         	-- PDF version number
 
 -- Proprietés ajoutées lors du portage en PLSQL.
 fpdf_charwidths ArrayCharWidths;		-- Characters table.
 MyHeader_Proc txt;						-- Personal Header procedure.
 MyHeader_ProcParam tv4000a;            -- Table of parameters of the personal header Proc.
 MyFooter_Proc txt;						-- Personal Footer procedure.
 MyFooter_ProcParam tv4000a;            -- Table of parameters of the personal footer Proc.
 formatArray recFormat;					-- Dimension of the format (variable : format).
 gb_mode_debug boolean := false;
 Linespacing number;
 
 -- variables dont je ne maitrise pas bien l'emploi.
 -- A vérifier au court de la validation du portage.
 originalsize word;
 size1 word;
 size2 word;
/*******************************************************************************
*                                                                              *
*           Protected methods : Internal function and procedures               *
*                                                                              *
*******************************************************************************/
----------------------------------------------------------------------------------
-- proc. and func. spécifiques ajoutées au portage.
----------------------------------------------------------------------------------
procedure print (pstr varchar2) is
begin
  -- Choose the output mode...
  htp.p(pstr);
  -- My outpout method
  -- affiche.p(pstr);
end print;


----------------------------------------------------------------------------------
-- Testing if method for additionnal fonts exists in this package
-- lv_existing_methods MUST reference all the "p_put..." procedure of the package.
----------------------------------------------------------------------------------
function methode_exists(pMethodName varchar2) return boolean is
lv_existing_methods varchar2(2000) 
:= 'p_putstream,p_putxobjectdict,p_putresourcedict,p_putfonts,p_putimages,p_putresources,'||
   'p_putinfo,p_putcatalog,p_putheader,p_puttrailer,p_putpages';
begin
   if (instr(lv_existing_methods, lower(pMethodName) ) > 0 ) then
     return true;
   end if;
   return false;
exception 
  when others then
   return false;
end methode_exists;

----------------------------------------------------------------------------------
-- Calculate the length of the final document contained in the plsql table pdfDoc.
----------------------------------------------------------------------------------
function getPDFDocLength return pls_integer is
  lg pls_integer := 0;
begin
  for i in pdfDoc.first..pdfDoc.last loop
    lg := lg + nvl(length(pdfDoc(i)), 0);
  end loop;
  return lg;
exception 
  when others then
   error('getPDFDocLength : '||sqlerrm);
   return -1;
end getPDFDocLength;

----------------------------------------------------------------------------------
-- Setting metric for courier Font
----------------------------------------------------------------------------------
function getFontCourier return charSet is
mySet charSet;
begin 
	--
	-- Courier font.
	--
	for i in 0..255
	loop
		mySet(chr(i)):=600;
	end loop;
	return mySet;
end getFontCourier;

----------------------------------------------------------------------------------
-- Setting metric for helvetica
----------------------------------------------------------------------------------
function getFontHelvetica return charSet is
mySet charSet;
begin 
	-- helvetica font.
	mySet(chr(0)) := 278; mySet(chr(1)) := 278; mySet(chr(2)) := 278; mySet(chr(3)) := 278; mySet(chr(4)) := 278; mySet(chr(5)) := 278; mySet(chr(6)) := 278; mySet(chr(7)) := 278; mySet(chr(8)) := 278; mySet(chr(9)) := 278; mySet(chr(10)) := 278; mySet(chr(11)) := 278; mySet(chr(12)) := 278; mySet(chr(13)) := 278; mySet(chr(14)) := 278; mySet(chr(15)) := 278; mySet(chr(16)) := 278; mySet(chr(17)) := 278; mySet(chr(18)) := 278; mySet(chr(19)) := 278; mySet(chr(20)) := 278; mySet(chr(21)) := 278;
	mySet(chr(22)) := 278; mySet(chr(23)) := 278; mySet(chr(24)) := 278; mySet(chr(25)) := 278; mySet(chr(26)) := 278; mySet(chr(27)) := 278; mySet(chr(28)) := 278; mySet(chr(29)) := 278; mySet(chr(30)) := 278; mySet(chr(31)) := 278;
	mySet(' ') := 278; mySet('!') := 278; mySet('"') := 355; mySet('#') := 556; mySet('$') := 556; mySet('%') := 889; mySet('&') := 667; mySet('''') := 191; mySet('(') := 333; mySet(')') := 333; mySet('*') := 389; mySet('+') := 584;
	mySet(',') := 278; mySet('-') := 333; mySet('.') := 278; mySet('/') := 278; mySet('0') := 556; mySet('1') := 556; mySet('2') := 556; mySet('3') := 556; mySet('4') := 556; mySet('5') := 556; mySet('6') := 556; mySet('7') := 556; mySet('8') := 556; mySet('9') := 556; mySet(':') := 278; mySet(';') := 278; mySet('<') := 584; mySet('=') := 584; mySet('>') := 584; mySet('?') := 556; mySet('@') := 1015; mySet('A') := 667;
	mySet('B') := 667; mySet('C') := 722; mySet('D') := 722; mySet('E') := 667; mySet('F') := 611; mySet('G') := 778; mySet('H') := 722; mySet('I') := 278; mySet('J') := 500; mySet('K') := 667; mySet('L') := 556; mySet('M') := 833; mySet('N') := 722; mySet('O') := 778; mySet('P') := 667; mySet('Q') := 778; mySet('R') := 722; mySet('S') := 667; mySet('T') := 611; mySet('U') := 722; mySet('V') := 667; mySet('W') := 944;
	mySet('X') := 667; mySet('Y') := 667; mySet('Z') := 611; mySet('[') := 278; mySet('\') := 278; mySet(']') := 278; mySet('^') := 469; mySet('_') := 556; mySet('`') := 333; mySet('a') := 556; mySet('b') := 556; mySet('c') := 500; mySet('d') := 556; mySet('e') := 556; mySet('f') := 278; mySet('g') := 556; mySet('h') := 556; mySet('i') := 222; mySet('j') := 222; mySet('k') := 500; mySet('l') := 222; mySet('m') := 833;
	mySet('n') := 556; mySet('o') := 556; mySet('p') := 556; mySet('q') := 556; mySet('r') := 333; mySet('s') := 500; mySet('t') := 278; mySet('u') := 556; mySet('v') := 500; mySet('w') := 722; mySet('x') := 500; mySet('y') := 500; mySet('z') := 500; mySet('{') := 334; mySet('|') := 260; mySet('}') := 334; mySet('~') := 584; mySet(chr(127)) := 350; mySet(chr(128)) := 556; mySet(chr(129)) := 350; mySet(chr(130)) := 222; mySet(chr(131)) := 556;
	mySet(chr(132)) := 333; mySet(chr(133)) := 1000; mySet(chr(134)) := 556; mySet(chr(135)) := 556; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 667; mySet(chr(139)) := 333; mySet(chr(140)) := 1000; mySet(chr(141)) := 350; mySet(chr(142)) := 611; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 222; mySet(chr(146)) := 222; mySet(chr(147)) := 333; mySet(chr(148)) := 333; mySet(chr(149)) := 350; mySet(chr(150)) := 556; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 1000;
	mySet(chr(154)) := 500; mySet(chr(155)) := 333; mySet(chr(156)) := 944; mySet(chr(157)) := 350; mySet(chr(158)) := 500; mySet(chr(159)) := 667; mySet(chr(160)) := 278; mySet(chr(161)) := 333; mySet(chr(162)) := 556; mySet(chr(163)) := 556; mySet(chr(164)) := 556; mySet(chr(165)) := 556; mySet(chr(166)) := 260; mySet(chr(167)) := 556; mySet(chr(168)) := 333; mySet(chr(169)) := 737; mySet(chr(170)) := 370; mySet(chr(171)) := 556; mySet(chr(172)) := 584; mySet(chr(173)) := 333; mySet(chr(174)) := 737; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 584; mySet(chr(178)) := 333; mySet(chr(179)) := 333; mySet(chr(180)) := 333; mySet(chr(181)) := 556; mySet(chr(182)) := 537; mySet(chr(183)) := 278; mySet(chr(184)) := 333; mySet(chr(185)) := 333; mySet(chr(186)) := 365; mySet(chr(187)) := 556; mySet(chr(188)) := 834; mySet(chr(189)) := 834; mySet(chr(190)) := 834; mySet(chr(191)) := 611; mySet(chr(192)) := 667; mySet(chr(193)) := 667; mySet(chr(194)) := 667; mySet(chr(195)) := 667; mySet(chr(196)) := 667; mySet(chr(197)) := 667;
	mySet(chr(198)) := 1000; mySet(chr(199)) := 722; mySet(chr(200)) := 667; mySet(chr(201)) := 667; mySet(chr(202)) := 667; mySet(chr(203)) := 667; mySet(chr(204)) := 278; mySet(chr(205)) := 278; mySet(chr(206)) := 278; mySet(chr(207)) := 278; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 778; mySet(chr(211)) := 778; mySet(chr(212)) := 778; mySet(chr(213)) := 778; mySet(chr(214)) := 778; mySet(chr(215)) := 584; mySet(chr(216)) := 778; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 667; mySet(chr(222)) := 667; mySet(chr(223)) := 611; mySet(chr(224)) := 556; mySet(chr(225)) := 556; mySet(chr(226)) := 556; mySet(chr(227)) := 556; mySet(chr(228)) := 556; mySet(chr(229)) := 556; mySet(chr(230)) := 889; mySet(chr(231)) := 500; mySet(chr(232)) := 556; mySet(chr(233)) := 556; mySet(chr(234)) := 556; mySet(chr(235)) := 556; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 556; mySet(chr(241)) := 556;
	mySet(chr(242)) := 556; mySet(chr(243)) := 556; mySet(chr(244)) := 556; mySet(chr(245)) := 556; mySet(chr(246)) := 556; mySet(chr(247)) := 584; mySet(chr(248)) := 611; mySet(chr(249)) := 556; mySet(chr(250)) := 556; mySet(chr(251)) := 556; mySet(chr(252)) := 556; mySet(chr(253)) := 500; mySet(chr(254)) := 556; mySet(chr(255)) := 500;
	return mySet;
end getFontHelvetica;

----------------------------------------------------------------------------------
-- Setting metric for helvetica ITALIC
----------------------------------------------------------------------------------
function getFontHelveticai return charSet is
mySet charSet;
begin 
	-- helvetica Italic font.
	mySet(chr(0)) := 278; mySet(chr(1)) := 278; mySet(chr(2)) := 278; mySet(chr(3)) := 278; mySet(chr(4)) := 278; mySet(chr(5)) := 278; mySet(chr(6)) := 278; mySet(chr(7)) := 278; mySet(chr(8)) := 278; mySet(chr(9)) := 278; mySet(chr(10)) := 278; mySet(chr(11)) := 278; mySet(chr(12)) := 278; mySet(chr(13)) := 278; mySet(chr(14)) := 278; mySet(chr(15)) := 278; mySet(chr(16)) := 278; mySet(chr(17)) := 278; mySet(chr(18)) := 278; mySet(chr(19)) := 278; mySet(chr(20)) := 278; mySet(chr(21)) := 278;
	mySet(chr(22)) := 278; mySet(chr(23)) := 278; mySet(chr(24)) := 278; mySet(chr(25)) := 278; mySet(chr(26)) := 278; mySet(chr(27)) := 278; mySet(chr(28)) := 278; mySet(chr(29)) := 278; mySet(chr(30)) := 278; mySet(chr(31)) := 278; mySet(' ') := 278; mySet('!') := 278; mySet('"') := 355; mySet('#') := 556; mySet('$') := 556; mySet('%') := 889; mySet('&') := 667; mySet('''') := 191; mySet('(') := 333; mySet(')') := 333; mySet('*') := 389; mySet('+') := 584;
	mySet(',') := 278; mySet('-') := 333; mySet('.') := 278; mySet('/') := 278; mySet('0') := 556; mySet('1') := 556; mySet('2') := 556; mySet('3') := 556; mySet('4') := 556; mySet('5') := 556; mySet('6') := 556; mySet('7') := 556; mySet('8') := 556; mySet('9') := 556; mySet(':') := 278; mySet(';') := 278; mySet('<') := 584; mySet('=') := 584; mySet('>') := 584; mySet('?') := 556; mySet('@') := 1015; mySet('A') := 667;
	mySet('B') := 667; mySet('C') := 722; mySet('D') := 722; mySet('E') := 667; mySet('F') := 611; mySet('G') := 778; mySet('H') := 722; mySet('I') := 278; mySet('J') := 500; mySet('K') := 667; mySet('L') := 556; mySet('M') := 833; mySet('N') := 722; mySet('O') := 778; mySet('P') := 667; mySet('Q') := 778; mySet('R') := 722; mySet('S') := 667; mySet('T') := 611; mySet('U') := 722; mySet('V') := 667; mySet('W') := 944;
	mySet('X') := 667; mySet('Y') := 667; mySet('Z') := 611; mySet('[') := 278; mySet('\') := 278; mySet(']') := 278; mySet('^') := 469; mySet('_') := 556; mySet('`') := 333; mySet('a') := 556; mySet('b') := 556; mySet('c') := 500; mySet('d') := 556; mySet('e') := 556; mySet('f') := 278; mySet('g') := 556; mySet('h') := 556; mySet('i') := 222; mySet('j') := 222; mySet('k') := 500; mySet('l') := 222; mySet('m') := 833;
	mySet('n') := 556; mySet('o') := 556; mySet('p') := 556; mySet('q') := 556; mySet('r') := 333; mySet('s') := 500; mySet('t') := 278; mySet('u') := 556; mySet('v') := 500; mySet('w') := 722; mySet('x') := 500; mySet('y') := 500; mySet('z') := 500; mySet('{') := 334; mySet('|') := 260; mySet('}') := 334; mySet('~') := 584; mySet(chr(127)) := 350; mySet(chr(128)) := 556; mySet(chr(129)) := 350; mySet(chr(130)) := 222; mySet(chr(131)) := 556;
	mySet(chr(132)) := 333; mySet(chr(133)) := 1000; mySet(chr(134)) := 556; mySet(chr(135)) := 556; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 667; mySet(chr(139)) := 333; mySet(chr(140)) := 1000; mySet(chr(141)) := 350; mySet(chr(142)) := 611; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 222; mySet(chr(146)) := 222; mySet(chr(147)) := 333; mySet(chr(148)) := 333; mySet(chr(149)) := 350; mySet(chr(150)) := 556; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 1000;
	mySet(chr(154)) := 500; mySet(chr(155)) := 333; mySet(chr(156)) := 944; mySet(chr(157)) := 350; mySet(chr(158)) := 500; mySet(chr(159)) := 667; mySet(chr(160)) := 278; mySet(chr(161)) := 333; mySet(chr(162)) := 556; mySet(chr(163)) := 556; mySet(chr(164)) := 556; mySet(chr(165)) := 556; mySet(chr(166)) := 260; mySet(chr(167)) := 556; mySet(chr(168)) := 333; mySet(chr(169)) := 737; mySet(chr(170)) := 370; mySet(chr(171)) := 556; mySet(chr(172)) := 584; mySet(chr(173)) := 333; mySet(chr(174)) := 737; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 584; mySet(chr(178)) := 333; mySet(chr(179)) := 333; mySet(chr(180)) := 333; mySet(chr(181)) := 556; mySet(chr(182)) := 537; mySet(chr(183)) := 278; mySet(chr(184)) := 333; mySet(chr(185)) := 333; mySet(chr(186)) := 365; mySet(chr(187)) := 556; mySet(chr(188)) := 834; mySet(chr(189)) := 834; mySet(chr(190)) := 834; mySet(chr(191)) := 611; mySet(chr(192)) := 667; mySet(chr(193)) := 667; mySet(chr(194)) := 667; mySet(chr(195)) := 667; mySet(chr(196)) := 667; mySet(chr(197)) := 667;
	mySet(chr(198)) := 1000; mySet(chr(199)) := 722; mySet(chr(200)) := 667; mySet(chr(201)) := 667; mySet(chr(202)) := 667; mySet(chr(203)) := 667; mySet(chr(204)) := 278; mySet(chr(205)) := 278; mySet(chr(206)) := 278; mySet(chr(207)) := 278; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 778; mySet(chr(211)) := 778; mySet(chr(212)) := 778; mySet(chr(213)) := 778; mySet(chr(214)) := 778; mySet(chr(215)) := 584; mySet(chr(216)) := 778; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 667; mySet(chr(222)) := 667; mySet(chr(223)) := 611; mySet(chr(224)) := 556; mySet(chr(225)) := 556; mySet(chr(226)) := 556; mySet(chr(227)) := 556; mySet(chr(228)) := 556; mySet(chr(229)) := 556; mySet(chr(230)) := 889; mySet(chr(231)) := 500; mySet(chr(232)) := 556; mySet(chr(233)) := 556; mySet(chr(234)) := 556; mySet(chr(235)) := 556; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 556; mySet(chr(241)) := 556;
	mySet(chr(242)) := 556; mySet(chr(243)) := 556; mySet(chr(244)) := 556; mySet(chr(245)) := 556; mySet(chr(246)) := 556; mySet(chr(247)) := 584; mySet(chr(248)) := 611; mySet(chr(249)) := 556; mySet(chr(250)) := 556; mySet(chr(251)) := 556; mySet(chr(252)) := 556; mySet(chr(253)) := 500; mySet(chr(254)) := 556; mySet(chr(255)) := 500;
	return mySet;
end getFontHelveticai;

----------------------------------------------------------------------------------
-- Setting metric for helvetica BOLD
----------------------------------------------------------------------------------
function getFontHelveticab return charSet is
mySet charSet;
begin 
	-- helvetica bold font.
	mySet(chr(0)) := 278; mySet(chr(1)) := 278; mySet(chr(2)) := 278; mySet(chr(3)) := 278; mySet(chr(4)) := 278; mySet(chr(5)) := 278; mySet(chr(6)) := 278; mySet(chr(7)) := 278; mySet(chr(8)) := 278; mySet(chr(9)) := 278; mySet(chr(10)) := 278; mySet(chr(11)) := 278; mySet(chr(12)) := 278; mySet(chr(13)) := 278; mySet(chr(14)) := 278; mySet(chr(15)) := 278; mySet(chr(16)) := 278; mySet(chr(17)) := 278; mySet(chr(18)) := 278; mySet(chr(19)) := 278; mySet(chr(20)) := 278; mySet(chr(21)) := 278;
	mySet(chr(22)) := 278; mySet(chr(23)) := 278; mySet(chr(24)) := 278; mySet(chr(25)) := 278; mySet(chr(26)) := 278; mySet(chr(27)) := 278; mySet(chr(28)) := 278; mySet(chr(29)) := 278; mySet(chr(30)) := 278; mySet(chr(31)) := 278; mySet(' ') := 278; mySet('!') := 333; mySet('"') := 474; mySet('#') := 556; mySet('$') := 556; mySet('%') := 889; mySet('&') := 722; mySet('''') := 238; mySet('(') := 333; mySet(')') := 333; mySet('*') := 389; mySet('+') := 584;
	mySet(',') := 278; mySet('-') := 333; mySet('.') := 278; mySet('/') := 278; mySet('0') := 556; mySet('1') := 556; mySet('2') := 556; mySet('3') := 556; mySet('4') := 556; mySet('5') := 556; mySet('6') := 556; mySet('7') := 556; mySet('8') := 556; mySet('9') := 556; mySet(':') := 333; mySet(';') := 333; mySet('<') := 584; mySet('=') := 584; mySet('>') := 584; mySet('?') := 611; mySet('@') := 975; mySet('A') := 722;
	mySet('B') := 722; mySet('C') := 722; mySet('D') := 722; mySet('E') := 667; mySet('F') := 611; mySet('G') := 778; mySet('H') := 722; mySet('I') := 278; mySet('J') := 556; mySet('K') := 722; mySet('L') := 611; mySet('M') := 833; mySet('N') := 722; mySet('O') := 778; mySet('P') := 667; mySet('Q') := 778; mySet('R') := 722; mySet('S') := 667; mySet('T') := 611; mySet('U') := 722; mySet('V') := 667; mySet('W') := 944;
	mySet('X') := 667; mySet('Y') := 667; mySet('Z') := 611; mySet('[') := 333; mySet('\') := 278; mySet(']') := 333; mySet('^') := 584; mySet('_') := 556; mySet('`') := 333; mySet('a') := 556; mySet('b') := 611; mySet('c') := 556; mySet('d') := 611; mySet('e') := 556; mySet('f') := 333; mySet('g') := 611; mySet('h') := 611; mySet('i') := 278; mySet('j') := 278; mySet('k') := 556; mySet('l') := 278; mySet('m') := 889;
	mySet('n') := 611; mySet('o') := 611; mySet('p') := 611; mySet('q') := 611; mySet('r') := 389; mySet('s') := 556; mySet('t') := 333; mySet('u') := 611; mySet('v') := 556; mySet('w') := 778; mySet('x') := 556; mySet('y') := 556; mySet('z') := 500; mySet('{') := 389; mySet('|') := 280; mySet('}') := 389; mySet('~') := 584; mySet(chr(127)) := 350; mySet(chr(128)) := 556; mySet(chr(129)) := 350; mySet(chr(130)) := 278; mySet(chr(131)) := 556;
	mySet(chr(132)) := 500; mySet(chr(133)) := 1000; mySet(chr(134)) := 556; mySet(chr(135)) := 556; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 667; mySet(chr(139)) := 333; mySet(chr(140)) := 1000; mySet(chr(141)) := 350; mySet(chr(142)) := 611; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 278; mySet(chr(146)) := 278; mySet(chr(147)) := 500; mySet(chr(148)) := 500; mySet(chr(149)) := 350; mySet(chr(150)) := 556; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 1000;
	mySet(chr(154)) := 556; mySet(chr(155)) := 333; mySet(chr(156)) := 944; mySet(chr(157)) := 350; mySet(chr(158)) := 500; mySet(chr(159)) := 667; mySet(chr(160)) := 278; mySet(chr(161)) := 333; mySet(chr(162)) := 556; mySet(chr(163)) := 556; mySet(chr(164)) := 556; mySet(chr(165)) := 556; mySet(chr(166)) := 280; mySet(chr(167)) := 556; mySet(chr(168)) := 333; mySet(chr(169)) := 737; mySet(chr(170)) := 370; mySet(chr(171)) := 556; mySet(chr(172)) := 584; mySet(chr(173)) := 333; mySet(chr(174)) := 737; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 584; mySet(chr(178)) := 333; mySet(chr(179)) := 333; mySet(chr(180)) := 333; mySet(chr(181)) := 611; mySet(chr(182)) := 556; mySet(chr(183)) := 278; mySet(chr(184)) := 333; mySet(chr(185)) := 333; mySet(chr(186)) := 365; mySet(chr(187)) := 556; mySet(chr(188)) := 834; mySet(chr(189)) := 834; mySet(chr(190)) := 834; mySet(chr(191)) := 611; mySet(chr(192)) := 722; mySet(chr(193)) := 722; mySet(chr(194)) := 722; mySet(chr(195)) := 722; mySet(chr(196)) := 722; mySet(chr(197)) := 722;
	mySet(chr(198)) := 1000; mySet(chr(199)) := 722; mySet(chr(200)) := 667; mySet(chr(201)) := 667; mySet(chr(202)) := 667; mySet(chr(203)) := 667; mySet(chr(204)) := 278; mySet(chr(205)) := 278; mySet(chr(206)) := 278; mySet(chr(207)) := 278; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 778; mySet(chr(211)) := 778; mySet(chr(212)) := 778; mySet(chr(213)) := 778; mySet(chr(214)) := 778; mySet(chr(215)) := 584; mySet(chr(216)) := 778; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 667; mySet(chr(222)) := 667; mySet(chr(223)) := 611; mySet(chr(224)) := 556; mySet(chr(225)) := 556; mySet(chr(226)) := 556; mySet(chr(227)) := 556; mySet(chr(228)) := 556; mySet(chr(229)) := 556; mySet(chr(230)) := 889; mySet(chr(231)) := 556; mySet(chr(232)) := 556; mySet(chr(233)) := 556; mySet(chr(234)) := 556; mySet(chr(235)) := 556; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 611; mySet(chr(241)) := 611;
	mySet(chr(242)) := 611; mySet(chr(243)) := 611; mySet(chr(244)) := 611; mySet(chr(245)) := 611; mySet(chr(246)) := 611; mySet(chr(247)) := 584; mySet(chr(248)) := 611; mySet(chr(249)) := 611; mySet(chr(250)) := 611; mySet(chr(251)) := 611; mySet(chr(252)) := 611; mySet(chr(253)) := 556; mySet(chr(254)) := 611; mySet(chr(255)) := 556;
	return mySet;
end getFontHelveticab;

----------------------------------------------------------------------------------
-- Setting metric for helvetica BOLD ITALIC
----------------------------------------------------------------------------------
function getFontHelveticabi return charSet is
mySet charSet;
begin 
	-- helvetica bold italic font.
	mySet(chr(0)) := 278; mySet(chr(1)) := 278; mySet(chr(2)) := 278; mySet(chr(3)) := 278; mySet(chr(4)) := 278; mySet(chr(5)) := 278; mySet(chr(6)) := 278; mySet(chr(7)) := 278; mySet(chr(8)) := 278; mySet(chr(9)) := 278; mySet(chr(10)) := 278; mySet(chr(11)) := 278; mySet(chr(12)) := 278; mySet(chr(13)) := 278; mySet(chr(14)) := 278; mySet(chr(15)) := 278; mySet(chr(16)) := 278; mySet(chr(17)) := 278; mySet(chr(18)) := 278; mySet(chr(19)) := 278; mySet(chr(20)) := 278; mySet(chr(21)) := 278;
	mySet(chr(22)) := 278; mySet(chr(23)) := 278; mySet(chr(24)) := 278; mySet(chr(25)) := 278; mySet(chr(26)) := 278; mySet(chr(27)) := 278; mySet(chr(28)) := 278; mySet(chr(29)) := 278; mySet(chr(30)) := 278; mySet(chr(31)) := 278; mySet(' ') := 278; mySet('!') := 333; mySet('"') := 474; mySet('#') := 556; mySet('$') := 556; mySet('%') := 889; mySet('&') := 722; mySet('''') := 238; mySet('(') := 333; mySet(')') := 333; mySet('*') := 389; mySet('+') := 584;
	mySet(',') := 278; mySet('-') := 333; mySet('.') := 278; mySet('/') := 278; mySet('0') := 556; mySet('1') := 556; mySet('2') := 556; mySet('3') := 556; mySet('4') := 556; mySet('5') := 556; mySet('6') := 556; mySet('7') := 556; mySet('8') := 556; mySet('9') := 556; mySet(':') := 333; mySet(';') := 333; mySet('<') := 584; mySet('=') := 584; mySet('>') := 584; mySet('?') := 611; mySet('@') := 975; mySet('A') := 722;
	mySet('B') := 722; mySet('C') := 722; mySet('D') := 722; mySet('E') := 667; mySet('F') := 611; mySet('G') := 778; mySet('H') := 722; mySet('I') := 278; mySet('J') := 556; mySet('K') := 722; mySet('L') := 611; mySet('M') := 833; mySet('N') := 722; mySet('O') := 778; mySet('P') := 667; mySet('Q') := 778; mySet('R') := 722; mySet('S') := 667; mySet('T') := 611; mySet('U') := 722; mySet('V') := 667; mySet('W') := 944;
	mySet('X') := 667; mySet('Y') := 667; mySet('Z') := 611; mySet('[') := 333; mySet('\') := 278; mySet(']') := 333; mySet('^') := 584; mySet('_') := 556; mySet('`') := 333; mySet('a') := 556; mySet('b') := 611; mySet('c') := 556; mySet('d') := 611; mySet('e') := 556; mySet('f') := 333; mySet('g') := 611; mySet('h') := 611; mySet('i') := 278; mySet('j') := 278; mySet('k') := 556; mySet('l') := 278; mySet('m') := 889;
	mySet('n') := 611; mySet('o') := 611; mySet('p') := 611; mySet('q') := 611; mySet('r') := 389; mySet('s') := 556; mySet('t') := 333; mySet('u') := 611; mySet('v') := 556; mySet('w') := 778; mySet('x') := 556; mySet('y') := 556; mySet('z') := 500; mySet('{') := 389; mySet('|') := 280; mySet('}') := 389; mySet('~') := 584; mySet(chr(127)) := 350; mySet(chr(128)) := 556; mySet(chr(129)) := 350; mySet(chr(130)) := 278; mySet(chr(131)) := 556;
	mySet(chr(132)) := 500; mySet(chr(133)) := 1000; mySet(chr(134)) := 556; mySet(chr(135)) := 556; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 667; mySet(chr(139)) := 333; mySet(chr(140)) := 1000; mySet(chr(141)) := 350; mySet(chr(142)) := 611; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 278; mySet(chr(146)) := 278; mySet(chr(147)) := 500; mySet(chr(148)) := 500; mySet(chr(149)) := 350; mySet(chr(150)) := 556; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 1000;
	mySet(chr(154)) := 556; mySet(chr(155)) := 333; mySet(chr(156)) := 944; mySet(chr(157)) := 350; mySet(chr(158)) := 500; mySet(chr(159)) := 667; mySet(chr(160)) := 278; mySet(chr(161)) := 333; mySet(chr(162)) := 556; mySet(chr(163)) := 556; mySet(chr(164)) := 556; mySet(chr(165)) := 556; mySet(chr(166)) := 280; mySet(chr(167)) := 556; mySet(chr(168)) := 333; mySet(chr(169)) := 737; mySet(chr(170)) := 370; mySet(chr(171)) := 556; mySet(chr(172)) := 584; mySet(chr(173)) := 333; mySet(chr(174)) := 737; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 584; mySet(chr(178)) := 333; mySet(chr(179)) := 333; mySet(chr(180)) := 333; mySet(chr(181)) := 611; mySet(chr(182)) := 556; mySet(chr(183)) := 278; mySet(chr(184)) := 333; mySet(chr(185)) := 333; mySet(chr(186)) := 365; mySet(chr(187)) := 556; mySet(chr(188)) := 834; mySet(chr(189)) := 834; mySet(chr(190)) := 834; mySet(chr(191)) := 611; mySet(chr(192)) := 722; mySet(chr(193)) := 722; mySet(chr(194)) := 722; mySet(chr(195)) := 722; mySet(chr(196)) := 722; mySet(chr(197)) := 722;
	mySet(chr(198)) := 1000; mySet(chr(199)) := 722; mySet(chr(200)) := 667; mySet(chr(201)) := 667; mySet(chr(202)) := 667; mySet(chr(203)) := 667; mySet(chr(204)) := 278; mySet(chr(205)) := 278; mySet(chr(206)) := 278; mySet(chr(207)) := 278; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 778; mySet(chr(211)) := 778; mySet(chr(212)) := 778; mySet(chr(213)) := 778; mySet(chr(214)) := 778; mySet(chr(215)) := 584; mySet(chr(216)) := 778; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 667; mySet(chr(222)) := 667; mySet(chr(223)) := 611; mySet(chr(224)) := 556; mySet(chr(225)) := 556; mySet(chr(226)) := 556; mySet(chr(227)) := 556; mySet(chr(228)) := 556; mySet(chr(229)) := 556; mySet(chr(230)) := 889; mySet(chr(231)) := 556; mySet(chr(232)) := 556; mySet(chr(233)) := 556; mySet(chr(234)) := 556; mySet(chr(235)) := 556; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 611; mySet(chr(241)) := 611;
	mySet(chr(242)) := 611; mySet(chr(243)) := 611; mySet(chr(244)) := 611; mySet(chr(245)) := 611; mySet(chr(246)) := 611; mySet(chr(247)) := 584; mySet(chr(248)) := 611; mySet(chr(249)) := 611; mySet(chr(250)) := 611; mySet(chr(251)) := 611; mySet(chr(252)) := 611; mySet(chr(253)) := 556; mySet(chr(254)) := 611; mySet(chr(255)) := 556;
	return mySet;
end getFontHelveticabi;

----------------------------------------------------------------------------------
-- Setting metric for times
----------------------------------------------------------------------------------
function getFontTimes return charSet is
mySet charSet;
begin 
	-- Times font.
	mySet(chr(0)) := 250; mySet(chr(1)) := 250; mySet(chr(2)) := 250; mySet(chr(3)) := 250; mySet(chr(4)) := 250; mySet(chr(5)) := 250; mySet(chr(6)) := 250; mySet(chr(7)) := 250; mySet(chr(8)) := 250; mySet(chr(9)) := 250; mySet(chr(10)) := 250; mySet(chr(11)) := 250; mySet(chr(12)) := 250; mySet(chr(13)) := 250; mySet(chr(14)) := 250; mySet(chr(15)) := 250; mySet(chr(16)) := 250; mySet(chr(17)) := 250; mySet(chr(18)) := 250; mySet(chr(19)) := 250; mySet(chr(20)) := 250; mySet(chr(21)) := 250;
	mySet(chr(22)) := 250; mySet(chr(23)) := 250; mySet(chr(24)) := 250; mySet(chr(25)) := 250; mySet(chr(26)) := 250; mySet(chr(27)) := 250; mySet(chr(28)) := 250; mySet(chr(29)) := 250; mySet(chr(30)) := 250; mySet(chr(31)) := 250; mySet(' ') := 250; mySet('!') := 333; mySet('"') := 408; mySet('#') := 500; mySet('$') := 500; mySet('%') := 833; mySet('&') := 778; mySet('''') := 180; mySet('(') := 333; mySet(')') := 333; mySet('*') := 500; mySet('+') := 564;
	mySet(',') := 250; mySet('-') := 333; mySet('.') := 250; mySet('/') := 278; mySet('0') := 500; mySet('1') := 500; mySet('2') := 500; mySet('3') := 500; mySet('4') := 500; mySet('5') := 500; mySet('6') := 500; mySet('7') := 500; mySet('8') := 500; mySet('9') := 500; mySet(':') := 278; mySet(';') := 278; mySet('<') := 564; mySet('=') := 564; mySet('>') := 564; mySet('?') := 444; mySet('@') := 921; mySet('A') := 722;
	mySet('B') := 667; mySet('C') := 667; mySet('D') := 722; mySet('E') := 611; mySet('F') := 556; mySet('G') := 722; mySet('H') := 722; mySet('I') := 333; mySet('J') := 389; mySet('K') := 722; mySet('L') := 611; mySet('M') := 889; mySet('N') := 722; mySet('O') := 722; mySet('P') := 556; mySet('Q') := 722; mySet('R') := 667; mySet('S') := 556; mySet('T') := 611; mySet('U') := 722; mySet('V') := 722; mySet('W') := 944;
	mySet('X') := 722; mySet('Y') := 722; mySet('Z') := 611; mySet('[') := 333; mySet('\') := 278; mySet(']') := 333; mySet('^') := 469; mySet('_') := 500; mySet('`') := 333; mySet('a') := 444; mySet('b') := 500; mySet('c') := 444; mySet('d') := 500; mySet('e') := 444; mySet('f') := 333; mySet('g') := 500; mySet('h') := 500; mySet('i') := 278; mySet('j') := 278; mySet('k') := 500; mySet('l') := 278; mySet('m') := 778;
	mySet('n') := 500; mySet('o') := 500; mySet('p') := 500; mySet('q') := 500; mySet('r') := 333; mySet('s') := 389; mySet('t') := 278; mySet('u') := 500; mySet('v') := 500; mySet('w') := 722; mySet('x') := 500; mySet('y') := 500; mySet('z') := 444; mySet('{') := 480; mySet('|') := 200; mySet('}') := 480; mySet('~') := 541; mySet(chr(127)) := 350; mySet(chr(128)) := 500; mySet(chr(129)) := 350; mySet(chr(130)) := 333; mySet(chr(131)) := 500;
	mySet(chr(132)) := 444; mySet(chr(133)) := 1000; mySet(chr(134)) := 500; mySet(chr(135)) := 500; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 556; mySet(chr(139)) := 333; mySet(chr(140)) := 889; mySet(chr(141)) := 350; mySet(chr(142)) := 611; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 333; mySet(chr(146)) := 333; mySet(chr(147)) := 444; mySet(chr(148)) := 444; mySet(chr(149)) := 350; mySet(chr(150)) := 500; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 980;
	mySet(chr(154)) := 389; mySet(chr(155)) := 333; mySet(chr(156)) := 722; mySet(chr(157)) := 350; mySet(chr(158)) := 444; mySet(chr(159)) := 722; mySet(chr(160)) := 250; mySet(chr(161)) := 333; mySet(chr(162)) := 500; mySet(chr(163)) := 500; mySet(chr(164)) := 500; mySet(chr(165)) := 500; mySet(chr(166)) := 200; mySet(chr(167)) := 500; mySet(chr(168)) := 333; mySet(chr(169)) := 760; mySet(chr(170)) := 276; mySet(chr(171)) := 500; mySet(chr(172)) := 564; mySet(chr(173)) := 333; mySet(chr(174)) := 760; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 564; mySet(chr(178)) := 300; mySet(chr(179)) := 300; mySet(chr(180)) := 333; mySet(chr(181)) := 500; mySet(chr(182)) := 453; mySet(chr(183)) := 250; mySet(chr(184)) := 333; mySet(chr(185)) := 300; mySet(chr(186)) := 310; mySet(chr(187)) := 500; mySet(chr(188)) := 750; mySet(chr(189)) := 750; mySet(chr(190)) := 750; mySet(chr(191)) := 444; mySet(chr(192)) := 722; mySet(chr(193)) := 722; mySet(chr(194)) := 722; mySet(chr(195)) := 722; mySet(chr(196)) := 722; mySet(chr(197)) := 722;
	mySet(chr(198)) := 889; mySet(chr(199)) := 667; mySet(chr(200)) := 611; mySet(chr(201)) := 611; mySet(chr(202)) := 611; mySet(chr(203)) := 611; mySet(chr(204)) := 333; mySet(chr(205)) := 333; mySet(chr(206)) := 333; mySet(chr(207)) := 333; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 722; mySet(chr(211)) := 722; mySet(chr(212)) := 722; mySet(chr(213)) := 722; mySet(chr(214)) := 722; mySet(chr(215)) := 564; mySet(chr(216)) := 722; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 722; mySet(chr(222)) := 556; mySet(chr(223)) := 500; mySet(chr(224)) := 444; mySet(chr(225)) := 444; mySet(chr(226)) := 444; mySet(chr(227)) := 444; mySet(chr(228)) := 444; mySet(chr(229)) := 444; mySet(chr(230)) := 667; mySet(chr(231)) := 444; mySet(chr(232)) := 444; mySet(chr(233)) := 444; mySet(chr(234)) := 444; mySet(chr(235)) := 444; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 500; mySet(chr(241)) := 500;
	mySet(chr(242)) := 500; mySet(chr(243)) := 500; mySet(chr(244)) := 500; mySet(chr(245)) := 500; mySet(chr(246)) := 500; mySet(chr(247)) := 564; mySet(chr(248)) := 500; mySet(chr(249)) := 500; mySet(chr(250)) := 500; mySet(chr(251)) := 500; mySet(chr(252)) := 500; mySet(chr(253)) := 500; mySet(chr(254)) := 500; mySet(chr(255)) := 500;
	return mySet;
end getFontTimes;

----------------------------------------------------------------------------------
-- Setting metric for times ITALIC
----------------------------------------------------------------------------------
function getFontTimesi return charSet is
mySet charSet;
begin 
	-- Times italic font.
	mySet(chr(0)) := 250; mySet(chr(1)) := 250; mySet(chr(2)) := 250; mySet(chr(3)) := 250; mySet(chr(4)) := 250; mySet(chr(5)) := 250; mySet(chr(6)) := 250; mySet(chr(7)) := 250; mySet(chr(8)) := 250; mySet(chr(9)) := 250; mySet(chr(10)) := 250; mySet(chr(11)) := 250; mySet(chr(12)) := 250; mySet(chr(13)) := 250; mySet(chr(14)) := 250; mySet(chr(15)) := 250; mySet(chr(16)) := 250; mySet(chr(17)) := 250; mySet(chr(18)) := 250; mySet(chr(19)) := 250; mySet(chr(20)) := 250; mySet(chr(21)) := 250;
	mySet(chr(22)) := 250; mySet(chr(23)) := 250; mySet(chr(24)) := 250; mySet(chr(25)) := 250; mySet(chr(26)) := 250; mySet(chr(27)) := 250; mySet(chr(28)) := 250; mySet(chr(29)) := 250; mySet(chr(30)) := 250; mySet(chr(31)) := 250; mySet(' ') := 250; mySet('!') := 333; mySet('"') := 420; mySet('#') := 500; mySet('$') := 500; mySet('%') := 833; mySet('&') := 778; mySet('''') := 214; mySet('(') := 333; mySet(')') := 333; mySet('*') := 500; mySet('+') := 675;
	mySet(',') := 250; mySet('-') := 333; mySet('.') := 250; mySet('/') := 278; mySet('0') := 500; mySet('1') := 500; mySet('2') := 500; mySet('3') := 500; mySet('4') := 500; mySet('5') := 500; mySet('6') := 500; mySet('7') := 500; mySet('8') := 500; mySet('9') := 500; mySet(':') := 333; mySet(';') := 333; mySet('<') := 675; mySet('=') := 675; mySet('>') := 675; mySet('?') := 500; mySet('@') := 920; mySet('A') := 611;
	mySet('B') := 611; mySet('C') := 667; mySet('D') := 722; mySet('E') := 611; mySet('F') := 611; mySet('G') := 722; mySet('H') := 722; mySet('I') := 333; mySet('J') := 444; mySet('K') := 667; mySet('L') := 556; mySet('M') := 833; mySet('N') := 667; mySet('O') := 722; mySet('P') := 611; mySet('Q') := 722; mySet('R') := 611; mySet('S') := 500; mySet('T') := 556; mySet('U') := 722; mySet('V') := 611; mySet('W') := 833;
	mySet('X') := 611; mySet('Y') := 556; mySet('Z') := 556; mySet('[') := 389; mySet('\') := 278; mySet(']') := 389; mySet('^') := 422; mySet('_') := 500; mySet('`') := 333; mySet('a') := 500; mySet('b') := 500; mySet('c') := 444; mySet('d') := 500; mySet('e') := 444; mySet('f') := 278; mySet('g') := 500; mySet('h') := 500; mySet('i') := 278; mySet('j') := 278; mySet('k') := 444; mySet('l') := 278; mySet('m') := 722;
	mySet('n') := 500; mySet('o') := 500; mySet('p') := 500; mySet('q') := 500; mySet('r') := 389; mySet('s') := 389; mySet('t') := 278; mySet('u') := 500; mySet('v') := 444; mySet('w') := 667; mySet('x') := 444; mySet('y') := 444; mySet('z') := 389; mySet('{') := 400; mySet('|') := 275; mySet('}') := 400; mySet('~') := 541; mySet(chr(127)) := 350; mySet(chr(128)) := 500; mySet(chr(129)) := 350; mySet(chr(130)) := 333; mySet(chr(131)) := 500;
	mySet(chr(132)) := 556; mySet(chr(133)) := 889; mySet(chr(134)) := 500; mySet(chr(135)) := 500; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 500; mySet(chr(139)) := 333; mySet(chr(140)) := 944; mySet(chr(141)) := 350; mySet(chr(142)) := 556; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 333; mySet(chr(146)) := 333; mySet(chr(147)) := 556; mySet(chr(148)) := 556; mySet(chr(149)) := 350; mySet(chr(150)) := 500; mySet(chr(151)) := 889; mySet(chr(152)) := 333; mySet(chr(153)) := 980;
	mySet(chr(154)) := 389; mySet(chr(155)) := 333; mySet(chr(156)) := 667; mySet(chr(157)) := 350; mySet(chr(158)) := 389; mySet(chr(159)) := 556; mySet(chr(160)) := 250; mySet(chr(161)) := 389; mySet(chr(162)) := 500; mySet(chr(163)) := 500; mySet(chr(164)) := 500; mySet(chr(165)) := 500; mySet(chr(166)) := 275; mySet(chr(167)) := 500; mySet(chr(168)) := 333; mySet(chr(169)) := 760; mySet(chr(170)) := 276; mySet(chr(171)) := 500; mySet(chr(172)) := 675; mySet(chr(173)) := 333; mySet(chr(174)) := 760; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 675; mySet(chr(178)) := 300; mySet(chr(179)) := 300; mySet(chr(180)) := 333; mySet(chr(181)) := 500; mySet(chr(182)) := 523; mySet(chr(183)) := 250; mySet(chr(184)) := 333; mySet(chr(185)) := 300; mySet(chr(186)) := 310; mySet(chr(187)) := 500; mySet(chr(188)) := 750; mySet(chr(189)) := 750; mySet(chr(190)) := 750; mySet(chr(191)) := 500; mySet(chr(192)) := 611; mySet(chr(193)) := 611; mySet(chr(194)) := 611; mySet(chr(195)) := 611; mySet(chr(196)) := 611; mySet(chr(197)) := 611;
	mySet(chr(198)) := 889; mySet(chr(199)) := 667; mySet(chr(200)) := 611; mySet(chr(201)) := 611; mySet(chr(202)) := 611; mySet(chr(203)) := 611; mySet(chr(204)) := 333; mySet(chr(205)) := 333; mySet(chr(206)) := 333; mySet(chr(207)) := 333; mySet(chr(208)) := 722; mySet(chr(209)) := 667; mySet(chr(210)) := 722; mySet(chr(211)) := 722; mySet(chr(212)) := 722; mySet(chr(213)) := 722; mySet(chr(214)) := 722; mySet(chr(215)) := 675; mySet(chr(216)) := 722; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 556; mySet(chr(222)) := 611; mySet(chr(223)) := 500; mySet(chr(224)) := 500; mySet(chr(225)) := 500; mySet(chr(226)) := 500; mySet(chr(227)) := 500; mySet(chr(228)) := 500; mySet(chr(229)) := 500; mySet(chr(230)) := 667; mySet(chr(231)) := 444; mySet(chr(232)) := 444; mySet(chr(233)) := 444; mySet(chr(234)) := 444; mySet(chr(235)) := 444; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 500; mySet(chr(241)) := 500;
	mySet(chr(242)) := 500; mySet(chr(243)) := 500; mySet(chr(244)) := 500; mySet(chr(245)) := 500; mySet(chr(246)) := 500; mySet(chr(247)) := 675; mySet(chr(248)) := 500; mySet(chr(249)) := 500; mySet(chr(250)) := 500; mySet(chr(251)) := 500; mySet(chr(252)) := 500; mySet(chr(253)) := 444; mySet(chr(254)) := 500; mySet(chr(255)) := 444;
	return mySet;
end getFontTimesi;

----------------------------------------------------------------------------------
-- Setting metric for times BOLD
----------------------------------------------------------------------------------
function getFontTimesb return charSet is
mySet charSet;
begin 
	-- Times bold font.
	mySet(chr(0)) := 250; mySet(chr(1)) := 250; mySet(chr(2)) := 250; mySet(chr(3)) := 250; mySet(chr(4)) := 250; mySet(chr(5)) := 250; mySet(chr(6)) := 250; mySet(chr(7)) := 250; mySet(chr(8)) := 250; mySet(chr(9)) := 250; mySet(chr(10)) := 250; mySet(chr(11)) := 250; mySet(chr(12)) := 250; mySet(chr(13)) := 250; mySet(chr(14)) := 250; mySet(chr(15)) := 250; mySet(chr(16)) := 250; mySet(chr(17)) := 250; mySet(chr(18)) := 250; mySet(chr(19)) := 250; mySet(chr(20)) := 250; mySet(chr(21)) := 250;
	mySet(chr(22)) := 250; mySet(chr(23)) := 250; mySet(chr(24)) := 250; mySet(chr(25)) := 250; mySet(chr(26)) := 250; mySet(chr(27)) := 250; mySet(chr(28)) := 250; mySet(chr(29)) := 250; mySet(chr(30)) := 250; mySet(chr(31)) := 250; mySet(' ') := 250; mySet('!') := 333; mySet('"') := 555; mySet('#') := 500; mySet('$') := 500; mySet('%') := 1000; mySet('&') := 833; mySet('''') := 278; mySet('(') := 333; mySet(')') := 333; mySet('*') := 500; mySet('+') := 570;
	mySet(',') := 250; mySet('-') := 333; mySet('.') := 250; mySet('/') := 278; mySet('0') := 500; mySet('1') := 500; mySet('2') := 500; mySet('3') := 500; mySet('4') := 500; mySet('5') := 500; mySet('6') := 500; mySet('7') := 500; mySet('8') := 500; mySet('9') := 500; mySet(':') := 333; mySet(';') := 333; mySet('<') := 570; mySet('=') := 570; mySet('>') := 570; mySet('?') := 500; mySet('@') := 930; mySet('A') := 722;
	mySet('B') := 667; mySet('C') := 722; mySet('D') := 722; mySet('E') := 667; mySet('F') := 611; mySet('G') := 778; mySet('H') := 778; mySet('I') := 389; mySet('J') := 500; mySet('K') := 778; mySet('L') := 667; mySet('M') := 944; mySet('N') := 722; mySet('O') := 778; mySet('P') := 611; mySet('Q') := 778; mySet('R') := 722; mySet('S') := 556; mySet('T') := 667; mySet('U') := 722; mySet('V') := 722; mySet('W') := 1000;
	mySet('X') := 722; mySet('Y') := 722; mySet('Z') := 667; mySet('[') := 333; mySet('\') := 278; mySet(']') := 333; mySet('^') := 581; mySet('_') := 500; mySet('`') := 333; mySet('a') := 500; mySet('b') := 556; mySet('c') := 444; mySet('d') := 556; mySet('e') := 444; mySet('f') := 333; mySet('g') := 500; mySet('h') := 556; mySet('i') := 278; mySet('j') := 333; mySet('k') := 556; mySet('l') := 278; mySet('m') := 833;
	mySet('n') := 556; mySet('o') := 500; mySet('p') := 556; mySet('q') := 556; mySet('r') := 444; mySet('s') := 389; mySet('t') := 333; mySet('u') := 556; mySet('v') := 500; mySet('w') := 722; mySet('x') := 500; mySet('y') := 500; mySet('z') := 444; mySet('{') := 394; mySet('|') := 220; mySet('}') := 394; mySet('~') := 520; mySet(chr(127)) := 350; mySet(chr(128)) := 500; mySet(chr(129)) := 350; mySet(chr(130)) := 333; mySet(chr(131)) := 500;
	mySet(chr(132)) := 500; mySet(chr(133)) := 1000; mySet(chr(134)) := 500; mySet(chr(135)) := 500; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 556; mySet(chr(139)) := 333; mySet(chr(140)) := 1000; mySet(chr(141)) := 350; mySet(chr(142)) := 667; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 333; mySet(chr(146)) := 333; mySet(chr(147)) := 500; mySet(chr(148)) := 500; mySet(chr(149)) := 350; mySet(chr(150)) := 500; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 1000;
	mySet(chr(154)) := 389; mySet(chr(155)) := 333; mySet(chr(156)) := 722; mySet(chr(157)) := 350; mySet(chr(158)) := 444; mySet(chr(159)) := 722; mySet(chr(160)) := 250; mySet(chr(161)) := 333; mySet(chr(162)) := 500; mySet(chr(163)) := 500; mySet(chr(164)) := 500; mySet(chr(165)) := 500; mySet(chr(166)) := 220; mySet(chr(167)) := 500; mySet(chr(168)) := 333; mySet(chr(169)) := 747; mySet(chr(170)) := 300; mySet(chr(171)) := 500; mySet(chr(172)) := 570; mySet(chr(173)) := 333; mySet(chr(174)) := 747; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 570; mySet(chr(178)) := 300; mySet(chr(179)) := 300; mySet(chr(180)) := 333; mySet(chr(181)) := 556; mySet(chr(182)) := 540; mySet(chr(183)) := 250; mySet(chr(184)) := 333; mySet(chr(185)) := 300; mySet(chr(186)) := 330; mySet(chr(187)) := 500; mySet(chr(188)) := 750; mySet(chr(189)) := 750; mySet(chr(190)) := 750; mySet(chr(191)) := 500; mySet(chr(192)) := 722; mySet(chr(193)) := 722; mySet(chr(194)) := 722; mySet(chr(195)) := 722; mySet(chr(196)) := 722; mySet(chr(197)) := 722;
	mySet(chr(198)) := 1000; mySet(chr(199)) := 722; mySet(chr(200)) := 667; mySet(chr(201)) := 667; mySet(chr(202)) := 667; mySet(chr(203)) := 667; mySet(chr(204)) := 389; mySet(chr(205)) := 389; mySet(chr(206)) := 389; mySet(chr(207)) := 389; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 778; mySet(chr(211)) := 778; mySet(chr(212)) := 778; mySet(chr(213)) := 778; mySet(chr(214)) := 778; mySet(chr(215)) := 570; mySet(chr(216)) := 778; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 722; mySet(chr(222)) := 611; mySet(chr(223)) := 556; mySet(chr(224)) := 500; mySet(chr(225)) := 500; mySet(chr(226)) := 500; mySet(chr(227)) := 500; mySet(chr(228)) := 500; mySet(chr(229)) := 500; mySet(chr(230)) := 722; mySet(chr(231)) := 444; mySet(chr(232)) := 444; mySet(chr(233)) := 444; mySet(chr(234)) := 444; mySet(chr(235)) := 444; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 500; mySet(chr(241)) := 556;
	mySet(chr(242)) := 500; mySet(chr(243)) := 500; mySet(chr(244)) := 500; mySet(chr(245)) := 500; mySet(chr(246)) := 500; mySet(chr(247)) := 570; mySet(chr(248)) := 500; mySet(chr(249)) := 556; mySet(chr(250)) := 556; mySet(chr(251)) := 556; mySet(chr(252)) := 556; mySet(chr(253)) := 500; mySet(chr(254)) := 556; mySet(chr(255)) := 500;
	return mySet;
end getFontTimesb;

----------------------------------------------------------------------------------
-- Setting metric for times BOLD ITALIC
----------------------------------------------------------------------------------
function getFontTimesbi return charSet is
mySet charSet;
begin 
	-- Times bold italic font.
	mySet(chr(0)) := 250; mySet(chr(1)) := 250; mySet(chr(2)) := 250; mySet(chr(3)) := 250; mySet(chr(4)) := 250; mySet(chr(5)) := 250; mySet(chr(6)) := 250; mySet(chr(7)) := 250; mySet(chr(8)) := 250; mySet(chr(9)) := 250; mySet(chr(10)) := 250; mySet(chr(11)) := 250; mySet(chr(12)) := 250; mySet(chr(13)) := 250; mySet(chr(14)) := 250; mySet(chr(15)) := 250; mySet(chr(16)) := 250; mySet(chr(17)) := 250; mySet(chr(18)) := 250; mySet(chr(19)) := 250; mySet(chr(20)) := 250; mySet(chr(21)) := 250;
	mySet(chr(22)) := 250; mySet(chr(23)) := 250; mySet(chr(24)) := 250; mySet(chr(25)) := 250; mySet(chr(26)) := 250; mySet(chr(27)) := 250; mySet(chr(28)) := 250; mySet(chr(29)) := 250; mySet(chr(30)) := 250; mySet(chr(31)) := 250; mySet(' ') := 250; mySet('!') := 389; mySet('"') := 555; mySet('#') := 500; mySet('$') := 500; mySet('%') := 833; mySet('&') := 778; mySet('''') := 278; mySet('(') := 333; mySet(')') := 333; mySet('*') := 500; mySet('+') := 570;
	mySet(',') := 250; mySet('-') := 333; mySet('.') := 250; mySet('/') := 278; mySet('0') := 500; mySet('1') := 500; mySet('2') := 500; mySet('3') := 500; mySet('4') := 500; mySet('5') := 500; mySet('6') := 500; mySet('7') := 500; mySet('8') := 500; mySet('9') := 500; mySet(':') := 333; mySet(';') := 333; mySet('<') := 570; mySet('=') := 570; mySet('>') := 570; mySet('?') := 500; mySet('@') := 832; mySet('A') := 667;
	mySet('B') := 667; mySet('C') := 667; mySet('D') := 722; mySet('E') := 667; mySet('F') := 667; mySet('G') := 722; mySet('H') := 778; mySet('I') := 389; mySet('J') := 500; mySet('K') := 667; mySet('L') := 611; mySet('M') := 889; mySet('N') := 722; mySet('O') := 722; mySet('P') := 611; mySet('Q') := 722; mySet('R') := 667; mySet('S') := 556; mySet('T') := 611; mySet('U') := 722; mySet('V') := 667; mySet('W') := 889;
	mySet('X') := 667; mySet('Y') := 611; mySet('Z') := 611; mySet('[') := 333; mySet('\') := 278; mySet(']') := 333; mySet('^') := 570; mySet('_') := 500; mySet('`') := 333; mySet('a') := 500; mySet('b') := 500; mySet('c') := 444; mySet('d') := 500; mySet('e') := 444; mySet('f') := 333; mySet('g') := 500; mySet('h') := 556; mySet('i') := 278; mySet('j') := 278; mySet('k') := 500; mySet('l') := 278; mySet('m') := 778;
	mySet('n') := 556; mySet('o') := 500; mySet('p') := 500; mySet('q') := 500; mySet('r') := 389; mySet('s') := 389; mySet('t') := 278; mySet('u') := 556; mySet('v') := 444; mySet('w') := 667; mySet('x') := 500; mySet('y') := 444; mySet('z') := 389; mySet('{') := 348; mySet('|') := 220; mySet('}') := 348; mySet('~') := 570; mySet(chr(127)) := 350; mySet(chr(128)) := 500; mySet(chr(129)) := 350; mySet(chr(130)) := 333; mySet(chr(131)) := 500;
	mySet(chr(132)) := 500; mySet(chr(133)) := 1000; mySet(chr(134)) := 500; mySet(chr(135)) := 500; mySet(chr(136)) := 333; mySet(chr(137)) := 1000; mySet(chr(138)) := 556; mySet(chr(139)) := 333; mySet(chr(140)) := 944; mySet(chr(141)) := 350; mySet(chr(142)) := 611; mySet(chr(143)) := 350; mySet(chr(144)) := 350; mySet(chr(145)) := 333; mySet(chr(146)) := 333; mySet(chr(147)) := 500; mySet(chr(148)) := 500; mySet(chr(149)) := 350; mySet(chr(150)) := 500; mySet(chr(151)) := 1000; mySet(chr(152)) := 333; mySet(chr(153)) := 1000;
	mySet(chr(154)) := 389; mySet(chr(155)) := 333; mySet(chr(156)) := 722; mySet(chr(157)) := 350; mySet(chr(158)) := 389; mySet(chr(159)) := 611; mySet(chr(160)) := 250; mySet(chr(161)) := 389; mySet(chr(162)) := 500; mySet(chr(163)) := 500; mySet(chr(164)) := 500; mySet(chr(165)) := 500; mySet(chr(166)) := 220; mySet(chr(167)) := 500; mySet(chr(168)) := 333; mySet(chr(169)) := 747; mySet(chr(170)) := 266; mySet(chr(171)) := 500; mySet(chr(172)) := 606; mySet(chr(173)) := 333; mySet(chr(174)) := 747; mySet(chr(175)) := 333;
	mySet(chr(176)) := 400; mySet(chr(177)) := 570; mySet(chr(178)) := 300; mySet(chr(179)) := 300; mySet(chr(180)) := 333; mySet(chr(181)) := 576; mySet(chr(182)) := 500; mySet(chr(183)) := 250; mySet(chr(184)) := 333; mySet(chr(185)) := 300; mySet(chr(186)) := 300; mySet(chr(187)) := 500; mySet(chr(188)) := 750; mySet(chr(189)) := 750; mySet(chr(190)) := 750; mySet(chr(191)) := 500; mySet(chr(192)) := 667; mySet(chr(193)) := 667; mySet(chr(194)) := 667; mySet(chr(195)) := 667; mySet(chr(196)) := 667; mySet(chr(197)) := 667;
	mySet(chr(198)) := 944; mySet(chr(199)) := 667; mySet(chr(200)) := 667; mySet(chr(201)) := 667; mySet(chr(202)) := 667; mySet(chr(203)) := 667; mySet(chr(204)) := 389; mySet(chr(205)) := 389; mySet(chr(206)) := 389; mySet(chr(207)) := 389; mySet(chr(208)) := 722; mySet(chr(209)) := 722; mySet(chr(210)) := 722; mySet(chr(211)) := 722; mySet(chr(212)) := 722; mySet(chr(213)) := 722; mySet(chr(214)) := 722; mySet(chr(215)) := 570; mySet(chr(216)) := 722; mySet(chr(217)) := 722; mySet(chr(218)) := 722; mySet(chr(219)) := 722;
	mySet(chr(220)) := 722; mySet(chr(221)) := 611; mySet(chr(222)) := 611; mySet(chr(223)) := 500; mySet(chr(224)) := 500; mySet(chr(225)) := 500; mySet(chr(226)) := 500; mySet(chr(227)) := 500; mySet(chr(228)) := 500; mySet(chr(229)) := 500; mySet(chr(230)) := 722; mySet(chr(231)) := 444; mySet(chr(232)) := 444; mySet(chr(233)) := 444; mySet(chr(234)) := 444; mySet(chr(235)) := 444; mySet(chr(236)) := 278; mySet(chr(237)) := 278; mySet(chr(238)) := 278; mySet(chr(239)) := 278; mySet(chr(240)) := 500; mySet(chr(241)) := 556;
	mySet(chr(242)) := 500; mySet(chr(243)) := 500; mySet(chr(244)) := 500; mySet(chr(245)) := 500; mySet(chr(246)) := 500; mySet(chr(247)) := 570; mySet(chr(248)) := 500; mySet(chr(249)) := 556; mySet(chr(250)) := 556; mySet(chr(251)) := 556; mySet(chr(252)) := 556; mySet(chr(253)) := 444; mySet(chr(254)) := 500; mySet(chr(255)) := 444;
	return mySet;
end getFontTimesbi;

----------------------------------------------------------------------------------
-- Setting metric for Symbol
----------------------------------------------------------------------------------
function getFontSymbol return charSet is
mySet charSet;
begin 
	-- Symbol font.
	mySet(chr(0)) := 250; mySet(chr(1)) := 250; mySet(chr(2)) := 250; mySet(chr(3)) := 250; mySet(chr(4)) := 250; mySet(chr(5)) := 250; mySet(chr(6)) := 250; mySet(chr(7)) := 250; mySet(chr(8)) := 250; mySet(chr(9)) := 250; mySet(chr(10)) := 250; mySet(chr(11)) := 250; mySet(chr(12)) := 250; mySet(chr(13)) := 250; mySet(chr(14)) := 250; mySet(chr(15)) := 250; mySet(chr(16)) := 250; mySet(chr(17)) := 250; mySet(chr(18)) := 250; mySet(chr(19)) := 250; mySet(chr(20)) := 250; mySet(chr(21)) := 250;
	mySet(chr(22)) := 250; mySet(chr(23)) := 250; mySet(chr(24)) := 250; mySet(chr(25)) := 250; mySet(chr(26)) := 250; mySet(chr(27)) := 250; mySet(chr(28)) := 250; mySet(chr(29)) := 250; mySet(chr(30)) := 250; mySet(chr(31)) := 250; mySet(' ') := 250; mySet('!') := 333; mySet('"') := 713; mySet('#') := 500; mySet('$') := 549; mySet('%') := 833; mySet('&') := 778; mySet('''') := 439; mySet('(') := 333; mySet(')') := 333; mySet('*') := 500; mySet('+') := 549;
	mySet(',') := 250; mySet('-') := 549; mySet('.') := 250; mySet('/') := 278; mySet('0') := 500; mySet('1') := 500; mySet('2') := 500; mySet('3') := 500; mySet('4') := 500; mySet('5') := 500; mySet('6') := 500; mySet('7') := 500; mySet('8') := 500; mySet('9') := 500; mySet(':') := 278; mySet(';') := 278; mySet('<') := 549; mySet('=') := 549; mySet('>') := 549; mySet('?') := 444; mySet('@') := 549; mySet('A') := 722;
	mySet('B') := 667; mySet('C') := 722; mySet('D') := 612; mySet('E') := 611; mySet('F') := 763; mySet('G') := 603; mySet('H') := 722; mySet('I') := 333; mySet('J') := 631; mySet('K') := 722; mySet('L') := 686; mySet('M') := 889; mySet('N') := 722; mySet('O') := 722; mySet('P') := 768; mySet('Q') := 741; mySet('R') := 556; mySet('S') := 592; mySet('T') := 611; mySet('U') := 690; mySet('V') := 439; mySet('W') := 768;
	mySet('X') := 645; mySet('Y') := 795; mySet('Z') := 611; mySet('[') := 333; mySet('\') := 863; mySet(']') := 333; mySet('^') := 658; mySet('_') := 500; mySet('`') := 500; mySet('a') := 631; mySet('b') := 549; mySet('c') := 549; mySet('d') := 494; mySet('e') := 439; mySet('f') := 521; mySet('g') := 411; mySet('h') := 603; mySet('i') := 329; mySet('j') := 603; mySet('k') := 549; mySet('l') := 549; mySet('m') := 576;
	mySet('n') := 521; mySet('o') := 549; mySet('p') := 549; mySet('q') := 521; mySet('r') := 549; mySet('s') := 603; mySet('t') := 439; mySet('u') := 576; mySet('v') := 713; mySet('w') := 686; mySet('x') := 493; mySet('y') := 686; mySet('z') := 494; mySet('{') := 480; mySet('|') := 200; mySet('}') := 480; mySet('~') := 549; mySet(chr(127)) := 0; mySet(chr(128)) := 0; mySet(chr(129)) := 0; mySet(chr(130)) := 0; mySet(chr(131)) := 0;
	mySet(chr(132)) := 0; mySet(chr(133)) := 0; mySet(chr(134)) := 0; mySet(chr(135)) := 0; mySet(chr(136)) := 0; mySet(chr(137)) := 0; mySet(chr(138)) := 0; mySet(chr(139)) := 0; mySet(chr(140)) := 0; mySet(chr(141)) := 0; mySet(chr(142)) := 0; mySet(chr(143)) := 0; mySet(chr(144)) := 0; mySet(chr(145)) := 0; mySet(chr(146)) := 0; mySet(chr(147)) := 0; mySet(chr(148)) := 0; mySet(chr(149)) := 0; mySet(chr(150)) := 0; mySet(chr(151)) := 0; mySet(chr(152)) := 0; mySet(chr(153)) := 0;
	mySet(chr(154)) := 0; mySet(chr(155)) := 0; mySet(chr(156)) := 0; mySet(chr(157)) := 0; mySet(chr(158)) := 0; mySet(chr(159)) := 0; mySet(chr(160)) := 750; mySet(chr(161)) := 620; mySet(chr(162)) := 247; mySet(chr(163)) := 549; mySet(chr(164)) := 167; mySet(chr(165)) := 713; mySet(chr(166)) := 500; mySet(chr(167)) := 753; mySet(chr(168)) := 753; mySet(chr(169)) := 753; mySet(chr(170)) := 753; mySet(chr(171)) := 1042; mySet(chr(172)) := 987; mySet(chr(173)) := 603; mySet(chr(174)) := 987; mySet(chr(175)) := 603;
	mySet(chr(176)) := 400; mySet(chr(177)) := 549; mySet(chr(178)) := 411; mySet(chr(179)) := 549; mySet(chr(180)) := 549; mySet(chr(181)) := 713; mySet(chr(182)) := 494; mySet(chr(183)) := 460; mySet(chr(184)) := 549; mySet(chr(185)) := 549; mySet(chr(186)) := 549; mySet(chr(187)) := 549; mySet(chr(188)) := 1000; mySet(chr(189)) := 603; mySet(chr(190)) := 1000; mySet(chr(191)) := 658; mySet(chr(192)) := 823; mySet(chr(193)) := 686; mySet(chr(194)) := 795; mySet(chr(195)) := 987; mySet(chr(196)) := 768; mySet(chr(197)) := 768;
	mySet(chr(198)) := 823; mySet(chr(199)) := 768; mySet(chr(200)) := 768; mySet(chr(201)) := 713; mySet(chr(202)) := 713; mySet(chr(203)) := 713; mySet(chr(204)) := 713; mySet(chr(205)) := 713; mySet(chr(206)) := 713; mySet(chr(207)) := 713; mySet(chr(208)) := 768; mySet(chr(209)) := 713; mySet(chr(210)) := 790; mySet(chr(211)) := 790; mySet(chr(212)) := 890; mySet(chr(213)) := 823; mySet(chr(214)) := 549; mySet(chr(215)) := 250; mySet(chr(216)) := 713; mySet(chr(217)) := 603; mySet(chr(218)) := 603; mySet(chr(219)) := 1042;
	mySet(chr(220)) := 987; mySet(chr(221)) := 603; mySet(chr(222)) := 987; mySet(chr(223)) := 603; mySet(chr(224)) := 494; mySet(chr(225)) := 329; mySet(chr(226)) := 790; mySet(chr(227)) := 790; mySet(chr(228)) := 786; mySet(chr(229)) := 713; mySet(chr(230)) := 384; mySet(chr(231)) := 384; mySet(chr(232)) := 384; mySet(chr(233)) := 384; mySet(chr(234)) := 384; mySet(chr(235)) := 384; mySet(chr(236)) := 494; mySet(chr(237)) := 494; mySet(chr(238)) := 494; mySet(chr(239)) := 494; mySet(chr(240)) := 0; mySet(chr(241)) := 329;
	mySet(chr(242)) := 274; mySet(chr(243)) := 686; mySet(chr(244)) := 686; mySet(chr(245)) := 686; mySet(chr(246)) := 384; mySet(chr(247)) := 384; mySet(chr(248)) := 384; mySet(chr(249)) := 384; mySet(chr(250)) := 384; mySet(chr(251)) := 384; mySet(chr(252)) := 494; mySet(chr(253)) := 494; mySet(chr(254)) := 494; mySet(chr(255)) := 0;
	return mySet;
end getFontSymbol;

----------------------------------------------------------------------------------
-- Setting metric for zapfdingbats
----------------------------------------------------------------------------------
function getFontZapfdingbats return charSet is
mySet charSet;
begin 
	-- zapfdingbats font.
	mySet(chr(0)) := 0; mySet(chr(1)) := 0; mySet(chr(2)) := 0; mySet(chr(3)) := 0; mySet(chr(4)) := 0; mySet(chr(5)) := 0; mySet(chr(6)) := 0; mySet(chr(7)) := 0; mySet(chr(8)) := 0; mySet(chr(9)) := 0; mySet(chr(10)) := 0; mySet(chr(11)) := 0; mySet(chr(12)) := 0; mySet(chr(13)) := 0; mySet(chr(14)) := 0; mySet(chr(15)) := 0; mySet(chr(16)) := 0; mySet(chr(17)) := 0; mySet(chr(18)) := 0; mySet(chr(19)) := 0; mySet(chr(20)) := 0; mySet(chr(21)) := 0;
	mySet(chr(22)) := 0; mySet(chr(23)) := 0; mySet(chr(24)) := 0; mySet(chr(25)) := 0; mySet(chr(26)) := 0; mySet(chr(27)) := 0; mySet(chr(28)) := 0; mySet(chr(29)) := 0; mySet(chr(30)) := 0; mySet(chr(31)) := 0; mySet(' ') := 278; mySet('!') := 974; mySet('"') := 961; mySet('#') := 974; mySet('$') := 980; mySet('%') := 719; mySet('&') := 789; mySet('''') := 790; mySet('(') := 791; mySet(')') := 690; mySet('*') := 960; mySet('+') := 939;
	mySet(',') := 549; mySet('-') := 855; mySet('.') := 911; mySet('/') := 933; mySet('0') := 911; mySet('1') := 945; mySet('2') := 974; mySet('3') := 755; mySet('4') := 846; mySet('5') := 762; mySet('6') := 761; mySet('7') := 571; mySet('8') := 677; mySet('9') := 763; mySet(':') := 760; mySet(';') := 759; mySet('<') := 754; mySet('=') := 494; mySet('>') := 552; mySet('?') := 537; mySet('@') := 577; mySet('A') := 692;
	mySet('B') := 786; mySet('C') := 788; mySet('D') := 788; mySet('E') := 790; mySet('F') := 793; mySet('G') := 794; mySet('H') := 816; mySet('I') := 823; mySet('J') := 789; mySet('K') := 841; mySet('L') := 823; mySet('M') := 833; mySet('N') := 816; mySet('O') := 831; mySet('P') := 923; mySet('Q') := 744; mySet('R') := 723; mySet('S') := 749; mySet('T') := 790; mySet('U') := 792; mySet('V') := 695; mySet('W') := 776;
	mySet('X') := 768; mySet('Y') := 792; mySet('Z') := 759; mySet('[') := 707; mySet('\') := 708; mySet(']') := 682; mySet('^') := 701; mySet('_') := 826; mySet('`') := 815; mySet('a') := 789; mySet('b') := 789; mySet('c') := 707; mySet('d') := 687; mySet('e') := 696; mySet('f') := 689; mySet('g') := 786; mySet('h') := 787; mySet('i') := 713; mySet('j') := 791; mySet('k') := 785; mySet('l') := 791; mySet('m') := 873;
	mySet('n') := 761; mySet('o') := 762; mySet('p') := 762; mySet('q') := 759; mySet('r') := 759; mySet('s') := 892; mySet('t') := 892; mySet('u') := 788; mySet('v') := 784; mySet('w') := 438; mySet('x') := 138; mySet('y') := 277; mySet('z') := 415; mySet('{') := 392; mySet('|') := 392; mySet('}') := 668; mySet('~') := 668; mySet(chr(127)) := 0; mySet(chr(128)) := 390; mySet(chr(129)) := 390; mySet(chr(130)) := 317; mySet(chr(131)) := 317;
	mySet(chr(132)) := 276; mySet(chr(133)) := 276; mySet(chr(134)) := 509; mySet(chr(135)) := 509; mySet(chr(136)) := 410; mySet(chr(137)) := 410; mySet(chr(138)) := 234; mySet(chr(139)) := 234; mySet(chr(140)) := 334; mySet(chr(141)) := 334; mySet(chr(142)) := 0; mySet(chr(143)) := 0; mySet(chr(144)) := 0; mySet(chr(145)) := 0; mySet(chr(146)) := 0; mySet(chr(147)) := 0; mySet(chr(148)) := 0; mySet(chr(149)) := 0; mySet(chr(150)) := 0; mySet(chr(151)) := 0; mySet(chr(152)) := 0; mySet(chr(153)) := 0;
	mySet(chr(154)) := 0; mySet(chr(155)) := 0; mySet(chr(156)) := 0; mySet(chr(157)) := 0; mySet(chr(158)) := 0; mySet(chr(159)) := 0; mySet(chr(160)) := 0; mySet(chr(161)) := 732; mySet(chr(162)) := 544; mySet(chr(163)) := 544; mySet(chr(164)) := 910; mySet(chr(165)) := 667; mySet(chr(166)) := 760; mySet(chr(167)) := 760; mySet(chr(168)) := 776; mySet(chr(169)) := 595; mySet(chr(170)) := 694; mySet(chr(171)) := 626; mySet(chr(172)) := 788; mySet(chr(173)) := 788; mySet(chr(174)) := 788; mySet(chr(175)) := 788;
	mySet(chr(176)) := 788; mySet(chr(177)) := 788; mySet(chr(178)) := 788; mySet(chr(179)) := 788; mySet(chr(180)) := 788; mySet(chr(181)) := 788; mySet(chr(182)) := 788; mySet(chr(183)) := 788; mySet(chr(184)) := 788; mySet(chr(185)) := 788; mySet(chr(186)) := 788; mySet(chr(187)) := 788; mySet(chr(188)) := 788; mySet(chr(189)) := 788; mySet(chr(190)) := 788; mySet(chr(191)) := 788; mySet(chr(192)) := 788; mySet(chr(193)) := 788; mySet(chr(194)) := 788; mySet(chr(195)) := 788; mySet(chr(196)) := 788; mySet(chr(197)) := 788;
	mySet(chr(198)) := 788; mySet(chr(199)) := 788; mySet(chr(200)) := 788; mySet(chr(201)) := 788; mySet(chr(202)) := 788; mySet(chr(203)) := 788; mySet(chr(204)) := 788; mySet(chr(205)) := 788; mySet(chr(206)) := 788; mySet(chr(207)) := 788; mySet(chr(208)) := 788; mySet(chr(209)) := 788; mySet(chr(210)) := 788; mySet(chr(211)) := 788; mySet(chr(212)) := 894; mySet(chr(213)) := 838; mySet(chr(214)) := 1016; mySet(chr(215)) := 458; mySet(chr(216)) := 748; mySet(chr(217)) := 924; mySet(chr(218)) := 748; mySet(chr(219)) := 918;
	mySet(chr(220)) := 927; mySet(chr(221)) := 928; mySet(chr(222)) := 928; mySet(chr(223)) := 834; mySet(chr(224)) := 873; mySet(chr(225)) := 828; mySet(chr(226)) := 924; mySet(chr(227)) := 924; mySet(chr(228)) := 917; mySet(chr(229)) := 930; mySet(chr(230)) := 931; mySet(chr(231)) := 463; mySet(chr(232)) := 883; mySet(chr(233)) := 836; mySet(chr(234)) := 836; mySet(chr(235)) := 867; mySet(chr(236)) := 867; mySet(chr(237)) := 696; mySet(chr(238)) := 696; mySet(chr(239)) := 874; mySet(chr(240)) := 0; mySet(chr(241)) := 874;
	mySet(chr(242)) := 760; mySet(chr(243)) := 946; mySet(chr(244)) := 771; mySet(chr(245)) := 865; mySet(chr(246)) := 771; mySet(chr(247)) := 888; mySet(chr(248)) := 967; mySet(chr(249)) := 888; mySet(chr(250)) := 831; mySet(chr(251)) := 873; mySet(chr(252)) := 927; mySet(chr(253)) := 970; mySet(chr(254)) := 918; mySet(chr(255)) := 0;
	return mySet;
end getFontZapfdingbats;

----------------------------------------------------------------------------------
-- Inclusion des métriques d'une font.
----------------------------------------------------------------------------------
procedure p_includeFont (pfontname varchar2) is
mySet charSet;
begin
  if (pfontname is not null) then 
  	 case pfontname
	 when 'courier' then -- courier
	    mySet := getFontCourier;
		fpdf_charwidths(pfontname||'B') := mySet;
		fpdf_charwidths(pfontname||'I') := mySet;
		fpdf_charwidths(pfontname||'BI') := mySet;
	  when 'helvetica' then	  -- helvetica font.
	  mySet := getFontHelvetica;
	  --
	  when 'helveticaI' then	  -- helvetica italic font.
	  mySet := getFontHelveticai;
	  --
	  when 'helveticaB' then	  -- helvetica bold font.
	  mySet := getFontHelveticab;
	  --
	  when 'helveticaBI' then	  -- helvetica bold italic font.
	  mySet := getFontHelveticabi;
	  --
	  when 'times' then	  -- times font.
	  mySet := getFontTimes;
	  --
	  when 'timesI' then	  -- times italic font.
	  mySet := getFontTimesi;
	  --
	  when 'timesB' then	  -- times bold font.
	  mySet := getFontTimesb;
	  --
	  when 'timesBI' then	  -- times bold italic font.
	  mySet := getFontTimesbi;
	  --
	  when 'symbol' then	  -- symbol font.
	  mySet := getFontSymbol;
	  --
	  when 'zapfdingbats' then	  -- zapfdingbats font.
	  mySet := getFontZapfdingbats;
	  --
	  else null;
	  end case; 
	  fpdf_charwidths(pfontname) := mySet;
  end if; 
end p_includeFont;


----------------------------------------------------------------------------------
-- p_getFontMetrics : récupérer les metric d'une font.
----------------------------------------------------------------------------------
function p_getFontMetrics(pFontName varchar2) return charSet is
mySet charSet;
begin
  if (pfontname is not null) then 
  	 case pfontname
	 when 'courier' then -- courier
	    mySet := getFontCourier;
	  when 'helvetica' then	  -- helvetica font.
	    mySet := getFontHelvetica;
	  --
	  when 'helveticaI' then	  -- helvetica italic font.
	    mySet := getFontHelveticai;
	  --
	  when 'helveticaB' then	  -- helvetica bold font.
	    mySet := getFontHelveticab;
	  --
	  when 'helveticaBI' then	  -- helvetica bold italic font.
	    mySet := getFontHelveticabi;
	  --
	  when 'times' then	  -- times font.
	    mySet := getFontTimes;
	  --
	  when 'timesI' then	  -- times italic font.
	    mySet := getFontTimesi;
	  --
	  when 'timesI' then	  -- times bold font.
	    mySet := getFontTimesb;
	  --
	  when 'timesBI' then	  -- times bold italic font.
	    mySet := getFontTimesbi;
	  --
	  when 'symbol' then	  -- symbol font.
	    mySet := getFontSymbol;
	  --
	  when 'zapfdingbats' then	  -- zapfdingbats font.
	    mySet := getFontZapfdingbats;
	  --
	  else null;
	  end case; 
  end if; 
  return mySet;
end p_getFontMetrics;

----------------------------------------------------------------------------------
-- Parcours le tableau des images et renvoie true si l'image cherché existe 
-- dans le tableau.
----------------------------------------------------------------------------------
function imageExists(pFile varchar2) return boolean is
begin
  if (images.exists(lower(pFile))) then
     return true;
  end if;
  return false;
exception 
  when others then
   error('imageExists : '||sqlerrm);
   return false;
end imageExists;

----------------------------------------------------------------------------------
-- Parcours le tableau des charwidths et renvoie true si il existe pour la font
-- donnée.
----------------------------------------------------------------------------------
function fpdf_charwidthsExists(pFontName varchar2) return boolean is
chTab charSet;
begin
  if (fpdf_charwidths.exists(pFontName)) then
     chTab := fpdf_charwidths(pFontName);
	 if (nvl(chTab.count, 0) > 0) then
       return true;
	 end if;
  end if;
  return false;
exception
  when others then
    return false;
end fpdf_charwidthsExists;

----------------------------------------------------------------------------------
-- Parcours le tableau des fonts et renvoie true si il existe pour la font
-- donnée.
----------------------------------------------------------------------------------
function fontsExists(pFontName varchar2) return boolean is
ft word;
begin
  if (fonts.exists(pFontName)) then
     ft := fonts(pFontName).name||fonts(pFontName).type;
	 if (nvl(ft, 0) != 0) then
       return true;
	 end if;
  end if;
  return false;
exception
  when others then
    return false;
end fontsExists;

--------------------------------------------------------------------------------
-- get an image in a blob from an http url.
-- The image is converted  on the fly to PNG format.
--------------------------------------------------------------------------------
function getImageFromUrl(p_Url varchar2) return ordsys.ordImage is
	myImg ordsys.ordImage; 
	lv_url varchar2(2000) := p_Url;
	urityp URIType;
begin
	 -- normalize url.
     if (instr(lv_url, 'http') = 0 ) then
	   lv_url := 'http://'||owa_util.get_cgi_env('SERVER_NAME')||'/'||lv_url;
	 end if;
	 
	 urityp := URIFactory.getURI(lv_url);

	 myImg := ORDSYS.ORDImage.init();
	 myImg.source.localdata := urityp.getBlob();
	 myImg.setMimeType(urityp.getContentType());	
	 	 
	 begin
	 	  myImg.setProperties();
	 Exception
	 when others then
	 	  null; -- Ignore exceptions, mimetype is enough.
	 end;

	 -- Transform image to PNG if it is a GIF, a JPG or a BMP
	 if (myImg.getFileFormat() != 'PNGF' ) then
	 	myImg.process('fileFormat=PNGF,contentFormat=8bitlutrgb');
		myImg.setProperties();
	 end if;

	 return myImg;
exception
	when others then
		Error('pl_fpdf.getImageFromUrl :'||sqlerrm||', image :'||p_Url);
		return myImg;
  return myImg;
end getImageFromUrl;

--------------------------------------------------------------------------------
-- get an image in a blob from an oracle table.
--------------------------------------------------------------------------------
function getImageFromDatabase(pFile varchar2) return ordsys.ordImage is
  myImg ordsys.ordImage := ordsys.ordImage.init();
begin
  return myImg;
end getImageFromDatabase;

--------------------------------------------------------------------------------
-- Enables debug infos
--------------------------------------------------------------------------------
procedure DebugEnabled is
begin
  gb_mode_debug := true;
end DebugEnabled;

--------------------------------------------------------------------------------
-- disables debug infos
--------------------------------------------------------------------------------
procedure DebugDisabled is
begin
  gb_mode_debug := false;
end DebugDisabled;

--------------------------------------------------------------------------------
-- Returns the k property
--------------------------------------------------------------------------------
function GetScaleFactor return number is
begin
	-- Get scale factor
	return k;
end GetScaleFactor;

--------------------------------------------------------------------------------
-- Returns the Linespacing property
--------------------------------------------------------------------------------
function GetLineSpacing return number is
begin
	-- Get LineSpacing property
	return LineSpacing;
end GetLineSpacing;

--------------------------------------------------------------------------------
-- sets the Linespacing property
--------------------------------------------------------------------------------
Procedure SetLineSpacing (pls number) is
begin
    -- Set LineSpacing property
    LineSpacing := pls;
end SetLineSpacing;

----------------------------------------------------------------------------------
-- Compatibilité PHP -> PLSQL : proc. and func. spécifiques au portages
-- 				 	 		  	ajoutée pour des facilités de traduction
----------------------------------------------------------------------------------
function ord(pStr varchar2) return number is
begin
  return ascii(substr(pStr, 1, 1));
end ord;

function empty (p_myvar varchar2) return boolean is
begin
  if (p_myvar is null) then 
    return true;
  end if; 
  return false;
end empty;

function empty (p_mynum number) return boolean is
begin
  return empty (p_myvar => to_char(p_mynum));
end empty;

function str_replace ( psearch varchar2, preplace varchar2, psubject varchar2) return varchar2 is
begin
  return replace(psubject, psearch, preplace);
end str_replace;

function strlen (pstr varchar2) return number is
begin
  return length(pstr);
end strlen;

function tonumber(v_str in varchar2) return number is
   v_num number;
   v_str2 varchar2(255);
begin
   begin
      v_num := to_number(v_str);
   exception
      when others then
         v_num := null;
   end;
   if v_num is null then
      -- maybe wrong NLS, try again
      v_str2 := replace(v_str,',.','.,');
      begin
         v_num := to_number(v_str2);
      exception
         when others then
            v_num := null;
      end;
   end if;
   return v_num;
end;

function tochar(pnum number, pprecision number default 2) return varchar2 is
mynum word := replace(to_char(pnum),',','.');
ceilnum word;
decnum word;
begin
  if (instr(mynum,'.') = 0) then
    mynum := mynum || '.0';
  end if;
  ceilnum := nvl(substr(mynum,1,instr(mynum,'.')-1), '0');
  decnum := nvl(substr(mynum,instr(mynum,'.')+1), '0');
  decnum := substr(decnum,1, pprecision);
  if (pprecision = 0 ) then 
  	 mynum := ceilnum;
  else
  	 mynum := ceilnum || '.' ||decnum;
  end if; 
  return mynum;
end tochar;

function date_YmdHis (p_date date default sysdate) return varchar2 is
begin
  return to_char(p_date,'YYYYMMDDHH24MISS');
end date_YmdHis;

function is_string (pstr varchar2) return boolean is
temp varchar2(2000);
begin
  temp := to_number(pstr);
  -- Si on passe là c'est que la variable contient un nombre sinon => exception.
  return false;
exception
when others then
  return true;
end is_string;

function function_exists (pname varchar2) return boolean is
begin
  -- Pas de fct fdt de compression zlib sous oracle.
  return false;
end function_exists;

function strtoupper (pstr in out varchar2) return varchar2 is
begin
	 return upper(pstr);
end strtoupper;

function strtolower (pstr in out varchar2) return varchar2 is
begin
	 return lower(pstr);
end strtolower;

function substr_count (ptxt varchar2, pstr varchar2) return number is
  nbr number := 0;
begin
  for i in 1..length(ptxt)
  loop
    if (substr(ptxt,i,1) = pstr) then 
	  nbr := nbr + 1;
	end if; 
  end loop;
  return nbr;
end substr_count;

----------------------------------------------------------------------------------------
--  Traduction des méthodes PHP.
----------------------------------------------------------------------------------------
procedure p_dochecks is
begin
	-- Check for decimal separator 
    execute immediate 'alter session set NLS_NUMERIC_CHARACTERS = '',.''';
end p_dochecks;

----------------------------------------------------------------------------------------
function p_getfontpath return varchar2 is
begin
    -- Procedure inutile avec le PLSQL : toutes les fonts sont chargées en mémoire.
	return null;
end p_getfontpath;

----------------------------------------------------------------------------------------
procedure p_out(pstr varchar2 default null, pCRLF boolean default true) is 
lv_CRLF varchar2(2) := null;
begin
    if (pCRLF) then
	  lv_CRLF := chr(10);
	end if;
	-- Add a line to the document
	if(state = 2) then
		pages(page):= pages(page) || pstr || lv_CRLF;
	else
		pdfDoc(pdfDoc.last + 1) :=  pstr || lv_CRLF;
	end if; 
exception 
  when others then
   -- bug('p_out : '||sqlerrm);
   error('p_out : '||sqlerrm);
end p_out;

----------------------------------------------------------------------------------------
procedure p_newobj is
begin
	-- Begin a new object
	n := n + 1;
	offsets(n) := getPDFDocLength();
	p_out(n || ' 0 obj');
exception 
  when others then
   error('p_newobj : '||sqlerrm);
end p_newobj;

----------------------------------------------------------------------------------------
function p_escape(pstr varchar2) return varchar2 is
begin
	-- Add \ before \, ( and )
  return str_replace(')','\)',str_replace('(','\(',str_replace('\\','\\\\',pstr)));
  --return str_replace('\\','\\\\',pstr);
end p_escape;

----------------------------------------------------------------------------------------
function p_textstring(pstr varchar2) return varchar2 is
begin
	-- Format a text string
	return '(' || p_escape(pstr) || ')';
end p_textstring;

----------------------------------------------------------------------------------------
procedure p_putstream(pstr varchar2) is 
begin
	p_out('stream');
	p_out(pstr);
	p_out('endstream');
exception 
  when others then
   error('p_putstream : '||sqlerrm);
end p_putstream;

----------------------------------------------------------------------------------------
procedure p_putstream(pData in out NOCOPY blob) is 
	offset integer := 1;
    lv_content_length number := dbms_lob.getlength(pdata);
	buf_size integer := 2000;
	buf raw(2000);
begin
	p_out('stream');
	-- read the blob and put it in small pieces in a varchar
	while offset < lv_content_length loop
	  dbms_lob.read(pData,buf_size,offset,buf);
	  p_out(utl_raw.cast_to_varchar2(buf), false);
	  offset := offset + buf_size;
	end loop;
	-- put a CRLF at te end of the blob
	p_out(chr(10), false);
	p_out('endstream');
exception 
  when others then
   error('p_putstream : '||sqlerrm);
end p_putstream;

----------------------------------------------------------------------------------------
procedure p_putxobjectdict is
v txt;
begin
   v := images.first;
   while (v is not null) loop
	 p_out('/I' || images(v).i || ' ' || images(v).n || ' 0 R');
	 v := images.next(v);
   end loop;
exception
  when others then
  error('p_putxobjectdict : '||sqlerrm);
end p_putxobjectdict;

----------------------------------------------------------------------------------------
procedure p_putresourcedict is
v varchar2(200);
begin
	p_out('/ProcSet [/PDF /Text /ImageB /ImageC /ImageI]');
	p_out('/Font <<');
	v := fonts.first;
	while (v is not null) 
	loop
	    p_out('/F' || fonts(v).i || ' ' || fonts(v).n  ||' 0 R');
	    v := fonts.next(v);
	end loop;
	p_out('>>');
	p_out('/XObject <<');
	p_putxobjectdict();
	p_out('>>');
exception 
  when others then
   error('p_putresourcedict : '||sqlerrm);
end p_putresourcedict;

----------------------------------------------------------------------------------------
procedure p_putfonts is 
nf number := n;
i pls_integer;
k varchar2(200);
v varchar2(200);
myFont varchar2(2000);
mySet charSet;
myHeader boolean;
myType word;
myName word;
myFile word;
s varchar2(2000);
cw charSet;
theType word;
methode word;
-- plsqlmethode word;
begin
    null;
	i := diffs.first;
	while (i is not null) 
	loop
		-- Encodings
		p_newobj();
		p_out('<</Type /Encoding /BaseEncoding /WinAnsiEncoding /Differences ['|| diffs(i) ||']>>');
		p_out('endobj');
	    i:= diffs.next(i);
	end loop;
		
	-- foreach($this->FontFiles as $file=>$info)
	v := FontFiles.first;
	while (v is not null) 
	loop
		null;
		-- Font file embedding
		p_newobj();
		FontFiles(v).n:= n;
		myFont := null;
		
		
		mySet := p_getFontMetrics(FontFiles(v).file);
		for i in mySet.first..mySet.last loop
		  myFont := myFont || mySet(i);
		end loop;
		
		if (mySet.count = 0) then
		  Error('Font file not found');
		end if;
		
		if(FontFiles(v).length2 is not null) then
		    
			myHeader := false;
			if ( ord(myFont) = 128) then
			  myHeader := true;
			end if;
			
			if(myHeader) then
				-- Strip first binary header
				myFont := substr(myFont,6);
			end if; 
			
			if(myHeader and ord(substr(myFont,(FontFiles(v).length1), 1)) = 128) then
				-- Strip second binary header
				myFont := substr(myFont, 1, FontFiles(v).length1) || substr(myFont, FontFiles(v).length1 + 6);
			end if; 
		end if; 
		p_out('<</Length ' || strlen(myFont));

		p_out('/Length1 ' || FontFiles(v).length1);
		if(FontFiles(v).length2 is not null) then
			p_out('/Length2 '|| FontFiles(v).length2 ||' /Length3 0');
		end if; 
		p_out('>>');
		p_putstream(myFont);
		p_out('endobj');
		
		v := FontFiles.next(v);
	end loop;

	
	k := fonts.first;
	while (k is not null) loop
	
	
	--foreach(fonts as $k=>myFont)
	--{ 
		-- Font objects
		fonts(k).n := n+1;
		myType := fonts(k).type;
		myName := fonts(k).name;
		if(myType = 'core') then
			-- Standard font
			p_newobj();
			p_out('<</Type /Font');
			p_out('/BaseFont /' || myName);
			p_out('/Subtype /Type1');
			if(lower(myName) != 'symbol' and lower(myName) != 'zapfdingbats') then
				p_out('/Encoding /WinAnsiEncoding');
			end if;
			p_out('>>');
			p_out('endobj');
		elsif(lower(myType) = 'type1' or lower(myType) = 'truetype') then
			-- Additional Type1 or TrueType font
			p_newobj();
			p_out('<</Type /Font');
			p_out('/BaseFont /' || myName);
			p_out('/Subtype /' || myType);
			p_out('/FirstChar 32 /LastChar 255');
			p_out('/Widths ' || (n+1) || ' 0 R');
			p_out('/FontDescriptor ' || (n+2) || ' 0 R');
			if(fonts(k).enc is not null) then 			
				if(fonts(k).diff is not null) then
					p_out('/Encoding ' || (nf + fonts(k).diff) || ' 0 R');
				else
					p_out('/Encoding /WinAnsiEncoding');
			    end if;
			end if;
			p_out('>>');
			p_out('endobj');
			-- Widths
			p_newobj();
			
			cw := fonts(k).cw;
			s := '[';
			for i in 32..255 loop
				s := s || cw(chr(i)) || ' ';
		    end loop;
			p_out(s || ']');
			p_out('endobj');
			-- Descriptor
			p_newobj();
			s := '<</Type /FontDescriptor /FontName /' || myName;
			
			for l in fonts(k).dsc.first..fonts(k).dsc.last loop
				s := s || ' /' || l || ' ' || fonts(k).dsc(l);
			end loop;
			
			myFile := fonts(k).file;
			if (myFile is not null) then
			    if (lower(myType) = 'type1') then
				  theType := '';
				 else 
				  theType := '2';
				 end if;
				 s := s || ' /FontFile' || theType || ' ' || FontFiles(myFile).n || ' 0 R';
			end if;
			p_out(s || '>>');
			p_out('endobj');
		else
			-- Allow for additional types
			methode := 'p_put' || strtolower(myType);
			
			if(not methode_exists(methode)) then
				Error('Unsupported font type: ' || myType);
-- 			else
-- 			  plsqlmethode := 'begin pl_fpdf.'|| methode ||'(''' || fonts(k) || '''); end';
-- 			  execute immediate plsqlmethode;
			end if;
		end if;
		
		k := fonts.next(k);
	end loop;
exception 
  when others then
   error('p_putfonts : '||sqlerrm);
end p_putfonts;

----------------------------------------------------------------------------------------
procedure p_putimages is
  filter word;
  info recImage;
  v txt;
  trns txt;
  pal  txt;
begin
  if (b_compress) then
    filter := '/Filter /FlateDecode ';
  else
    filter := '';
  end if;
	--while(list($file,$info)=each($this->images))
	v := images.first;
	while (v is not null)  loop
		p_newobj();
		images(v).n := n;
	    info := images(v);
		p_out('<</Type /XObject');
		p_out('/Subtype /Image');
		p_out('/Width ' || info.w);
		p_out('/Height ' || info.h);
		if(info.cs = 'Indexed') then
			p_out('/ColorSpace [/Indexed /DeviceRGB ' || to_char(strlen(info.pal) / 3 - 1) || ' ' || to_char(n+1) || ' 0 R]');
		else
			p_out('/ColorSpace /' || info.cs);
			if(info.cs = 'DeviceCMYK') then
				p_out('/Decode (1 0 1 0 1 0 1 0)');
			end if;
		end if;

		p_out('/BitsPerComponent ' || info.bpc);
		if(info.f is not null) then
			p_out('/Filter /' || info.f);
		end if;
		if(info.parms is not null) then
			p_out(info.parms);
		end if;
		
		if(info.trns.first is not null ) then
			 trns := '';
			for i in info.trns.first..info.trns.count  loop
				 trns := trns || info.trns(i) || ' ' || info.trns(i) || ' ';
			end loop;
			p_out('/Mask (' || trns || ')');
		end if;

		p_out('/Length ' || dbms_lob.getlength(info.data) || '>>');
		p_putstream(info.data);
		images(v).data := null;
		p_out('endobj');

		--Palette
		if(info.cs = 'Indexed') then
			p_newobj();
			 if (b_compress) then 
			   -- gzcompress($info('pal'))
			   null;
			 else
			   pal := info.pal;
			 end if;
			p_out('<<' || filter || '/Length ' || strlen(pal) || '>>');
			p_putstream(pal);
			p_out('endobj');
		end if;
		v := images.next(v);
	end loop;
exception
  when others then
    error('p_putimages : '||sqlerrm);
end p_putimages;


----------------------------------------------------------------------------------------
procedure p_putresources is
begin
	p_putfonts();
	p_putimages();
	-- Resource dictionary
	offsets(2):= getPDFDocLength();
	p_out('2 0 obj');
	p_out('<<');
	p_putresourcedict();
	p_out('>>');
	p_out('endobj');
exception
  when others then
    error('p_putresources : '||sqlerrm);
end p_putresources;

----------------------------------------------------------------------------------------
procedure p_putinfo is
begin
	p_out('/Producer ' || p_textstring('PL_FPDF ' || PL_FPDF_VERSION || ' portage pour Laclasse.com par P.G. Levallois de la version '|| FPDF_VERSION ||' de PHP/FPDF d''Olivier Plathey.'));
	if(not empty(title)) then
		p_out('/Title ' || p_textstring(title));
	end if; 
	if(not empty(subject)) then
		p_out('/Subject ' || p_textstring(subject));
	end if; 
	if(not empty(author)) then
		p_out('/Author ' || p_textstring(author));
	end if; 
	if(not empty(keywords)) then
		p_out('/Keywords ' || p_textstring(keywords));
	end if; 
	if(not empty(creator)) then
		p_out('/Creator ' || p_textstring(creator));
	end if; 
	p_out('/CreationDate ' || p_textstring('D:' || date_YmdHis()));
exception
  when others then
    error('p_putinfo : '||sqlerrm);
end p_putinfo;

----------------------------------------------------------------------------------------
procedure p_putcatalog is
begin
	p_out('/Type /Catalog');
	p_out('/Pages 1 0 R');
	if(ZoomMode='fullpage') then
		p_out('/OpenAction [3 0 R /Fit]');
	elsif(ZoomMode='fullwidth') then
		p_out('/OpenAction [3 0 R /FitH null]');
	elsif(ZoomMode='real') then
		p_out('/OpenAction [3 0 R /XYZ null null 1]');
	elsif(not is_string(ZoomMode)) then
		p_out('/OpenAction [3 0 R /XYZ null null ' || (ZoomMode/100) || ']');
    end if; 
	if(LayoutMode='single') then
		p_out('/PageLayout /SinglePage');
	elsif(LayoutMode='continuous') then
		p_out('/PageLayout /OneColumn');
	elsif(LayoutMode='two') then
		p_out('/PageLayout /TwoColumnLeft');
    end if; 
exception
  when others then
    error('p_putcatalog : '||sqlerrm);
end p_putcatalog;


----------------------------------------------------------------------------------------
procedure p_putheader is
begin
	p_out('%PDF-' || PDFVersion);
end p_putheader;


----------------------------------------------------------------------------------------
procedure p_puttrailer is 
begin
	p_out('/Size ' || (n+1));
	p_out('/Root ' || n || ' 0 R');
	p_out('/Info ' || (n-1) || ' 0 R');
end p_puttrailer;

----------------------------------------------------------------------------------------
procedure p_endpage is
begin
	-- End of page contents
	state:=1;
end p_endpage;

----------------------------------------------------------------------------------------
procedure p_putpages is
   nb number := page;
   filter varchar2(200);
   annots bigtext;
   rect txt;
   -- l Array2dim;
   -- h number;
   kids txt;
   v_0 varchar2(255);
   v_1 varchar2(255);
   v_2 varchar2(255);
   v_3 varchar2(255);
   v_4 varchar2(255);
   v_0n number;
   v_1n number;
   v_2n number;
   v_3n number;
begin
   -- Replace number of pages
	 if not empty(AliasNbPages) then
		   for i in 1..nb loop
		      pages(i) := str_replace(AliasNbPages,nb,pages(i));
		   end loop;
	 end if; 
	 
	 if DefOrientation = 'P' then
		  wPt:=fwPt;
		  hPt:=fhPt;
	 else
		  wPt:=fhPt;
		  hPt:=fwPt;
   end if; 
	 
	 if (b_compress) then 
	    filter := '/Filter /FlateDecode ';
	 else
	    filter := '';
	 end if; 
	 
   for i in 1..nb loop
		  -- Page
		  p_newobj();
		  p_out('<</Type /Page');
		  p_out('/Parent 1 0 R');
		  if(OrientationChanges.exists(i)) then
			   p_out('/MediaBox [0 0 '||tochar(hPt)||' '||tochar(wPt)||']');
	    end if; 
		  p_out('/Resources 2 0 R');
		
      if(PageLinks.exists(i)) then
			   --Links     [one/page]
			   annots := '/Annots [';
			   --for v in PageLinks(i).first..PageLinks(i).last loop
         v_0 := PageLinks(i).zero;
         v_0n := tonumber(v_0);
         v_1 := PageLinks(i).un;
         v_1n := tonumber(v_1);
         v_2 := PageLinks(i).deux;
         v_2n := tonumber(v_2);
         v_3 := PageLinks(i).trois;
         v_3n := tonumber(v_3);
         v_4 := PageLinks(i).quatre; 
			   rect := tochar(v_0) || ' ' || tochar(v_1) || ' ' || tochar(v_0n + v_2n) 
            || ' ' || tochar(v_1n - v_3n);
			   annots := annots || '<</Type /Annot /Subtype /Link /Rect [' || rect || 
            '] /Border [0 0 0] ';
         if is_string(PageLinks(i).quatre) then
					  annots := annots ||'/A <</S /URI /URI '||p_textstring(PageLinks(i).quatre) 
               || '>>>>';
             /* ????
				else
					l := links(PageLinks(i).quatre);
					if (OrientationChanges(l.zero) is not null) then
					  h := wPt;
					else
					  h := hPt;
					end if;
					annots := annots || '/Dest ('||tochar(1 + 2 * l.zero,2)||' 0 R /XYZ 0 '||tochar(h - l.un * k)||' null)>>';
                */
         end if;
			   --end loop;
			   p_out(annots || ']');
      end if; 


		  p_out('/Contents ' || to_char(n+1) || ' 0 R>>');
		  p_out('endobj');
		  -- Page content
		  -- Pas de compression : oracle ne sait pas faire.
	    p_newobj();
	    p_out('<<' || filter || '/Length ' || strlen(pages(i)) || '>>');
	    p_putstream(pages(i));
	    p_out('endobj');
   end loop;

	 -- Pages root
	 offsets(1):=getPDFDocLength();
	 p_out('1 0 obj');
	 p_out('<</Type /Pages');
	 kids := '/Kids [';
     
      -- Bug dicoverd by Alexandre : arodichevski@newmed.net
	 --for i in 0..nb loop
     for i in 0..nb-1 loop
	    kids := kids || to_char(3+2*i) || ' 0 R ';
	 end loop;

	 p_out( kids || ']');
	 p_out('/Count '|| nb);
	 p_out('/MediaBox [0 0 '||tochar(wPt)||' '||tochar(hPt)||']');
	 p_out('>>');
	 p_out('endobj');
exception
   when others then
      error('p_putpages : '||sqlerrm);
end p_putpages;

----------------------------------------------------------------------------------------
procedure p_enddoc is
o number;
begin
   
	p_putheader();
     
	p_putpages();
     
	p_putresources();
    
	-- Info
	p_newobj();
	p_out('<<');
	p_putinfo();
	p_out('>>');
	p_out('endobj');
   
	-- Catalog
	p_newobj();
	p_out('<<');
	p_putcatalog();
	p_out('>>');
	p_out('endobj');
	
    -- Cross-ref
	o := getPDFDocLength();
	p_out('xref');
	p_out('0 ' || (n+1));
	p_out('0000000000 65535 f ');
    
	for i in 1..n 
	loop
	  p_out(substr('0000000000', 1, 10 - length(offsets(i)) ) ||offsets(i) || ' 00000 n ');
	end loop;
	-- Trailer
	p_out('trailer');
	p_out('<<');
	p_puttrailer();
	p_out('>>');
	p_out('startxref');
	p_out(o);
	p_out('%%EOF');
	state := 3;
    
exception
  when others then
    error('p_enddoc : '||sqlerrm);
end p_enddoc;

----------------------------------------------------------------------------------------
procedure p_beginpage(orientation varchar2) is
Myorientation word := orientation;
begin
	page := page + 1;
	pages(page):='';
	state:=2;
	x:=lMargin;
	y:=tMargin;
	FontFamily:='';
	-- Page orientation
	if(empty(Myorientation)) then
		Myorientation:=DefOrientation;
	else
	    Myorientation := substr(Myorientation, 1, 1);
		Myorientation:=strtoupper(Myorientation);
		if(Myorientation!=DefOrientation) then
			OrientationChanges(page):=true;
		end if; 
	end if; 
	if(Myorientation!=CurOrientation) then
		-- Change orientation
		if(orientation='P') then
			wPt:=fwPt;
			hPt:=fhPt;
			w:=fw;
			h:=fh;
		else
			wPt:=fhPt;
			hPt:=fwPt;
			w:=fh;
			h:=fw;
		end if; 
		pageBreakTrigger:=h-bMargin;
		CurOrientation:=Myorientation;
	end if; 
exception
  when others then
    error('p_beginpage : '||sqlerrm);
end p_beginpage;

----------------------------------------------------------------------------------------
function p_dounderline(px number,py number,ptxt varchar2) return varchar2 is
up word := CurrentFont.up;
ut word := CurrentFont.ut;
w number := 0;
begin
	w:=GetStringWidth(ptxt) + ws * substr_count(ptxt,' ');
	return tochar(px*k,2)||' '||tochar((h-(py-up/1000*fontsize))*k,2)||' '||tochar(w*k,2)||' '||tochar(-ut/1000*fontsizePt,2)||' re f';
exception
  when others then
    error('p_dounderline : '||sqlerrm);
end p_dounderline;


--------------------------------------------------------------------------------
-- Function to convert a binary unsigned integer
-- into a PLSQL number
--------------------------------------------------------------------------------
function p_freadint( p_data in varchar2 ) return number
is
    l_number number default 0;
    l_bytes  number default length(p_data);
	big_endian constant boolean default true;
begin
    if (big_endian)
    then
        for i in 1 .. l_bytes loop
            l_number := l_number +
                              ascii(substr(p_data,i,1)) *
                                           power(2,8*(i-1));
        end loop;
    else
        for i in 1 .. l_bytes loop
            l_number := l_number +
                         ascii(substr(p_data,l_bytes-i+1,1)) *
                         power(2,8*(i-1));
        end loop;
    end if;

    return l_number;
end p_freadint;


/*
--------------------------------------------------------------------------------
-- Parse an image
--------------------------------------------------------------------------------
function p_parseImage(pFile varchar2) return recImage is
  myImg ordsys.ordImage := ordsys.ordImage.init();
  myImgInfo recImage;
  myCtFormat word;
  -- colspace word;
  myblob blob;
  png_signature constant varchar2(8)  := chr(137) || 'PNG' || chr(13) || chr(10) || chr(26) || chr(10);
  amount number;
  f number default 1;
  buf varchar2(8192);
  bufRaw raw(32000);
  amount_rd number;
  amount_wr number;
  offset_rd number;
  offset_wr number;
  ct word;
  colors pls_integer;
  n number;
  myType word;
  -- NullTabN tn;
  imgDataStartsHere number;
  imgDataStopsHere number;
  nb_chuncks number;
  ---------------------------------------------------------------------------------------------
  function freadb(pBlob in out nocopy blob, pHandle in out number, pLength in out number) return raw is
	l_data_raw  raw(8192);
    l_hdr_size  number default 2000;
  begin
	dbms_lob.read(pBlob, pLength, pHandle, l_data_raw);
	pHandle := pHandle + pLength;
	return l_data_raw;
  end freadb;

  function fread(pBlob in out nocopy blob, pHandle in out number, pLength in out number) return varchar2 is
  begin
	return utl_raw.cast_to_varchar2(freadb(pBlob, pHandle, pLength));
  end fread;
  
  ---------------------------------------------------------------------------------------------

begin
    myImgInfo.data := empty_blob();
	myImg := getImageFromUrl(pFile);
	myCtFormat := myImg.getContentFormat();
	myblob := myImg.getContent();
	myImgInfo.i := 1;
	-- reading the blob
	amount := 8;
	--Check signature
	if(fread(myblob, f, amount) != png_signature ) then
	    Error('Not a PNG file: ' || pFile);
	end if;
	
	-- Read header chunk
	amount := 4;
	buf := fread(myblob, f, amount);
	
	buf := fread(myblob, f, amount);	
	if(buf != 'IHDR') then
	   Error('Incorrect PNG file: ' || pFile);
	end if;
	
    myImgInfo.w := myImg.getWidth();
    myImgInfo.h := myImg.getHeight();

	-- ^^^ I have already get width and height, so go forward (read 4 Bytes twice)
	buf := fread(myblob, f, amount);
	buf := fread(myblob, f, amount);

	amount := 1;
	
	myImgInfo.bpc := ord(fread(myblob, f, amount));	
	if( myImgInfo.bpc > 8) then
		Error('16-bit depth not supported: ' || pFile);    
	end if;  
	
	ct := ord(fread(myblob, f, amount));	
	if( ct = 0 ) then
		myImgInfo.cs := 'DeviceGray';
	elsif( ct = 2 ) then
		myImgInfo.cs := 'DeviceRGB';
	elsif( ct = 3 ) then
		myImgInfo.cs := 'Indexed';
	else
		Error('Alpha channel not supported: ' || pFile);
    end if;
	if( ord(fread(myblob, f, amount)) != 0 ) then
		Error('Unknown compression method: ' || pFile);
	end if;
	if( ord(fread(myblob, f, amount)) != 0 ) then
		Error('Unknown filter method: ' || pFile);
	end if;
	if( ord(fread(myblob, f, amount)) != 0 ) then
		Error('Interlacing not supported: ' || pFile);
	end if;
	
	amount := 4;
	buf := fread(myblob, f, amount);
	
	if (ct = 2 ) then
	  colors := 3;
	else
	  colors := 1;
	end if;
	
	myImgInfo.parms := '/DecodeParms <</Predictor 15 /Colors ' || to_char(colors) || ' /BitsPerComponent ' || myImgInfo.bpc || ' /Columns ' || myImgInfo.w || '>>';
	-- scan chunks looking for palette, transparency and image data
	loop
	    amount := 4;
		n := utl_raw.cast_to_binary_integer(freadb(myblob, f, amount));
		myType := fread(myblob, f, amount);
		if(myType = 'PLTE') then
			-- Read palette
			amount := n;
			myImgInfo.pal := fread(myblob, f, amount);
			amount := 4;
			buf := fread(myblob, f, amount);
		elsif(myType = 'tRNS') then
			--   Read transparency info
			amount := n;
			buf := fread(myblob, f, amount);
			if(ct = 0) then
			    myImgInfo.trns(1) := ord(substr(buf,1,1));
			elsif( ct = 2) then
			   myImgInfo.trns(1) := ord(substr(buf,1,1));
			   myImgInfo.trns(2) := ord(substr(buf,3,1));
			   myImgInfo.trns(3) := ord(substr(buf,5,1));
			else
				if(instr(buf,chr(0)) > 0) then
					myImgInfo.trns(1) := instr(buf,chr(0));
				end if;
			end if;
			amount := 4;
			buf := fread(myblob, f, amount);
		elsif(myType = 'IDAT') then
			-- Read image data block after the loop, just mark the begin of data
			imgDataStartsHere := f;
			exit;
		elsif(myType = 'IEND') then

			exit;
		else
			amount := n + 4;
			buf := fread(myblob, f, amount);
		end if;
		exit when n is null or n = 0;
	end loop;
	
	imgDataStopsHere := dbms_lob.instr(myblob, utl_raw.cast_to_raw('IEND'),1,1);
	-- copy image in the structure.
	amount_rd := 8192;
	amount_wr := 8192;
	offset_rd := 1;
	offset_wr := 1;
	nb_chuncks := ceil(((imgDataStopsHere - imgDataStartsHere)) / amount_rd);
	dbms_lob.createtemporary(myImgInfo.data, true);
	for i in 1..nb_chuncks loop
	   offset_rd := imgDataStartsHere + ((i - 1) * amount_rd);

	   dbms_lob.read(myblob, amount_rd, offset_rd, bufRaw);
	   offset_wr := ((i - 1) * amount_wr) + 1;
	   amount_wr := amount_rd;
	   dbms_lob.write(myImgInfo.data, amount_wr, offset_wr, bufRaw);
	end loop;
	if( myImgInfo.cs = 'Indexed' and myImgInfo.pal is null) then
		Error('Missing palette in '|| pFile);
	end if;
    myImgInfo.f := 'FlateDecode';
    return myImgInfo;
exception
  when others then
    Error('p_parseImage : '||SQLERRM);
	return myImgInfo;
end p_parseImage;

*/

--------------------------------------------------------------------------------
-- Parse an image
--------------------------------------------------------------------------------
function p_parseImage(pFile varchar2) return recImage is
  myImg ordsys.ordImage := ordsys.ordImage.init();
  myImgInfo recImage;
  myCtFormat word;
  colspace word;
  myblob blob;
  chunk_content blob;
  png_signature constant varchar2(8)  := chr(137) || 'PNG' || chr(13) || chr(10) || chr(26) || chr(10);
  signature_len integer := 8;
  chunklength_len integer := 4;
  chunktype_len integer := 4;
  chunkdata_len integer;
  widthheight_len integer := 8;
  hdrflag_len integer := 1;
  crc_len integer := 4;
  chunk_num integer := 0;
  --amount number;
  f number default 1;
  f_chunk number default 1;
  buf varchar2(8192);
  ct word;
  colors pls_integer;
  n number;
  myType word;
  NullTabN tn;
  imgDataStartsHere number;
  imgDataStopsHere number;
  nb_chuncks number;
  ---------------------------------------------------------------------------------------------
  function freadb(pBlob in out nocopy blob, pHandle in out number, pLength in out number) return raw is
    l_data_raw  raw(8192);
    l_hdr_size  number default 2000;
  begin
    dbms_lob.read(pBlob, pLength, pHandle, l_data_raw);
    pHandle := pHandle + pLength;
    return l_data_raw;
  end freadb;

  function fread(pBlob in out nocopy blob, pHandle in out number, pLength in out number) return varchar2 is
  begin
    return utl_raw.cast_to_varchar2(freadb(pBlob, pHandle, pLength));
  end fread;
  
  procedure fread_blob(pBlob in out nocopy blob, pHandle in out number, 
                       pLength in out number, pDestBlob in out nocopy blob ) is
  begin
    dbms_lob.trim( pDestBlob, 0);
    dbms_lob.copy( pDestBlob, pBlob, pLength, 1, pHandle );
    pHandle := pHandle + pLength;
  end fread_blob;
  
  ---------------------------------------------------------------------------------------------

begin
  dbms_lob.createtemporary(chunk_content, true );
  dbms_lob.open(chunk_content,dbms_lob.LOB_READWRITE);
  --we use the package level imgBlob variable so the temp blob will persist throughout pdf creation.
  dbms_lob.createtemporary(imgBlob, true );
  myImgInfo.data := imgBlob;
  dbms_lob.open(myImgInfo.data,dbms_lob.LOB_READWRITE);
     myImg := getImageFromUrl(pFile);
    myCtFormat := myImg.getContentFormat();
    myblob := myImg.getContent();
    myImgInfo.i := 1;
    -- reading the blob

    --Check signature
    if(fread(myblob, f, signature_len) != png_signature ) then
        Error('Not a PNG file: ' || pFile);
    end if;

  myImgInfo.w := myImg.getWidth();
  myImgInfo.h := myImg.getHeight();

    -- scan chunks looking for palette, transparency and image data
    loop
    
        chunkdata_len := utl_raw.cast_to_binary_integer(freadb(myblob, f, chunklength_len));
        myType := fread(myblob, f, chunktype_len);
    --read chunk contents into separate blob
    if( chunkdata_len > 0 ) then
      fread_blob(myblob,f,chunkdata_len,chunk_content);
      f_chunk := 1;
    end if;
    chunk_num := chunk_num + 1;
    --discard the crc
    buf := fread(myblob, f, crc_len);
    if( chunk_num = 1 and myType != 'IHDR' ) then
      Error('Incorrect PNG file: ' || pFile);
    elsif(myType = 'IHDR') then
      -- ^^^ I have already get width and height, so go forward (read 4 Bytes twice)
      buf := fread(chunk_content, f_chunk, widthheight_len);

      myImgInfo.bpc := ord(fread(chunk_content, f_chunk, hdrflag_len));    
      if( myImgInfo.bpc > 8) then
        Error('16-bit depth not supported: ' || pFile);    
      end if;  
      
      ct := ord(fread(chunk_content, f_chunk, hdrflag_len));    
      if( ct = 0 ) then
        myImgInfo.cs := 'DeviceGray';
      elsif( ct = 2 ) then
        myImgInfo.cs := 'DeviceRGB';
      elsif( ct = 3 ) then
        myImgInfo.cs := 'Indexed';
      else
        Error('Alpha channel not supported: ' || pFile);
        end if;
      if( ord(fread(chunk_content, f_chunk, hdrflag_len)) != 0 ) then
        Error('Unknown compression method: ' || pFile);
      end if;
      if( ord(fread(chunk_content, f_chunk, hdrflag_len)) != 0 ) then
        Error('Unknown filter method: ' || pFile);
      end if;
      if( ord(fread(chunk_content, f_chunk, hdrflag_len)) != 0 ) then
        Error('Interlacing not supported: ' || pFile);
      end if;
      if (ct = 2 ) then
        colors := 3;
      else
        colors := 1;
      end if;
      
      myImgInfo.parms := '/DecodeParms <</Predictor 15 /Colors ' || to_char(colors) || ' /BitsPerComponent ' || myImgInfo.bpc || ' /Columns ' || myImgInfo.w || '>>';
          
        elsif(myType = 'PLTE') then
            -- Read palette
            myImgInfo.pal := fread(chunk_content, f_chunk, chunkdata_len ) ;
        elsif(myType = 'tRNS') then
            --   Read transparency info
            buf := fread(chunk_content, f_chunk, chunkdata_len ) ;
            if(ct = 0) then
                myImgInfo.trns(1) := ord(substr(buf,1,1));
            elsif( ct = 2) then
               myImgInfo.trns(1) := ord(substr(buf,1,1));
               myImgInfo.trns(2) := ord(substr(buf,3,1));
               myImgInfo.trns(3) := ord(substr(buf,5,1));
            else
                if(instr(buf,chr(0)) > 0) then
                    myImgInfo.trns(1) := instr(buf,chr(0));
                end if;
            end if;
        elsif(myType = 'IDAT') then
            -- Read image data block after the loop, just mark the begin of data
            dbms_lob.append(myImgInfo.data,chunk_content);
        elsif(myType = 'IEND') then
            exit;
        end if;
    end loop;
    
    if( myImgInfo.cs = 'Indexed' and myImgInfo.pal is null) then
        Error('Missing palette in '|| pFile);
    end if;
  myImgInfo.f := 'FlateDecode';
  dbms_lob.close(chunk_content);
  dbms_lob.close(myImgInfo.data);
  dbms_lob.freetemporary(chunk_content);
  return myImgInfo;
exception
  when others then
    Error('p_parseImage : '||SQLERRM);
    return myImgInfo;
end p_parseImage;


/*******************************************************************************
*                                                                              *
*                               Public methods                                 *
*                                                                              *
********************************************************************************/

----------------------------------------------------------------------------------------
-- Methods added to FPDF primary class
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-- SetDash Ecrire en pointillés
----------------------------------------------------------------------------------------
procedure SetDash(pblack number default 0, pwhite number default 0) is
  s txt;
begin
    if(pblack != 0 or pwhite != 0) then
        s := '['||tochar(pblack*k, 3)||' '||tochar( pwhite*k, 3)||'] 0 d';
    else
        s := '[] 0 d';
	end if;
    p_out(s);
end SetDash;
  
----------------------------------------------------------------------------------------
-- Methods from FPDF primary class
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
procedure Error(pmsg varchar2) is
begin
    if gb_mode_debug then
	  print('<pre>');
	  for i in pdfDoc.first..pdfDoc.last loop
	    if i is not null then
	      print(replace(replace(pdfDoc(i),'>','&gt;'),'<','&lt;'));
		end if;
	  end loop;
	  print('</pre>');
	end if;
	-- Fatal error
	raise_application_error(-20100,'<B>PL_FPDF error: </B>'|| pmsg);
end Error;

----------------------------------------------------------------------------------------
function GetCurrentFontSize return number is
begin
	-- Get fontsizePt
	return fontsizePt;
end GetCurrentFontSize;

----------------------------------------------------------------------------------------
function GetCurrentFontStyle return varchar2 is
begin
	-- Get fontStyle
	return fontStyle;
end GetCurrentFontStyle;

----------------------------------------------------------------------------------------
function GetCurrentFontFamily return varchar2 is
begin
	-- Get fontStyle
	return FontFamily;
end GetCurrentFontFamily;


----------------------------------------------------------------------------------------
procedure Ln(h number default null) is
begin
	-- Line feed; default value is last cell height
	x :=lMargin;
	if(is_string(h)) then
		y:= y + lasth;
	else
		y:= y + h;
    end if; 
end Ln;

----------------------------------------------------------------------------------------
function GetX return number is
begin
	-- Get x position
	return x;
end GetX;

----------------------------------------------------------------------------------------
procedure SetX(px number) is
begin
	-- Set x position
	if(px>=0) then 
		x:=px;
	else
		x:=w+px;
	end if; 
end SetX;

----------------------------------------------------------------------------------------
function GetY return number is
begin
	-- Get y position
	return y;
end GetY;

----------------------------------------------------------------------------------------
procedure SetY(py number) is 
begin
	-- Set y position and reset x
	x:=lMargin;
	if(py>=0) then
		y:=py;
	else
		y:=h+py;
	end if; 
end SetY;

----------------------------------------------------------------------------------------
procedure SetXY(x number,y number) is 
begin
	-- Set x and y positions
	SetY(y);
	SetX(x);
end SetXY;

----------------------------------------------------------------------------------------
-- SetHeaderProc : setting header Callback
----------------------------------------------------------------------------------------
procedure SetHeaderProc(headerprocname in varchar2, paramTable tv4000a default noParam) is
begin
   MyHeader_Proc := headerprocname;
   MyHeader_ProcParam := paramTable;
end;

----------------------------------------------------------------------------------------
-- SetFooterProc : setting footer Callback
----------------------------------------------------------------------------------------
procedure SetFooterProc(footerprocname in varchar2, paramTable tv4000a default noParam) is
begin
   MyFooter_Proc := footerprocname;
   MyFooter_ProcParam := paramTable;
end;

----------------------------------------------------------------------------------------
procedure SetMargins(left number,top number ,right number default -1) is 
myright margin := right;
begin
	-- Set left, top and right margins
	lMargin:=left;
	tMargin:=top;
	if(myright=-1) then
		myright:=left;
	end if; 
	rMargin:=myright;
end SetMargins;

----------------------------------------------------------------------------------------
procedure SetLeftMargin( pMargin number) is 
begin
	-- Set left margin
	lMargin:=pMargin;
	if(page > 0 and  x < pMargin) then
		x:= pMargin;
	end if; 
end SetLeftMargin;

----------------------------------------------------------------------------------------
procedure SetTopMargin(pMargin number) is
begin
	-- Set top margin
	tMargin := pMargin;
end SetTopMargin;

----------------------------------------------------------------------------------------
procedure SetRightMargin(pMargin number) is 
begin
	-- Set right margin
	rMargin := pMargin;
end SetRightMargin;

----------------------------------------------------------------------------------------
procedure SetAutoPageBreak(pauto boolean, pMargin number default 0) is  
begin
	-- Set auto page break mode and triggering margin
	AutoPageBreak := pauto;
	bMargin := pMargin;
	pageBreakTrigger:=h-pMargin;
end SetAutoPageBreak;

----------------------------------------------------------------------------------------
procedure SetDisplayMode(zoom varchar2,layout varchar2 default 'continuous') is
begin
	-- Set display mode in viewer
	if(zoom in ('fullpage', 'fullwidth', 'real', 'default') or not is_string(zoom)) then
		ZoomMode:= zoom;
	else
		Error('Incorrect zoom display mode: ' || zoom);
	end if; 
	if(layout in ('single', 'continuous', 'two', 'default')) then
		LayoutMode := layout;
	else
		Error('Incorrect layout display mode: ' || layout);
	end if; 
end SetDisplayMode;

----------------------------------------------------------------------------------------
procedure SetCompression(p_compress boolean default false) is
begin
	-- Set page compression
	if(function_exists('gzcompress')) then
		b_compress:=p_compress;
	else
		b_compress:=false;
	end if; 
end SetCompression;

----------------------------------------------------------------------------------------
procedure SetTitle(ptitle varchar2) is
begin
	-- Title of document
	title:=ptitle;
end SetTitle;

----------------------------------------------------------------------------------------
procedure SetSubject(psubject varchar2) is
begin
	-- Subject of document
	subject:= psubject;
end SetSubject;

----------------------------------------------------------------------------------------
procedure SetAuthor(pauthor varchar2) is
begin
	-- Author of document
	author:=pauthor;
end SetAuthor;

----------------------------------------------------------------------------------------
procedure SetKeywords(pkeywords varchar2) is
begin
	-- Keywords of document
	keywords:=pkeywords;
end SetKeywords;

----------------------------------------------------------------------------------------
procedure SetCreator(pcreator varchar2) is
begin
	-- Creator of document
	creator:=pcreator;
end SetCreator;

----------------------------------------------------------------------------------------
procedure SetAliasNbPages(palias varchar2 default '{nb}') is
begin
	-- Define an alias for total number of pages
	AliasNbPages:=palias;
end SetAliasNbPages;


----------------------------------------------------------------------------------------
-- buildPlsqlStatment : building the pl/lsq stmt for header or Footer hooked custom proc
--                      Binding parameters and values.
----------------------------------------------------------------------------------------
function buildPlsqlStatment(callbackProc varchar2, tParam tv4000a default noParam) return varchar2 is
    plsqStmt bigtext;
    paramName word;
begin
    if (tParam.first is not null) then
        -- retrieving values of parameters to build plssql statment.
        plsqStmt := 'Begin '||callbackProc||'(';
        paramName := tParam.first;
        while (paramName is not null) loop
            if (paramName != tParam.first) then
                plsqStmt := plsqStmt || ', ';
            end if;
            plsqStmt := plsqStmt || paramName ||'=>'''||
                                    replace(tParam(paramName), '''', '''''')||'''';
            paramName := tParam.next(paramName);
        end loop;
        plsqStmt := plsqStmt||'); end;';
    else
        plsqStmt := 'Begin '||callbackProc||'; end;';
    end if;
    return plsqStmt;
end buildPlsqlStatment;

----------------------------------------------------------------------------------------
-- Header : Procedure that hook the callback procedure for the repetitive header on each page;
----------------------------------------------------------------------------------------
procedure Header is
    plsqStmt bigtext;
begin
	-- MyHeader_Proc defined in Declaration
	if (not empty(MyHeader_Proc)) then  
        -- building plsql stmt.
        plsqStmt := buildPlsqlStatment(MyHeader_Proc, MyHeader_ProcParam);
        -- Executing callback.
        execute immediate plsqStmt;
	end if; 
exception
    when others then
        error('Header : '||sqlerrm||' statment : '||plsqStmt);
end Header;

----------------------------------------------------------------------------------------
-- Footer : Procedure that hook the callback procedure for the repetitive footer on each page;
----------------------------------------------------------------------------------------
procedure Footer is
    plsqStmt bigtext;
begin
	-- MyFooter_Proc defined in Declaration
	if (not empty(MyFooter_Proc)) then  
        -- building plsql stmt.
        plsqStmt := buildPlsqlStatment(MyFooter_Proc, MyFooter_ProcParam);
        -- Executing callback.
	   execute immediate plsqStmt;
	end if; 
exception
    when others then
        error('Footer : '||sqlerrm||' statment : '||plsqStmt);
end Footer;

----------------------------------------------------------------------------------------
function PageNo return number is 
begin
	-- Get current page number
	return page;
end PageNo;

----------------------------------------------------------------------------------------
procedure SetDrawColor(r number,g number default -1,b number default -1) is
begin
	-- Set color for all stroking operations
	if((r=0 and g=0 and b=0) or g=-1)  then 
		DrawColor:=tochar(r/255,3)||' G';
	else
		DrawColor:=tochar(r/255,3) || ' ' || tochar(g/255,3) || ' ' || tochar(b/255,3) || ' RG';
	end if; 
	if(page>0) then
		p_out(DrawColor);
	end if; 
end SetDrawColor;

----------------------------------------------------------------------------------------
procedure SetFillColor (r number,g number default -1,b number default -1) is
begin
	-- Set color for all filling operations
	if((r=0 and g=0 and b=0) or g=-1) then
		FillColor:=tochar(r/255,3) || ' g';
	else
		FillColor:=tochar(r/255,3) ||' '|| tochar(g/255,3) ||' '|| tochar(b/255,3) || ' rg';
	end if;
	if (FillColor!=TextColor) then 
	  ColorFlag:=true;
	else
	  ColorFlag:=false;
	end if; 
	if(page>0) then
		p_out(FillColor);
	end if; 
end SetFillColor;

----------------------------------------------------------------------------------------
procedure SetTextColor (r number,g number default -1,b number default -1) is
begin
	-- Set color for text
	if((r=0 and g=0 and b=0) or g=-1) then
		TextColor:=tochar(r/255,3) || ' g';
	else
		TextColor:=tochar(r/255,3) ||' '|| tochar(g/255,3) ||' '|| tochar(b/255,3) || ' rg';
	end if; 
	if (FillColor!=TextColor) then 
	  ColorFlag:=true;
	else
	  ColorFlag:=false;
	end if; 
end SetTextColor;

----------------------------------------------------------------------------------------
procedure SetLineWidth(width number) is 
begin
	-- Set line width
	LineWidth:=width;
	if(page>0) then 
		p_out(tochar(width*k,2) ||' w');
	end if; 
end SetLineWidth;

----------------------------------------------------------------------------------------
procedure Line(x1 number,y1 number,x2 number,y2 number) is 
begin
	-- Draw a line
	p_out( tochar(x1*k,2) || 
		   ' ' || tochar((h-y1)*k,2) || 
		   ' m ' || tochar(x2*k,2) || 
		   ' ' || tochar((h-y2)*k,2) || ' l S');
end Line;


----------------------------------------------------------------------------------------
procedure Rect(px number,py number,pw number,ph number,pstyle varchar2 default '') is
op word;
begin
	-- Draw a rectangle
	if(pstyle='F') then
		op:='f';
	elsif(pstyle='FD' or pstyle='DF') then
		op:='B';
	else
		op:='S';
	end if; 
	p_out(tochar(px*k,2) || ' ' || tochar((h-py)*k,2) || ' ' || tochar(pw*k,2) || ' ' || tochar(-ph*k,2) || ' re ' || op);
end Rect;

----------------------------------------------------------------------------------------
procedure Triangle(px number, py number, psize number, 
                   porientation varchar default 'left', pstyle varchar2 default '') is
point_1 point;
point_2 point;
point_3 point;
points tab_points;
begin
    --For now just draxw the triangle to the left orientation
    point_1.x := px;
    point_1.y := py;

    point_2.x := px + psize;
    point_2.y := py + psize;
    
    point_3.x := px;
    point_3.y := py + 2 * psize;
    
    points(0) := point_1;
    points(1) := point_2;
    points(2) := point_3;

    Poly(points, true, pstyle);
end;

----------------------------------------------------------------------------------------
procedure Poly(points tab_points, pclose boolean, pstyle varchar2 default '') is
op word;
pdf_cmd varchar2(1000);
begin
	if(pstyle='F') then
		op:='f';
	elsif(pstyle='FD' or pstyle='DF') then
		op:='B';
	else
		op:='S';
	end if;
    
    pdf_cmd := tochar(points(0).x *k, 2) || ' ' || tochar((h - points(0).y) *k, 2) || ' m' || CHR(10);
    
    for i in 1..points.last loop
        pdf_cmd := pdf_cmd || tochar(points(i).x*k, 2) || ' ' || tochar((h - points(i).y) * k, 2) || ' l' || CHR(10);
    end loop;
    
    if(pclose) then
        pdf_cmd := pdf_cmd || ' h' || CHR(10);
    end if;
    
    pdf_cmd := pdf_cmd || ' ' || op || CHR(10);
    
    --htp.p(pdf_cmd);
    p_out(pdf_cmd);
end;

----------------------------------------------------------------------------------------
procedure SetLineDashPattern(pdash varchar2 default '[] 0') is
begin
    p_out(pdash || ' d');
end;

/*
    // Sets line style
    // Parameters:
    // - style: Line style. Array with keys among the following:
    //   . width: Width of the line in user units
    //   . cap: Type of cap to put on the line (butt, round, square). The difference between 'square' and 'butt' is that 'square' projects a flat end past the end of the line.
    //   . join: miter, round or bevel
    //   . dash: Dash pattern. Is 0 (without dash) or array with series of length values, which are the lengths of the on and off dashes.
    //           For example: (2) represents 2 on, 2 off, 2 on , 2 off ...
    //                        (2, 1) is 2 on, 1 off, 2 on, 1 off.. etc
    //   . phase: Modifier of the dash pattern which is used to shift the point at which the pattern starts
    //   . color: Draw color. Array with components (red, green, blue)
    function SetLineStyle($style) {
        extract($style);
        if (isset($width)) {
            $width_prev = $this->LineWidth;
            $this->SetLineWidth($width);
            $this->LineWidth = $width_prev;
        }
        if (isset($cap)) {
            $ca = array('butt' => 0, 'round'=> 1, 'square' => 2);
            if (isset($ca[$cap]))
                $this->_out($ca[$cap] . ' J');
        }
        if (isset($join)) {
            $ja = array('miter' => 0, 'round' => 1, 'bevel' => 2);
            if (isset($ja[$join]))
                $this->_out($ja[$join] . ' j');
        }
        if (isset($dash)) {
            $dash_string = '';
            if ($dash) {
                if(ereg('^.+, ', $dash))
                    $tab = explode(', ', $dash);
                else
                    $tab = array($dash);
                $dash_string = '';
                foreach ($tab as $i => $v) {
                    if ($i > 0)
                        $dash_string .= ' ';
                    $dash_string .= sprintf('%.2f', $v);
                }
            }
            if (!isset($phase) || !$dash)
                $phase = 0;
            $this->_out(sprintf('[%s] %.2f d', $dash_string, $phase));
        }
        if (isset($color)) {
            list($r, $g, $b) = $color;
            $this->SetDrawColor($r, $g, $b);
        }
    }
*/

----------------------------------------------------------------------------------------
function AddLink return number is
nb_link number := links.count + 1;
begin
	-- Create a new internal link
	links(nb_link).zero := 0;
	links(nb_link).un := 0;
	return nb_link;
end AddLink;

----------------------------------------------------------------------------------------
procedure SetLink(plink number,py number default 0,ppage number default -1) is
mypy number := py;
myppage number := ppage;
begin
	-- Set destination of internal link
	if(mypy=-1) then
		mypy:=y;
	end if; 
	if(myppage=-1) then
		myppage:=page;
	end if; 
	links(plink).zero:=myppage;
	links(plink).un:=mypy;
end SetLink;

----------------------------------------------------------------------------------------
procedure Link(px number,py number,pw number,ph number,plink varchar2) is
  v_last_plink integer;
  v_ntoextend integer;
  v_rec rec5;
begin
	-- Put a link on the page
  -- Init PageLinks, if not exists
  begin
     v_last_plink := PageLinks.count;
  exception
     when others then
        PageLinks := linksArray(v_rec);
  end;
  -- extend, so PageLinks(page) exists
  v_last_plink := PageLinks.last;
  v_ntoextend := page-v_last_plink;
  if v_ntoextend > 0 then
     PageLinks.extend(v_ntoextend);
  end if;
  -- set values
	PageLinks(page).zero:=px*k;
	PageLinks(page).un:=hPt-py*k;
	PageLinks(page).deux:=pw*k;
	PageLinks(page).trois:=ph*k;
	PageLinks(page).quatre:=plink;
end Link;


----------------------------------------------------------------------------------------
procedure Text(px number,py number,ptxt varchar2) is
s varchar2(2000);
begin
	-- Output a string
	s:='BT '|| tochar(px*k,2) ||' '|| tochar((h-py)*k,2) ||' Td ('||p_escape(ptxt)||') Tj ET';
	if(underline and ptxt is not null) then
		s := s || ' ' || p_dounderline(px,py,ptxt);
	end if; 
	if(ColorFlag) then
		s := 'q '|| TextColor ||' ' || s || ' Q';
	end if; 
	p_out(s);
end Text;

----------------------------------------------------------------------------------------
function AcceptPageBreak return boolean is
begin
	-- Accept automatic page break or not
	return AutoPageBreak;
end AcceptPageBreak;


----------------------------------------------------------------------------------------
procedure OpenPDF is
begin
	-- Begin document
	state:=1;
end OpenPDF;

----------------------------------------------------------------------------------------
procedure ClosePDF is
begin

	-- Terminate document
	if(state=3) then
		return;
	end if; 
    
	if(page=0) then
		AddPage();
	end if; 
    
	-- Page footer
	InFooter:=true;
	Footer();
	InFooter:=false;
    
	-- Close page
	p_endpage();
    
	-- Close document
	p_enddoc();
    
end ClosePDF;

----------------------------------------------------------------------------------------
procedure AddPage(orientation varchar2 default '') is 
myFamily txt;
myStyle txt;
mySize number := fontsizePt;
lw phrase := LineWidth;
dc phrase := DrawColor;
fc phrase := FillColor;
tc phrase := TextColor;
cf flag := ColorFlag;

begin
	-- Start a new page
	if(state=0) then
		OpenPDF();
	end if;
	myFamily:= FontFamily;
	if (underline) then 
	   myStyle := FontStyle || 'U';
	end if; 
	if(page>0) then
		-- Page footer
		InFooter:=true;
		Footer();
		InFooter:=false;
		-- Close page
		p_endpage();
	end if; 
	-- Start new page
	p_beginpage(orientation);
	-- Set line cap style to square
	p_out('2 J');
	-- Set line width
	LineWidth:=lw;
	p_out(tochar(lw*k)||' w');
	-- Set font
	if(myFamily is not null) then
		SetFont(myFamily,myStyle,mySize);
	end if; 
	-- Set colors
	DrawColor:=dc;
	if(dc!='0 G') then
		p_out(dc);
	end if; 
	FillColor:=fc;
	if(fc!='0 g') then
		p_out(fc);
	end if; 
	TextColor:= tc;
	ColorFlag:= cf;
	-- Page header
	header();
	-- Restore line width
	if(LineWidth!=lw) then
		LineWidth:=lw;
		p_out(tochar(lw*k)||' w');
	end if; 
	-- Restore font

	if myFamily is null then
		SetFont(myFamily,myStyle,mySize);
	end if; 
	-- Restore colors
	if(DrawColor!=dc) then
		DrawColor:=dc;
		p_out(dc);
	end if; 
	if(FillColor!=fc) then 
		FillColor:=fc;
		p_out(fc);
	end if; 
	TextColor:=tc;
	ColorFlag:=cf;
end AddPage;

----------------------------------------------------------------------------------------
procedure update_line_spacing is
begin
	Linespacing := (fontsizePt / k);	-- minimum line spacing in multicell
end;

----------------------------------------------------------------------------------------
procedure fpdf
  (orientation varchar2 default 'P',
   unit varchar2 default 'mm',
   format varchar2 default 'A4') is
   myorientation word := orientation;
   myformat word := format;
   mymargin margin;
begin
	-- Some checks
	p_dochecks();
	-- Initialization of properties
	page:=0;
	n:=2;
	-- Open the final structure for the PDF document.
	pdfDoc(1) := null;
	state:=0;
	InFooter:=false;
	lasth:=0;
	--FontFamily:='';
	FontFamily:='helvetica';
	fontstyle:='';
	fontsizePt:=12;
	underline:=false;
	DrawColor:='0 G';
	FillColor:='0 g';
	TextColor:='0 g';
	ColorFlag:=false;
	ws:=0;

	-- Standard fonts
	CoreFonts('courier') := 'Courier';
	CoreFonts('courierB') := 'Courier-Bold';
	CoreFonts('courierI') := 'Courier-Oblique';
	CoreFonts('courierBI') := 'Courier-BoldOblique';
	CoreFonts('helvetica') := 'Helvetica';
	CoreFonts('helveticaB') := 'Helvetica-Bold';
	CoreFonts('helveticaI') := 'Helvetica-Oblique';
	CoreFonts('helveticaBI') := 'Helvetica-BoldOblique';
	CoreFonts('times') := 'Times-Roman';
	CoreFonts('timesB') := 'Times-Bold';
	CoreFonts('timesI') := 'Times-Italic';
	CoreFonts('timesBI') := 'Times-BoldItalic';
	CoreFonts('symbol') := 'Symbol';
	CoreFonts('zapfdingbats') := 'ZapfDingbats';
	
	-- Scale factor 
	if(unit='pt') then
		k:=1;
	elsif(unit='mm') then
		k:=72/25.4;
	elsif(unit='cm') then
		k:=72/2.54;
	elsif(unit='in') then
		k:=72;
	else
		Error('Incorrect unit: ' || unit);
	end if; 
	
	-- Others added properties
    update_line_spacing;
	
	-- Page format
	if(is_string(myformat)) then
		myformat:=strtolower(myformat);
		if(myformat='a3') then
			formatArray.largeur := 841.89;
			formatArray.hauteur := 1190.55;
		elsif(myformat='a4') then
			formatArray.largeur := 595.28;
			formatArray.hauteur := 841.89;
		elsif(myformat='a5') then
			formatArray.largeur := 420.94;
			formatArray.hauteur := 595.28;
		elsif(myformat='letter') then
			formatArray.largeur := 612;
			formatArray.hauteur := 792;
		elsif(myformat='legal') then
			formatArray.largeur := 612;
			formatArray.hauteur := 1008;
		else
			Error('Unknown page format: '|| myformat);
		end if; 
		fwPt:=formatArray.largeur;
		fhPt:=formatArray.hauteur;
	else
		fwPt:=formatArray.largeur*k;
		fhPt:=formatArray.hauteur*k;
	end if; 
	fw:=fwPt/k;
	fh:=fhPt/k;
	-- Page orientation
	myorientation:=strtolower(myorientation);
	if(myorientation='p' or  myorientation='portrait') then
		DefOrientation:='P';
		wPt:=fwPt;
		hPt:=fhPt;
	elsif(myorientation='l' or myorientation='landscape') then
		DefOrientation:='L';
		wPt:=fhPt;
		hPt:=fwPt;
	else
		Error('Incorrect orientation: ' || myorientation);
	end if; 
	CurOrientation:=DefOrientation;
	w:=wPt/k;
	h:=hPt/k;
	-- Page margins (1 cm) 
	mymargin:=28.35/k;
	SetMargins(mymargin,mymargin);
	-- Interior cell margin (1 mm) 
	cMargin:=mymargin/10;
	-- Line width (0.2 mm)
	LineWidth:=.567/k;
	-- Automatic page break
	SetAutoPageBreak(true,2*mymargin);
	-- Full width display mode
	SetDisplayMode('fullwidth');
	-- Disable compression
	SetCompression(false);
	-- Set default PDF version number
	PDFVersion:='1.3';
end fpdf;

----------------------------------------------------------------------------------------
procedure AddFont (family varchar2, style varchar2 default '',filename varchar2 default '') is
  myfamily word := family;
  mystyle  word := style;
  myfile   word := filename;
  fontkey word;
  fontCount number;
  i pls_integer;
  d pls_integer;
  nb pls_integer;
  myDiff varchar2(2000);
  myType varchar2(256); -- ????????? Cette variable est peut-être globale ????????????
  -- tabNull tv4000;
begin
	-- Add a TrueType or Type1 font
	myfamily:=strtolower(myfamily);
	if myfile is null then
		myfile:=str_replace(' ','',myfamily) || strtolower(mystyle) || '.php';
	end if; 
	if(myfamily='arial') then
		myfamily:='helvetica';
	end if; 
	mystyle:=strtoupper(mystyle);
	if(mystyle='IB')  then
		mystyle:='BI';
	end if; 
	
	fontkey:=myfamily || mystyle;
	if(fonts.exists(fontkey)) then
		Error('Font already added: ' || myfamily || ' ' || mystyle);
	end if; 
    
	p_includeFont(fontkey);
	

	fontCount:=nvl(fonts.count, 0) + 1;

	fonts(fontkey).i := fontCount;
	fonts(fontkey).type := 'core';
	fonts(fontkey).name := coreFonts(fontkey);
	fonts(fontkey).up := -100;
	fonts(fontkey).ut := 50;
	fonts(fontkey).cw := fpdf_charWidths(fontkey);
	fonts(fontkey).file := myfile;
	
	if(myDiff is not null) then
		-- Search existing encodings
		d:=0;
		nb:=diffs.count;
		for i in 1..nb
		loop
			if(diffs(i) = myDiff) then
				d:=i;
				exit;
			end if; 
		end loop;
		if(d=0) then
			d:=nb+1;
			diffs(d):=myDiff;
		end if; 
		fonts(fontkey).diff:=d;
	end if; 

	if(myfile is not null) then
		if(myType = 'TrueType') then
		    FontFiles(myfile).length1 := originalsize;
		else
		    FontFiles(myfile).length1 := size1;
		    FontFiles(myfile).length2 := size2;
		end if; 
	end if; 
end AddFont;

----------------------------------------------------------------------------------------
procedure SetFont(pfamily varchar2,pstyle varchar2 default '',psize number default 0) is
myfamily word := pfamily;
mystyle	 word := pstyle;
mysize	 number := psize;
FontCount number := 0;
myFontFile word;
fontkey  word;
-- tabnull tv4000;
begin
	-- Select a font; size given in points
	myfamily:=strtolower(myfamily);

	if myfamily is null then
		myfamily:=FontFamily;
	end if; 
	
	if(myfamily='arial') then
		myfamily:='helvetica';
	elsif(myfamily='symbol' or  myfamily='zapfdingbats') then
		mystyle:='';
	end if; 
	mystyle:=strtoupper(mystyle);
	
	if(instr(mystyle,'U') > 0) then
		underline:=true;
		mystyle:=str_replace('U','',mystyle);
	else
		underline:=false;
	end if; 
	if(mystyle='IB') then
		mystyle:='BI';
	end if; 
	if(mysize=0) then
		mysize:=fontsizePt;
	end if; 

	-- Test if font is already selected
	if(FontFamily=myfamily and fontstyle=mystyle and fontsizePt=mysize) then
		return;
	end if; 
	
	-- Test if used for the first time	
	fontkey:=nvl(myfamily || mystyle, '');


	--if(not fontsExists(fontkey)) then
	if(not fonts.exists(fontkey)) then
		-- Check if one of the standard fonts
		
		if(CoreFonts.exists(fontkey)) then
			--if(not fpdf_charwidthsExists(fontkey)) then
			if(not fpdf_charwidths.exists(fontkey)) then
				-- Load metric file
				
				myFontFile:=myfamily;
				if(myfamily='times' or myfamily='helvetica') then
					myFontFile:=myFontFile || strtolower(mystyle);
				end if; 
				-- 
				p_includeFont(fontkey);
				-- 
				if(not fpdf_charwidthsExists(fontkey)) then
					Error('Could not include font metric file');
				end if; 
			end if; 
			FontCount:=nvl(fonts.count,0) + 1;
			fonts(fontkey).i := FontCount;
	 		fonts(fontkey).type := 'core';
			fonts(fontkey).name := CoreFonts(fontkey); 
			fonts(fontkey).up  := -100;  
			fonts(fontkey).ut := 50;   
			fonts(fontkey).cw  := fpdf_charwidths(fontkey);  
		else
			Error('Undefined font: ' || myfamily || ' ' || mystyle);
		end if; 
	end if; 
	-- Select it
	FontFamily:=myfamily;
	fontstyle:=mystyle;
	fontsizePt:=mysize;
	fontsize:=mysize/k;
	-- if(fontsExists(fontkey)) then
	    CurrentFont:= fonts(fontkey);
	-- end if;
	if(page>0) then
		p_out('BT /F'||CurrentFont.i||' '||tochar(fontsizePt,2)||' Tf ET');
	end if; 
    
    --We've change the font size so we need to update line spacing
    update_line_spacing;
end SetFont;


----------------------------------------------------------------------------------------
function GetStringWidth(pstr varchar2) return number is
charSetWidth CharSet;
w number;
lg number;
wdth number;
c car;
begin
	-- Get width of a string in the current font
	charSetWidth := CurrentFont.cw;
	w:=0;
	lg := strlen(pstr);
	for i in 1..lg
	loop
	    wdth := 0;
		c := substr(pstr,i,1);
	    --if (charSetWidth.exists(c)) then
		  wdth := charSetWidth(c);
		--end if;
		w:= w + wdth;
	end loop;
	return w * fontsize/1000;
end GetStringWidth;


----------------------------------------------------------------------------------------
procedure SetFontSize(psize number) is
begin
	-- Set font size in points
	if(fontsizePt=psize) then
		return;
	end if; 
	fontsizePt:=psize;
	fontsize:=psize/k;
	if(page>0) then
		p_out('BT /F'||CurrentFont.i||' '||tochar(fontsizePt,2)||' Tf ET');
	end if; 
end SetFontSize;

----------------------------------------------------------------------------------------
procedure Cell
		 (pw number,
		  ph number default 0,
		  ptxt varchar2 default '',
		  pborder varchar2 default '0',
		  pln number default 0,
		  palign varchar2 default '',
		  pfill number default 0,
		  plink varchar2 default '') is
 myPW number := pw;
 myK k%type := k;
 myX x%type := x;
 myY y%type := y;
 myWS ws%type := ws;
 myS txt;
 myOP txt;
 myDX number;
 myTXT2 txt;
begin
  null;
	-- Output a cell 
	if( ( y + ph > pageBreakTrigger) and  not InFooter and AcceptPageBreak()) then
		-- Automatic page break
		if(myWS > 0) then
			ws:=0;
			p_out('0 Tw');
		end if; 
		AddPage(CurOrientation);
		x:=myX;
		if(myWS > 0) then
			ws := myWS;
			p_out(tochar(myWS * myK,3) ||' Tw');
		end if; 
	end if; 

	if(myPW = 0) then
		myPW := w - rMargin - x;
	end if; 
	myS := '';
	if(pfill = 1 or pborder = '1') then
		if(pfill = 1) then 
		  if (pborder = '1') then 
		    myOP :=  'B';
		  else
		    myOP := 'f';
		  end if; 
		else
			myOP := 'S';
		end if; 
		myS := tochar(x*myK,2)||' '||tochar((h-y)*myK,2)||' '||tochar(myPW*myK,2)||' '||tochar(-ph*myK,2)||' re '||myOP||' ';
	end if; 
	
	if(is_string(pborder)) then
		myX := x;
		myY := y;
		if(instr(pborder,'L') > 0) then
			myS := myS || tochar(myX*myK,2) ||' '||tochar((h-myY)*myK,2)||' m '||tochar(myX*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' l S ';
		end if; 
		if(instr(pborder,'T') > 0) then
			myS := myS || tochar(myX*myK,2)||' '||tochar((h-myY)*myK,2)||' m '||tochar((myX+myPW)*myK,2)||' '||tochar((h-myY)*myK,2)||' l S ';
		end if; 
		if(instr(pborder,'R') > 0) then
			myS := myS || tochar((myX+myPW)*myK,2)||' '||tochar((h-myY)*myK,2)||' m '||tochar((myX+myPW)*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' l S ';
		end if; 
		if(instr(pborder,'B') > 0) then
			myS := myS || tochar(myX*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' m '||tochar((myX+myPW)*myK,2)||' '||tochar((h-(myY+ph))*myK,2)||' l S ';
		end if; 
	end if; 
	if ptxt is not null then
		if(palign='R') then
			myDX := myPW - cMargin - GetStringWidth(ptxt);
		elsif(palign='C') then
			myDX := (myPW - GetStringWidth(ptxt))/2;
		else
			myDX := cMargin;
		end if; 
		if(ColorFlag) then
			myS := myS || 'q ' || TextColor || ' ';
	    end if; 
		
        myTXT2 := str_replace('\','\\',ptxt);
        myTXT2 := str_replace(')','\)',myTXT2);
        myTXT2 := str_replace('(','\(',myTXT2);        
    myS := myS || 'BT '||tochar((x+myDX)*myK,2)||' '||tochar((h-(y+.5*ph+.3*fontsize))*myK,2)||' Td ('||myTXT2||') Tj ET';
		if(underline) then
			myS := myS || ' ' || p_dounderline(x+myDX,y+.5*ph+.3*fontsize,ptxt);
		end if; 
		if(ColorFlag) then
			myS := myS || ' Q';
		end if; 
		if(not empty(plink)) then
			Link(x + myDX,y + .5*ph - .5*fontsize, GetStringWidth(ptxt), fontsize, plink);
	    end if; 
	end if; 
	if(not empty(myS)) then
		p_out(myS);
	end if; 

	lasth := ph;
	if( pln>0 ) then
		-- Go to next line
		y := y + ph;
		if(pln=1) then
			x := lMargin;
		end if; 
	else
		x := x + myPW;
	end if; 
exception 
  when others then
   error('Cell : '||sqlerrm);
end Cell;
    
----------------------------------------------------------------------------------------
-- MultiCell : Output text with automatic or explicit line breaks
-- param phMax : give the max height for the multicell. (0 if non applicable)
-- if ph is null : the minimum height is the value of the property LineSpacing
----------------------------------------------------------------------------------------
function MultiCell
  ( pw number,
    ph number default 0,
	ptxt varchar2,
	pborder varchar2 default '0',
	palign varchar2 default 'J',
	pfill number default 0,
	phMax number default 0) return number is
	
  charSetWidth CharSet;
  myPW number := pw;
  myBorder word := pborder;
  myS txt;
  myNB number;
  wmax number;
  myB txt;
  myB2 txt;
  sep number := -1;
--  i number := 0;
--  j number := 0;
  i number := 1;
  j number := 1;
  l number := 0;
  ns number := 0;
  nl number := 1;
  carac word;
  lb_skip boolean := false;
  ls number;
  cumulativeHeight number := 0;
  myH number := pH;
begin
	-- Output text with automatic or explicit line breaks
	
	-- see if we need to set Height to the minimum linespace
	if (myH = 0) then
	  myH := getLineSpacing;
	end if;
	
	charSetWidth := CurrentFont.cw;
	if(myPW = 0) then
		myPW:=w - rMargin - x;
	end if; 
	wmax := (myPW - 2 * cMargin) * 1000 / fontsize;
	myS := str_replace(CHR(13),'',ptxt);
	myNB := strlen(myS);
	if(myNB > 0 and substr(myS,-1) = CHR(10) ) then
		myNB := myNB - 1;
	end if; 
	myB := 0;

	if (myBorder is not null) then
		if(myBorder = '1') then
			myBorder :='LTRB';
			myB := 'LRT';
			myB2 := 'LR';
		else
			myB2 := '';
			if(instr(myBorder,'L') > 0) then
				myB2 := myB2 || 'L';
			end if; 
			if(instr(myBorder,'R') > 0) then
				myB2 := myB2 || 'R';
			end if; 
			if (instr(myBorder,'T') > 0) then 
			  myB := myB2 || 'T';
			else
			  myB := myB2;
			end if; 
		end if; 
	end if; 

	while(i <= myNB)
	loop
	    lb_skip := false;
		-- Get next character
		carac := substr(myS,i,1);
		if(carac = CHR(10)) then
			-- Explicit line break
			if(ws > 0) then
				ws := 0;
				p_out('0 Tw');
			end if; 
			Cell(myPW,myH,substr(myS,j,i-j),myB,2,palign,pfill);
			cumulativeHeight := cumulativeHeight + myH;
			i := i + 1;
			sep := -1;
			j := i;
			l := 0;
			ns := 0;
			nl := nl + 1;
			if(myBorder is not null and nl = 2) then
				myB := myB2;
			end if; 
			-- si on passe là on continue à la prochaine itération de la boucle 
			-- en PHP il y avait l'instruction "continue" .
			lb_skip := true;
		end if; 
		
		if (not lb_skip) then 
			if(carac =' ') then
				sep := i;
				ls := l;
				ns := ns + 1;
			end if; 
			l := l + charSetWidth (carac);
			if( l > wmax) then
				-- Automatic line break
				if(sep=-1) then
					if(i=j) then
						i := i + 1;
					end if; 
					if(ws > 0) then
						ws := 0;
						p_out('0 Tw');
					end if; 

                    Cell(myPW,myH,substr(myS,j,i-j),myB,2,palign,pfill);
				else
					if(palign = 'J') then
					    if (ns > 1) then 
						  ws := (wmax - ls)/1000*fontsize/(ns-1);
						else
						  ws := 0;
						end if; 
						p_out(''|| tochar(ws*k,3) ||' Tw');
					end if; 
                    
                    Cell(myPW,myH,substr(myS,j,sep-j),myB,2,palign,pfill);
					i := sep + 1;
				end if; 
				cumulativeHeight := cumulativeHeight + myH;
				sep := -1;
				j := i;
				l := 0;
				ns := 0;
				nl := nl + 1;
				if(myBorder is not null and nl = 2) then
					myB := myB2;
				end if; 
			else
			  i := i + 1;
			end if; 
		end if; 
	end loop;

	-- Last chunk
	if(ws > 0) then
		ws := 0;
		p_out('0 Tw');
	end if; 
	
	if(myBorder is not null and instr(myBorder,'B') > 0) then
	  if (phMax > 0) then
	    if (cumulativeHeight >= phMax) then
		  myB := myB || 'B';
		end if;
	  else
	    myB := myB || 'B';
	  end if;
	end if; 
	Cell(myPW,myH,substr(myS,j,i-j),myB,2,palign,pfill);
	cumulativeHeight := cumulativeHeight + myH;
	
    -- add an empty cell if phMax is not reached.
	if (phMax > 0) then
	    if ( cumulativeHeight < phMax ) then
		    -- dealing with the bottom border.
			if(myBorder is not null and instr(myBorder,'B') > 0) then
				myB := myB || 'B';
			end if; 
	        Cell(myPW,phMax-cumulativeHeight,null,myB,2,palign,pfill);
	    end if;
	end if;

	x := lMargin;
    
    return nl;

exception 
  when others then
   error('MultiCell : '||sqlerrm);
end MultiCell;

----------------------------------------------------------------------------------------
-- MultiCell : Output text with automatic or explicit line breaks
-- param phMax : give the max height for the multicell. (0 if non applicable)
-- if ph is null : the minimum height is the value of the property LineSpacing
----------------------------------------------------------------------------------------
procedure MultiCell
  ( pwidth number,
    pheight number default 0,
    ptext varchar2,
    pbrdr varchar2 default '0',
    palignment varchar2 default 'J',
    pfillin number default 0,
    phMaximum number default 0) is
    
   ln_ignore number;
begin
    ln_ignore := MultiCell  ( pw => pwidth, ph => pheight, ptxt => ptext, pborder => pbrdr,
                              palign => palignment, pfill => pfillin, phMax => phMaximum);
end multicell;



----------------------------------------------------------------------------------------
procedure image ( pFile varchar2, 
		  		  pX number, 
				  pY number, 
				  pWidth number default 0,
				  pHeight number default 0,
				  pType varchar2 default null,
				  pLink varchar2 default null) is
				  
   myFile varchar2(2000) := pFile;
   -- myType varchar2(256) := pType;
   myW number := pWidth;
   myH number := pHeight;
   -- pos number;
   info recImage;
begin
    --Put an image on the page
	if ( not imageExists(myFile) ) then
		--First use of image, get info
		info := p_parseImage(myFile);
		info.i := nvl(images.count, 0) + 1;
		images(lower(myFile)) := info;
	else
		info := images(lower(myFile));
	end if;
	--Automatic width and height calculation if needed
	if(myW = 0 and myH = 0) then
		--Put image at 72 dpi
		myW := info.w / k;
		myH := info.h / k;
	end if;
	if (myW = 0) then
		myW := myH * info.w / info.h;
    end if;
	if (myH = 0) then
		myH := myW * info.h / info.w;
	end if;
	p_out('q '||tochar(myW * k, 2)||' 0 0 '||tochar(myH * k, 2)||' '||tochar(pX * k, 2)||' '||tochar((h - ( pY + myH)) * k, 2)||' cm /I'||to_char(info.i)||' Do Q');
	if(pLink is not null) then
		Link(pX,pY,myW,myH,pLink);
	end if;
exception 
  when others then
   error('image : '||sqlerrm);
end image;

/* THIS PROCEDURE HANGS UP ........... */
----------------------------------------------------------------------------------------
procedure Write(pH varchar2,ptxt varchar2,plink varchar2 default null) is
   charSetWidth CharSet;
   myW number;     -- remaining width from actual position in user units
   myWmax number;  -- remaining cellspace
   s bigtext;
   c word;
   nb pls_integer;
   sep pls_integer;
   i pls_integer;
   j pls_integer;
   l pls_integer;
   lsep pls_integer;
   lastl pls_integer;
begin
	-- Output text in flowing mode
	charSetWidth := CurrentFont.cw;
	myW := w - rMargin - x;
	myWmax := (myW - 2 * cMargin) * 1000 / FontSize;
	s := str_replace(chr(13),'',ptxt);
	nb := strlen(s);
	sep := -1;   -- no blank space encountered, position of last blank
  i := 1;      -- running position
  j := 1;      -- last remembered position , start for next output
	l := 0;      -- string length since last written
  lsep := 0;   -- position of last blank
  lastl := 0;  -- length till that blank
  -- Loop over all characters
	while i <= nb  loop
		-- Get next character
		c := substr(s, i, 1);
    
    -- Explicit line break
		if(c = chr(10)) then
			Cell(myW, pH, substr(s,j,i-j), 0, 1, '', 0, plink);   
      -- positioned at beginning of new line
			i := i + 1;
			sep := -1;
			j := i;
			l := 0;
      myW := w - rMargin - x;
			myWmax := (myW - 2 * cMargin) * 1000 / FontSize;  -- whole line
		
    else 
			if c = ' ' then
				 sep := i;
         lsep := 0;
         lastl := l;
      else
         lsep := lsep + charSetWidth(c);
			end if; 
			l := l + charSetWidth(c);
			if l > myWmax then
				-- Automatic line break
				if sep = -1 then  -- forced
          Cell(myW, pH, substr(s,j,i-j+1), 0, 1, '', 0, plink);
					i := i + 1;
          j := i;
          l := 0;
				else  -- wrap at last blank
					Cell(myW, pH, substr(s,j,sep-j), 0, 1, '', 0, plink);
					i := sep + 1;
          j := i;
          sep := -1;
          l := lsep-(myWmax-lastl);  -- rest remaining space from previous line
                                     -- WHY ????   
				end if;
        myW := w - rMargin - x;
				myWmax := (myW - 2 * cMargin) * 1000 / FontSize;
			else
				i := i + 1;
			end if;
		end if;
	end loop;
	-- Last chunk
	if( i != j ) then
		 Cell((l+2*cMargin) / 1000 * FontSize, pH, substr(s,j), 0, 0, '', 0, plink);
  end if;
exception 
   when others then
      error('write : '||sqlerrm);
end write;

----------------------------------------------------------------------------------------
procedure Output(pname varchar2 default null,pdest varchar2 default null) is
   myName word := pname;
   myDest word := pdest;
   v_doc blob;      -- finally complete document
   v_blob blob;
   v_clob clob;
   v_in pls_integer;
   v_out pls_integer;
   v_lang pls_integer;
   v_warning pls_integer;
   v_len pls_integer;
begin
   dbms_lob.createtemporary(v_blob, false, dbms_lob.session);
   dbms_lob.createtemporary(v_doc, false, dbms_lob.session);
	 -- Output PDF to some destination
	 -- Finish document if necessary
     
	 if state < 3 then
		  ClosePDF();
	 end if; 
     
	 myDest := strtoupper(myDest);
	 if(myDest is null) then
		  if(myName is null) then
			   myName := 'doc.pdf';
			   myDest := 'I';
		  else
			   myDest := 'D';
		  end if; 
	 end if; 
		
	 if (myDest = 'I') then 
      -- Send as pdf to a browser
      OWA_UTIL.MIME_HEADER('application/pdf',false);
      htp.print('Content-Length: ' || getPDFDocLength());
      htp.print('Content-disposition: inline; filename="' || myName || '"');
      owa_util.http_header_close;

			-- restitution du contenu...
      v_len := 1;
      
      for i in pdfDoc.first..pdfDoc.last loop
         v_clob := to_clob(pdfDoc(i));
         if v_clob is not null then
            v_in := 1;
            v_out := 1;
            v_lang := 0;
            v_warning := 0;
            v_len := dbms_lob.getlength(v_clob); 
            dbms_lob.convertToBlob(v_blob, v_clob, v_len,
               v_in, v_out, dbms_lob.default_csid, v_lang, v_warning);
            dbms_lob.append(v_doc, dbms_lob.substr(v_blob, v_len));   
         end if;
      end loop;
      wpg_docload.download_file(v_doc);
       
	 elsif (myDest = 'D') then
      
			-- Download file
			if(not empty(owa_util.get_cgi_env('HTTP_USER_AGENT')) and instr(owa_util.get_cgi_env('HTTP_USER_AGENT'),'MSIE') > 0) then
				OWA_UTIL.MIME_HEADER('application/force-download',false);
			else
				OWA_UTIL.MIME_HEADER('application/octet-stream',false);
			end if; 
			htp.print('Content-Length: ' || getPDFDocLength());
			htp.print('Content-disposition: attachment; filename="' || myName || '"');
			owa_util.http_header_close;

			-- restitution du contenu...
				for i in pdfDoc.first..pdfDoc.last loop
				  htp.prn(pdfDoc(i));
				end loop;
								
	 elsif (myDest = 'S') then 
     
		    --OWA_UTIL.MIME_HEADER('application/pdf');
			OWA_UTIL.MIME_HEADER('text/html');
			-- Return as a string
			for i in pdfDoc.first..pdfDoc.last loop
			  htp.prn(replace(replace(replace(pdfDoc(i),'<', '&lt;'),'>','&gt;'),chr(10),'<br/>'));
			end loop;
		else
			Error('Incorrect output destination: ' || myDest);
		end if; 
exception 
   when others then
      error('Output : '||sqlerrm);
end Output;
 
 
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------
-- Test procedures, and samples code.
----------------------------------------------------------------------------------------
procedure helloworld is
begin
    FPDF('P','cm','A4');
    openpdf;
    AddPage();
    SetFont('Arial','B',16);
    Cell(0,1.2,'Hello World',0,1,'C');
    Output();
end helloworld;

----------------------------------------------------------------------------------------
-- testImg :  Testing with an image.
----------------------------------------------------------------------------------------
procedure testImg is
 img varchar2(2000);
begin
    FPDF('P','cm','A4');
    openpdf;
    AddPage();
    SetFont('Arial','B',16);
    img := 'http://www.laclasse.com/v2/images/picto_laclassev2.png';
    Image(img,1, 1, 10);
    Output();
end testImg;

----------------------------------------------------------------------------------------
-- test :  Generic Testing proc. Put here what you want !
-------------------------------------------- --------------------------------------------
procedure test(pdest varchar2 default 'D') is
  myImgInfo recImage;
  img varchar2(2000);
  someTxt bigtext;
begin
	FPDF('P','cm','A4');
	openpdf;
	--setMargins(2,2,-2);
	AddPage();
	SetFont('Arial','B',16);
	SetTextColor(132,0,132);
	
/* TEST HELLOWORD */
	Cell(0,1.2,'(Helloworld avec des parenthèses !)',0,1,'C');

   
/*
 -- Test image
   print('Test de chargement d''une image à partir d''une url...<br>');
   --img := '/v2/images/b2i/fonds/fd_livret_college.png';
   img := '/v2/images/picto_laclassev2.png';
   --img := '/v2/images/picto_calendar.jpg';
   
   print('<img src="'||img||'"/><br>');
   myImgInfo := p_parseImage(img);
   print('i='||myImgInfo.i||'<br>');
   print('w='||myImgInfo.w||'<br>');
   print('h='||myImgInfo.h||'<br>');
   print('bpc='||myImgInfo.bpc||'<br>');
   print('lg='||dbms_lob.getlength( myImgInfo.data) ||' octets<br>');
   print('f='||myImgInfo.f||'<br>');
   print('cs='||myImgInfo.cs||'<br>');
   print('parms='||myImgInfo.parms||'<br>');
   print('pal='||myImgInfo.pal||'<br>');
   /*
   for i in myImgInfo.trns.first..myImgInfo.trns.last loop
     if i is not null then
   	   print('trns='||myImgInfo.trns(i)||'<br>');
	 end if;
   end loop;
  */
 

 -- TEST Image

 img := '/v2/images/b2i/fonds/fd_livret_college.png';
 --img := '/v2/images/picto_laclassev2.png';
 
 Image(img,1, 1, 10);
 Cell(0,1.2,'Validation des compétences du B2i',0,1,'C');
 
/*
  AddPage();
  Cell(0,1.2,'Validation des compétences du B2i page 2',0,1,'C');
  
  /*
  someTxt := 'some Texte some Texte some Texte some Texte some Texte some Texte some Texte some Texte ';
  someTxt := someTxt || someTxt; 
  --someTxt := someTxt || someTxt; 

  -- TEST DE Text
    SetFont('Arial','',12);
	text(10, 20, someTxt);
   */

	Output(pdest=>pdest);
end test;


----------------------------------------------------------------------------------------
-- MyRepetitiveHeader :  Proc that illustrates Header hooks.
-- The Header hook procedure has to be PUBLIC. (Spec declaration needeed).
----------------------------------------------------------------------------------------
procedure MyRepetitiveHeader(param1 varchar2, param2 varchar2) is
begin
    SetFont('Arial','I',9);
    cell(0,0,'Repetitive Header param1='||param1||', param2='||param2,0,1,'C');
    line(1,1.2,20,1.2);
end MyRepetitiveHeader;

----------------------------------------------------------------------------------------
-- MyRepetitiveFooter :  Proc that illustrates Footer hooks.
-- The Footer hook procedure has to be PUBLIC. (Spec declaration needeed).
----------------------------------------------------------------------------------------
procedure MyRepetitiveFooter is
begin
    SetFont('Arial',null,9);
    line(1,28,20,28);
    write(52,'Repetitive Footer');
end MyRepetitiveFooter;

--------------------------------------------------------------------------------
-- Affiche le numéro de page en base de page
--------------------------------------------------------------------------------
procedure lpc_footer is
begin
    if(PageNo != 1) then
        SetTextColor(88, 90, 90);
        -- Le numéro de la page courante est en gras
        SetFont('helvetica','B',12);
        Text(18, 29.3, PageNo);
        -- Le reste normal
        SetFont('helvetica','',12);
        Text(18.5, 29.3, '/ 25');
    end if;
end;


----------------------------------------------------------------------------------------
-- testHeader :  Proc that illustrates how to call header and footer hooks.
----------------------------------------------------------------------------------------
procedure testHeader is
 img varchar2(2000);
 tHdr tv4000a; -- This is a table for the custom header proc hooked
begin
    -- setting parameter values for proc 'MyRepetitiveHeader'
    tHdr('param1') := 'Value for Param1';
    tHdr('param2') := '123456';
    
    setHeaderProc('pl_fpdf.MyRepetitiveHeader', tHdr);
    setFooterProc('pl_fpdf.MyRepetitiveFooter');
    FPDF('P','cm','A4');
    openpdf;
    -- first page
    AddPage();   
    SetFont('Arial','B',16);
    Cell(0,1.2,'This is the first Page (left alignment)',0,1,'L');
    -- Second page
    AddPage();
    SetFont('Arial','B',16);
    Cell(0,1.2,'This is the second Page (right alignment)',0,1,'R');
    -- third page
    AddPage();
    SetFont('Arial','B',16);
    Cell(0,1.2,'This is the second Page (centered)',0,1,'C');
    Output();
end testHeader;


END PL_FPDF;
/
