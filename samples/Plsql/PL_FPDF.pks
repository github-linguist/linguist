CREATE OR REPLACE PACKAGE PL_FPDF AS
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
-- Public types and subtypes.
subtype word is varchar2(80);

type tv4000a is table of varchar2(4000) index by word;

--Point type use in polygons creation
type point is record (x number, y number);

type tab_points is table of point index by binary_integer;

-- Constantes globales
FPDF_VERSION constant varchar2(10) := '1.53'; 
PL_FPDF_VERSION constant varchar2(10) := '0.9.3'; 
noParam tv4000a;

-- methods added to FPDF
function GetCurrentFontSize return number;
function GetCurrentFontStyle return varchar2;
function GetCurrentFontFamily return varchar2;
procedure SetDash(pblack number default 0, pwhite number default 0);
function GetLineSpacing return number;
Procedure SetLineSpacing (pls number);
-- Allows to create a polygon with a table of points
-- pclose define if the polygon is closed or not
procedure Poly(points tab_points, pclose boolean, pstyle varchar2 default '');
procedure Triangle(px number, py number, psize number, 
                   porientation varchar default 'left', pstyle varchar2 default '');
                   
procedure SetLineDashPattern(pdash varchar2 default '[] 0');

-- FPDF public methods
procedure Ln(h number default null);
function  GetX return number;
procedure SetX(px number);
function  GetY return number;
procedure SetY(py number);
procedure SetXY(x number,y number);
procedure SetHeaderProc(headerprocname in varchar2, paramTable tv4000a default noParam);
procedure SetFooterProc(footerprocname in varchar2, paramTable tv4000a default noParam);
procedure SetMargins(left number,top number ,right number default -1);
procedure SetLeftMargin( pMargin number);
procedure SetTopMargin(pMargin number);
procedure SetRightMargin(pMargin number);
procedure SetAutoPageBreak(pauto boolean,pMargin number default 0);
procedure SetDisplayMode(zoom varchar2,layout varchar2 default 'continuous');
procedure SetCompression(p_compress boolean default false);
procedure SetTitle(ptitle varchar2);
procedure SetSubject(psubject varchar2);
procedure SetAuthor(pauthor varchar2);
procedure SetKeywords(pkeywords varchar2);
procedure SetCreator(pcreator varchar2);
procedure SetAliasNbPages(palias varchar2 default '{nb}');
procedure Header;
procedure Footer;
function  PageNo return number;
procedure SetDrawColor(r number,g number default -1,b number default -1);
procedure SetFillColor (r number,g number default -1,b number default -1);
procedure SetTextColor (r number,g number default -1,b number default -1);
procedure SetLineWidth(width number);
procedure Line(x1 number,y1 number,x2 number,y2 number);
procedure Rect(px number,py number,pw number,ph number,pstyle varchar2 default '');
function  AddLink return number;
procedure SetLink(plink number,py number default 0,ppage number default -1);
procedure Link(px number,py number,pw number,ph number,plink varchar2);
procedure Text(px number,py number,ptxt varchar2);
function  AcceptPageBreak return boolean;
procedure AddFont (family varchar2, style varchar2 default '',filename varchar2 default '');
procedure SetFont(pfamily varchar2,pstyle varchar2 default '',psize number default 0);
function GetStringWidth(pstr varchar2) return number;
procedure SetFontSize(psize number);
procedure Cell
		 (pw number,
		  ph number default 0,
		  ptxt varchar2 default '',
		  pborder varchar2 default '0',
		  pln number default 0,
		  palign varchar2 default '',
		  pfill number default 0,
		  plink varchar2 default '');
--Now return the number of line created with multiCell
function MultiCell
  ( pw number,
    ph number default 0,
	ptxt varchar2,
	pborder varchar2 default '0',
	palign varchar2 default 'J',
	pfill number default 0,
	phMax number default 0) return number;

procedure MultiCell
  ( pwidth number,
    pheight number default 0,
    ptext varchar2,
    pbrdr varchar2 default '0',
    palignment varchar2 default 'J',
    pfillin number default 0,
    phMaximum number default 0);
    
procedure Write(pH varchar2,ptxt varchar2,plink varchar2 default null);
procedure image ( pFile varchar2, 
		  		pX number, 
				  pY number, 
				  pWidth number default 0,
				  pHeight number default 0,
				  pType varchar2 default null,
				  pLink varchar2 default null);
				  
procedure Output(pname varchar2 default null,pdest varchar2 default null);

procedure OpenPDF;
procedure ClosePDF;
procedure AddPage(orientation varchar2 default '');
procedure fpdf  (orientation varchar2 default 'P', unit varchar2 default 'mm', format varchar2 default 'A4');
procedure Error(pmsg varchar2);
procedure DebugEnabled;
procedure DebugDisabled;
function GetScaleFactor return number;
function getImageFromUrl(p_Url varchar2) return ordsys.ordImage;

--
-- Sample codes.
--
procedure helloworld;
procedure testImg;
procedure test(pdest varchar2 default 'D');
procedure MyRepetitiveHeader(param1 varchar2, param2 varchar2);
procedure MyRepetitiveFooter;
procedure testHeader;
--------------------------------------------------------------------------------
-- Affiche le numéro de page en base de page
--------------------------------------------------------------------------------
procedure lpc_footer;

END PL_FPDF;
/
