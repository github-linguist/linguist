%% blockdraw.mp
%% Copyright 2006 Henrik Tidefelt
% 
% This work may be distributed and/or modified under the 
% conditions of the LaTeX Project Public License, either version 1.3 
% of this license or (at your option) any later version.
% The latest version of this license is in 
% http://www.latex-project.org/lppl.txt 
% and version 1.3 or later is part of all distributions of LaTeX 
% version 2005/12/01 or later.
% 
% This work has the LPPL maintenance status `maintained´.
% 
% The Current Maintainer of this work is Henrik Tidefelt,
% tidefelt@isy.liu.se.
% 
% This work, referred to as blockdraw.mp, consists of the files
%   shiftoff.mp
%   blockdraw.mp
%   bondgraph.mp
%   blockdraw_mp.pdf

numeric longblockrx;
numeric longblockry;
numeric smallblockr;
longblockrx := 0.7cm;
longblockry := 0.4cm;
smallblockr := 0.3cm;

numeric connectionlw;
numeric blocklw;
connectionlw := 0.8pt;
blocklw = connectionlw;

numeric textscale;
textscale := 1;

boolean implicitdraw;
implicitdraw := true;

input shiftoff;

def sizedblock(expr txt, z, rx, ry ) =
  begingroup
    save tmppic;
    picture tmppic;
    tmppic := nullpicture;
    addto tmppic also shiftoff( txt scaled textscale, to_center );
    addto tmppic doublepath (-rx,-ry)--(-rx,ry)--(rx,ry)--(rx,-ry)--cycle withpen pencircle scaled blocklw;
    tmppic := tmppic shifted z;
    if implicitdraw:
      draw tmppic;
    fi
    tmppic
  endgroup
enddef;
def longblock(expr txt, z ) =
  sizedblock( txt, z, longblockrx, longblockry )
enddef;
def squareblock(expr txt, z ) =
  sizedblock( txt, z, smallblockr, smallblockr )
enddef;
def roundblock(expr txt, z ) =
  begingroup
    save tmppic;
    picture tmppic;
    tmppic := nullpicture;
    addto tmppic also shiftoff( txt scaled textscale, to_center );
    addto tmppic doublepath (smallblockr,0)...(0,smallblockr)...(-smallblockr,0)...(0,-smallblockr)...cycle withpen pencircle scaled blocklw;
    tmppic := tmppic shifted z;
    if implicitdraw:
      draw tmppic;
    fi
    tmppic
  endgroup
enddef;
def splitdot(expr z ) =
  begingroup
    save tmppic;
    picture tmppic;
    tmppic := nullpicture;
    addto tmppic contour fullcircle scaled 2.5connectionlw shifted z;
    if implicitdraw:
      draw tmppic;
    fi
    tmppic
  endgroup
enddef;
def termcircle(expr z ) =
  begingroup
    save tmppic;
    picture tmppic;
    tmppic := nullpicture;
    addto tmppic doublepath fullcircle scaled 7connectionlw shifted z;
    if implicitdraw:
      draw tmppic;
    fi
    tmppic
  endgroup
enddef;
def pointpicture(expr z ) =
  begingroup
    save tmppic;
    picture tmppic;
    tmppic := nullpicture;
    addto tmppic contour fullcircle scaled 0 shifted z;
    tmppic
  endgroup
enddef;

def mspoint( expr pth, mediation, slide ) =
(point (arctime mediation * arclength(pth) + slide of pth) of pth)
enddef;
def hhconnect(expr pa, pb, mediation, slide ) =
  begingroup
    save mid, respath;
    numeric mid;
    path respath;
    mid := (mediation)[ xpart pa, xpart pb ] + (if (xpart pb) > (xpart pa): 1 else: -1 fi) * slide;
    respath := pa--( mid, ypart pa )--( mid, ypart pb )--pb;
    if implicitdraw:
      drawarrow respath withpen pencircle scaled connectionlw;
    fi
    respath
  endgroup
enddef;
def vvconnect(expr pa, pb, mediation, slide ) =
  begingroup
    save mid, respath;
    numeric mid;
    path respath;
    mid := (mediation)[ ypart pa, ypart pb ] + (if (ypart pb) > (ypart pa): 1 else: -1 fi) * slide;
    respath := pa--( xpart pa, mid )--( xpart pb, mid )--pb;
    if implicitdraw:
      drawarrow respath withpen pencircle scaled connectionlw;
    fi
    respath
  endgroup
enddef;
def hvconnect(expr pa, pb ) =
  begingroup
    save respath;
    path respath;
    respath := pa--( xpart pb, ypart pa )--pb;
    if implicitdraw:
      drawarrow respath withpen pencircle scaled connectionlw;
    fi
    respath
  endgroup
enddef;
def vhconnect(expr pa, pb ) =
  begingroup
    save respath;
    path respath;
    respath := pa--( xpart pa, ypart pb )--pb;
    if implicitdraw:
      drawarrow respath withpen pencircle scaled connectionlw;
    fi
    respath
  endgroup
enddef;

def conlabel(expr shiftdir, txt, z ) =
  begingroup
    save tmppic;
    picture tmppic;
    tmppic := shiftoffwlm( txt scaled textscale, shiftdir ) shifted z;
    if implicitdraw:
      draw tmppic;
    fi
    tmppic
  endgroup
enddef;

def leftpoint(expr pic, n, i ) =
  (i/(n+1))[llcorner pic,ulcorner pic]
enddef;
def rightpoint(expr pic, n, i ) =
  (i/(n+1))[lrcorner pic,urcorner pic]
enddef;
def bottompoint(expr pic, n, i ) =
  (i/(n+1))[llcorner pic,lrcorner pic]
enddef;
def toppoint(expr pic, n, i ) =
  (i/(n+1))[ulcorner pic,urcorner pic]
enddef;
def llconnect(expr pica, picb, slide ) =
  begingroup
    hhconnect( leftpoint( pica, 1, 1 ), leftpoint( picb, 1, 1 ), 0.5, slide )
  endgroup
enddef;
def rrconnect(expr pica, picb, slide ) =
  begingroup
    hhconnect( rightpoint( pica, 1, 1 ), rightpoint( picb, 1, 1 ), 0.5, slide )
  endgroup
enddef;
def ttconnect(expr pica, picb, slide ) =
  begingroup
    vvconnect( toppoint( pica, 1, 1 ), toppoint( picb, 1, 1 ), 0.5, slide )
  endgroup
enddef;
def bbconnect(expr pica, picb, slide ) =
  begingroup
    vvconnect( bottompoint( pica, 1, 1 ), bottompoint( picb, 1, 1 ), 0.5, slide )
  endgroup
enddef;
def lrconnect(expr pica, picb ) =
  begingroup
    hhconnect( leftpoint( pica, 1, 1 ), rightpoint( picb, 1, 1 ), 0.5, 0 )
  endgroup
enddef;
def rlconnect(expr pica, picb ) =
  begingroup
    hhconnect( rightpoint( pica, 1, 1 ), leftpoint( picb, 1, 1 ), 0.5, 0 )
  endgroup
enddef;
def btconnect(expr pica, picb ) =
  begingroup
    vvconnect( bottompoint( pica, 1, 1 ), toppoint( picb, 1, 1 ), 0.5, 0 )
  endgroup
enddef;
def tbconnect(expr pica, picb ) =
  begingroup
    vvconnect( toppoint( pica, 1, 1 ), bottompoint( picb, 1, 1 ), 0.5, 0 )
  endgroup
enddef;
def ltconnect(expr pica, picb ) =
  begingroup
    hvconnect( leftpoint( pica, 1, 1 ), toppoint( picb, 1, 1 ) )
  endgroup
enddef;
def lbconnect(expr pica, picb ) =
  begingroup
    hvconnect( leftpoint( pica, 1, 1 ), bottompoint( picb, 1, 1 ) )
  endgroup
enddef;
def rtconnect(expr pica, picb ) =
  begingroup
    hvconnect( rightpoint( pica, 1, 1 ), toppoint( picb, 1, 1 ) )
  endgroup
enddef;
def rbconnect(expr pica, picb ) =
  begingroup
    hvconnect( rightpoint( pica, 1, 1 ), bottompoint( picb, 1, 1 ) )
  endgroup
enddef;
def tlconnect(expr pica, picb ) =
  begingroup
    vhconnect( toppoint( pica, 1, 1 ), leftpoint( picb, 1, 1 ) )
  endgroup
enddef;
def trconnect(expr pica, picb ) =
  begingroup
    vhconnect( toppoint( pica, 1, 1 ), rightpoint( picb, 1, 1 ) )
  endgroup
enddef;
def blconnect(expr pica, picb ) =
  begingroup
    vhconnect( bottompoint( pica, 1, 1 ), leftpoint( picb, 1, 1 ) )
  endgroup
enddef;
def brconnect(expr pica, picb ) =
  begingroup
    vhconnect( bottompoint( pica, 1, 1 ), rightpoint( picb, 1, 1 ) )
  endgroup
enddef;

def connect(expr pica, picb ) =
  begingroup
    save rxa, rya, rxb, ryb;
    numeric rxa;
    numeric rya;
    numeric rxb;
    numeric ryb;
    save ca, cb;
    pair ca;
    pair cb;
    rxa = 0.5 * ((xpart lrcorner pica) - (xpart llcorner pica));
    rya = 0.5 * ((ypart ulcorner pica) - (ypart llcorner pica));
    rxb = 0.5 * ((xpart lrcorner picb) - (xpart llcorner picb));
    ryb = 0.5 * ((ypart ulcorner picb) - (ypart llcorner picb));
    ca = center pica;
    cb = center picb;
    if (xpart ca) < (xpart cb) - ( rxa + rxb ):
      if (ypart ca) < (ypart cb) - ( rya + ryb ):
	rbconnect( pica, picb )
      elseif (ypart ca) > (ypart cb) + ( rya + ryb ):
	rtconnect( pica, picb )
      else:
	rlconnect( pica, picb )
      fi
    elseif (xpart ca) > (xpart cb) + ( rxa + rxb ):
      if (ypart ca) < (ypart cb) - ( rya + ryb ):
	lbconnect( pica, picb )
      elseif (ypart ca) > (ypart cb) + ( rya + ryb ):
	ltconnect( pica, picb )
      else:
	lrconnect( pica, picb )
      fi
    else:
      if (ypart ca) < (ypart cb) - ( rya + ryb ):
	tbconnect( pica, picb )
      elseif (ypart ca) > (ypart cb) + ( rya + ryb ):
	btconnect( pica, picb )
      else:
	hvconnect( center pica, center picb )
      fi
    fi
  endgroup
enddef;
