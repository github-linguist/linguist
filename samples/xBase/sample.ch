#ifndef __HARBOUR__
   #ifndef __XPP__
      #ifndef __CLIP__
         #ifndef FlagShip
            #define __CLIPPER__
         #endif
      #endif
   #endif
#endif

/* File create flags */
#define FC_NORMAL          0  /* No file attributes are set */
#define FC_READONLY        1
#define FC_HIDDEN          2
#define FC_SYSTEM          4

// New-style comment
#command SET DELETED <x:ON,OFF,&>      => Set( _SET_DELETED, <(x)> )
#command SET DELETED (<x>)             => Set( _SET_DELETED, <x> )
#command @ <row>, <col> SAY <exp> [PICTURE <pic>] [COLOR <clr>] => ;
         DevPos( <row>, <col> ) ; DevOutPict( <exp>, <pic> [, <clr>] )
         
#command ENDIF <*x*> => endif

#ifdef __CLIPPER__   
   #xtranslate hb_MemoWrit( [<x,...>] )  => MemoWrit( <x> )
   #xtranslate hb_dbExists( <t> )        => File( <t> )
   #xtranslate hb_dbPack()               => __dbPack()
   #xtranslate hb_default( @<v>, <x> )   => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ),, <v> := <x>, )
#endif
