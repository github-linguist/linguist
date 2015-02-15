~% gap

            #########           ######         ###########           ###
         #############          ######         ############         ####
        ##############         ########        #############       #####
       ###############         ########        #####   ######      #####
      ######         #         #########       #####    #####     ######
     ######                   ##########       #####    #####    #######
     #####                    ##### ####       #####   ######   ########
     ####                    #####  #####      #############   ###  ####
     #####     #######       ####    ####      ###########    ####  ####
     #####     #######      #####    #####     ######        ####   ####
     #####     #######      #####    #####     #####         #############
      #####      #####     ################    #####         #############
      ######     #####     ################    #####         #############
      ################    ##################   #####                ####
       ###############    #####        #####   #####                ####
         #############    #####        #####   #####                ####
          #########      #####          #####  #####                ####

     Information at:  http://www.gap-system.org
     Try '?help' for help. See also  '?copyright' and  '?authors'

   Loading the library. Please be patient, this may take a while.
GAP4, Version: 4.4.12 of 17-Dec-2008, x86_64-unknown-linux-gnu-gcc
Components:  small 2.1, small2 2.0, small3 2.0, small4 1.0, small5 1.0, small6 1.0, small7 1.0, small8 1.0,
             small9 1.0, small10 0.2, id2 3.0, id3 2.1, id4 1.0, id5 1.0, id6 1.0, id9 1.0, id10 0.1, trans 1.0,
             prim 2.1  loaded.
Packages:    AClib 1.1, Polycyclic 2.6, Alnuth 2.2.5, AutPGrp 1.4, CrystCat 1.1.3, Cryst 4.1.6, CRISP 1.3.2,
             CTblLib 1.1.3, TomLib 1.1.4, FactInt 1.5.2, GAPDoc 1.2, FGA 1.1.0.1, IRREDSOL 1.1.2, LAGUNA 3.5.0,
             Sophus 1.23, Polenta 1.2.7, ResClasses 2.5.3  loaded.
gap> join := function(a, b, sep)
>   return Concatenation(a, sep, sep, b);
> end;
function( a, b, sep ) ... end
gap>
gap> join("Rosetta", "Code", ":");
"Rosetta::Code"
gap>
