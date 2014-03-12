#ifndef LIBCANIH
#define LIBCANIH
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <cstring>

#define int64 unsigned long long
//#define DEBUG

#ifdef DEBUG
#define dout cout
#else
#define dout if (0) cerr
#endif

using namespace std;

namespace libcanister
{

    //the canmem object is a generic memory container used commonly
    //throughout the canister framework to hold memory of uncertain
    //length which may or may not contain null bytes. 
    class canmem
    {
    public:
        char* data; //the raw memory block
        int size; //the absolute length of the block
        canmem(); //creates an unallocated canmem
        canmem(int allocsize); //creates an allocated, blank canmem of size
        canmem(char* strdata); //automates the creation of zero-limited canmems
        ~canmem(); //cleans up the canmem
        void zeromem(); //overwrites this canmem
        void fragmem(); //overwrites this canmem with fragment notation
        void countlen(); //counts length of zero-limited strings and stores it in size
        void trim(); //removes any nulls from the end of the string
        static canmem null(); //returns a singleton null canmem
        
    };
    
    //contains information about the canister
    class caninfo
    {
    public:
        canmem path; //physical path
        canmem internalname; //a name for the canister
        int numfiles; //the number of files in the canister
    };
    
    //necessary for the use of this class as a type in canfile
    class canister;
    
    //this object holds the definition of a 'file' within the
    //canister 'filesystem.'
    class canfile
    {
    public:
        libcanister::canister* parent; //the canister that holds this file
        canmem path; //internal path ('filename')
        canmem data; //the file's decompressed contents
        int isfrag; //0 = probably not fragment, 1 = definitely a fragment (ignore)
        int cfid; //'canfile id' -- a unique ID for this file
        int64 dsize; //ondisk size (compressed form size)
        int cachestate; //0 = not in memory, 1 = in memory, 2 = in memory and needs flush
                        //-1 = error, check the data for the message
        void cache(); //pull the file from disk and cache it in memory
        void cachedump(); //deletes the contents of this file from the memory cache after assuring the on disk copy is up to date
        void cachedumpfinal(fstream& infile); //same as cachedump, but more efficient during closing procedures
        void flush(); //updates the on disk copy, but retains the memory cache
    };

    //the primary class
    //this defines and controls a single canister
    class canister
    {
        //table of contents
        //absolutely worthless to the control code in the canister
        //but quite useful to programs using the API, as they may
        //desire to enumerate the files in a canister for a user's
        //use or for their own.
        //contains a newline-delimited list of files in the container.
        canfile TOC;
    public:
        caninfo info; //the general info about this canister

        //the raw canfiles -- recommended that programs do not modify
        //these files directly, but not enforced.
        canfile* files;
        bool readonly; //if true then no write routines will do anything
        
        //maximum number of files to have in memory at any given
        //time, change this to whatever suits your application.
        int cachemax;
        int cachecnt; //number of files in the cache (should not be modified)

        //both initialize the canister from a physical location
        canister (canmem fspath);
        canister (char* fspath);

        //destroys the canister (after flushing the modded buffers, of course)
        ~canister();
        
        //open the fspath
        //does it exist?
        // | --- yes --- opening it (return 1)
        // | --- yes --- file is corrupted, halting (return -1)
        // | --- no  --- making a new one (return 0)
        int open();
        
        //close the canister, flush all buffers, clean up
        int close();
        
        //deletes the file at path inside this canister
        int delFile(canmem path);
        
        //pulls the contents of the file from disk or memory and returns it as a file
        canfile getFile(canmem path);
        
        //creates a file if it does not exist, otherwise overwrites
        //returns whether operation succeeded
        bool writeFile(canmem path, canmem data);
        bool writeFile(canfile file);
        
        //get the 'table of contents', a file containing a newline delimited
        //list of the file paths in the container which have contents
        canfile getTOC();
 
        //brings the cache back within the cachemax limit
        //important: sCFID is the safe CFID
        //(the CFID of the file we want to avoid uncaching)
        //really just used internally, but it can't do any harm.
        void cacheclean(int sCFID, bool dFlush = false);
    };

}

#endif