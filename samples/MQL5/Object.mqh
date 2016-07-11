//+------------------------------------------------------------------+
//|                                                       Object.mqh |
//|                   Copyright 2009-2013, MetaQuotes Software Corp. |
//|                                              http://www.mql5.com |
//+------------------------------------------------------------------+
#include "StdLibErr.mqh"
//+------------------------------------------------------------------+
//| Class CObject.                                                   |
//| Purpose: Base class for storing elements.                        |
//+------------------------------------------------------------------+
class CObject
  {
private:
   CObject          *m_prev;               // previous item of list
   CObject          *m_next;               // next item of list

public:
                     CObject(void): m_prev(NULL),m_next(NULL)            {                 }
                    ~CObject(void)                                       {                 }
   //--- methods to access protected data
   CObject          *Prev(void)                                    const { return(m_prev); }
   void              Prev(CObject *node)                                 { m_prev=node;    }
   CObject          *Next(void)                                    const { return(m_next); }
   void              Next(CObject *node)                                 { m_next=node;    }
   //--- methods for working with files
   virtual bool      Save(const int file_handle)                         { return(true);   }
   virtual bool      Load(const int file_handle)                         { return(true);   }
   //--- method of identifying the object
   virtual int       Type(void)                                    const { return(0);      }
   //--- method of comparing the objects
   virtual int       Compare(const CObject *node,const int mode=0) const { return(0);      }
  };
//+------------------------------------------------------------------+
