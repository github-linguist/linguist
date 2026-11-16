namespace containers;

import "instance" // TOFIX: This is required to build Debug on Ubuntu 10.04, GCC 4.4.3
import "LinkList"

public class Link : ListItem
{
   class_fixed
public:
   uint64 data;
}

public class List<class LLT> : LinkList<Link, T = LLT, D = LLT>
{
   class_fixed
   // Generic iterator support
   LLT GetData(Link link)
   {
      return link ? ((class(LLT) && class(LLT).type == structClass) ? (LLT)&link.data : (LLT)link.data) : (LLT)0;
   }

   bool SetData(Link link, LLT value)
   {
      if(class(LLT).type == structClass)
         memcpy((void *)&link.data, (void *)(uintptr)value, class(LLT).structSize);
      else
         link.data = (uint64)value;
      return true;
   }

   Link Insert(Link after, LLT value)
   {
      Link link;
      Class cLLT = class(LLT);
      if(cLLT && cLLT.type == structClass)
      {
         uint sType = cLLT.structSize;
         link = (Link)new0 byte[sizeof(class ListItem) + sType];
         memcpy((void *)&link.data, (void *)value, sType);
      }
      else
      {
         // TOFIX: This allocates too much data?
         // link = Link { data = (uint64)value };
         link = (Link)new0 byte[sizeof(class ListItem) + sizeof(uint64)];
         link.data = (uint64)value;
      }
      LinkList::Insert(after, (LT)link);
      return link;
   }

   Link Add(LLT value)
   {
      return (Link)Insert(last, value);
   }

   void Remove(Link link)
   {
      LinkList::Remove(link);
      delete link;
   }

   void Delete(Link link)
   {
      D data = GetData(link);
      delete data;
      Remove(link);
   }

   void Free()
   {
      Link item = *&first;
      Class lltClass = class(LLT);
      bool byAddress = lltClass && lltClass.type == structClass;
      while(item)
      {
         Link next = item.next;
         D data = byAddress ? (LLT)&item.data : (LLT)item.data;
         delete data;
         delete item;
         item = next;
      }
      *&first = null;
      *&last = null;
      count = 0;

      /*
      LT item;
      while((item = first))
      {
         D data = GetData(item);
         delete data;
         Remove(item);
      }
      */
   }

   Link Find(const LLT value)
   {
      return (Link)Container::Find(value);
   }
}
