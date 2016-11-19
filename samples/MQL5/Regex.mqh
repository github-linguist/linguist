//+------------------------------------------------------------------+
//|                                               Regular Expression |
//|                        Copyright 2016, MetaQuotes Software Corp. |
//|                                             https://www.mql5.com |
//+------------------------------------------------------------------+
//| Regular Expression Library from .NET Framework 4.6.1 implemented |
//| in MetaQuotes Language 5 (MQL5)                                  |
//| Original sources at https://github.com/Microsoft/referencesource |
//|                                                                  |
//| The capabilities of the Regular Expression Library include:      |
//| - Lazy quantifiers                                               |
//| - Positive and negative lookbehind                               |
//| - Conditional evaluation                                         |
//| - Balancing group definitions                                    |
//| - Nonbacktracking subexpressions                                 |
//| - Right-to-left matching                                         |
//|                                                                  |
//| If you find any functional differences between Regular Expression|
//| Library for MQL5 and the original .NET Framework 4.6.1 project,  |
//| please contact developers of MQL5 on the Forum at www.mql5.com.  |
//|                                                                  |
//| You can report bugs found in the computational algorithms of the |
//| Regular Expression Library from .Net Framework 4.6.1 by notifying|
//| the project coordinators.                                        |
//+------------------------------------------------------------------+
//|                     The MIT License (MIT)                        |
//|                                                                  |
//| Permission is hereby granted, free of charge, to any person      |
//| obtaining a copy of this software and associated documentation   |
//| files (the "Software"), to deal in the Software without          |
//| restriction, including without limitation the rights to use,     |
//| copy, modify, merge, publish, distribute, sublicense, and/or sell|
//| copies of the Software, and to permit persons to whom the        |
//| Software is furnished to do so, subject to the following         |
//| conditions:                                                      |
//|                                                                  |
//| The above copyright notice and this permission notice shall be   |
//| included in all copies or substantial portions of the Software.  |
//|                                                                  |
//| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  |
//| EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  |
//| OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND         |
//| NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT      |
//| HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,     |
//| WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING     |
//| FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    |
//| OTHER DEALINGS IN THE SOFTWARE.                                  |
//|                                                                  |
//| A copy of the MIT License (MIT) is available at                  |
//| https://opensource.org/licenses/MIT                              |
//+------------------------------------------------------------------+
class Match;
class MatchCollection;
class CachedCodeEntry;
class ReplacementReference;
class RunnerReference;
class RegexRunner;
//+------------------------------------------------------------------+
//| Callback class.                                                  |
//+------------------------------------------------------------------+
typedef string(*MatchEvaluator)(Match*);
#include <Internal\TimeSpan\TimeSpan.mqh>
#include <Internal\Generic\LinkedList.mqh>
#include <Internal\Generic\Dictionary.mqh>
#include "RegexOptions.mqh"
#include "RegexCode.mqh"
#include "RegexTree.mqh"
#include "RegexParser.mqh"
#include "RegexReplacement.mqh"
#include "RegexWriter.mqh"
#include "RegexMatchCollection.mqh"
#include "RegexRunner.mqh"
#include "RegexInterpreter.mqh"
//+------------------------------------------------------------------+
//| Purpose: The Regex class represents a single compiled instance of| 
//| a regular expression.                                            |
//+------------------------------------------------------------------+
//+------------------------------------------------------------------+
//| Represents an immutable, compiled regular expression. Also       |
//| contains static methods that allow use of regular expressions    |
//| without instantiating a Regex explicitly.                        |
//+------------------------------------------------------------------+
class Regex
  {
protected:
   string            m_pattern;
   RegexOptions      m_roptions;
private:
   static const TimeSpan MaximumMatchTimeout;
public:
   static const TimeSpan InfiniteMatchTimeout;
protected:
   TimeSpan          m_internalMatchTimeout;// timeout for the execution of this regex
private:
   static const string DefaultMatchTimeout_ConfigKeyName;
public:
   static const TimeSpan FallbackDefaultMatchTimeout;
   static const TimeSpan DefaultMatchTimeout;
protected:
   Dictionary<int,int>*m_caps;        // if captures are sparse, this is the hashtable capnum->index
   Dictionary<string,int>*m_capnames; // if named captures are used, this maps names->index
   string            m_capslist[];    // if captures are sparse or named captures are used, this is the sorted list of names
   int               m_capsize;       // the size of the capture array
   RegexTree        *m_tree;
   RunnerReference *m_runnerref;      // cached runner
   ReplacementReference*m_replref;    // cached parsed replacement pattern
   RegexCode        *m_code;          // if interpreted, this is the code for RegexIntepreter
   bool              m_refsInitialized;// Default is false
   static            LinkedList<CachedCodeEntry*>m_livecode;// the cached of code that are currently loaded
   static int        m_cacheSize;     // Default is 15
public:
   static const int  MaxOptionShift;
public:
   //--- Constructors:
   //+------------------------------------------------------------------+
   //| Initializes a new instance of the Regex class.                   |
   //+------------------------------------------------------------------+
                     Regex() : m_refsInitialized(false)
     {
      this.m_internalMatchTimeout=DefaultMatchTimeout;
     }
   //+------------------------------------------------------------------+
   //| Creates and compiles a regular expression object for the         |
   //| specified regular expression.                                    |
   //+------------------------------------------------------------------+
                     Regex(const string pattern) : m_refsInitialized(0)
     {
      Initialize(pattern,None,DefaultMatchTimeout,false);
     }
   //+------------------------------------------------------------------+
   //| Creates and compiles a regular expression object for the         |
   //| specified regular expression with options that modify the        |
   //| pattern.                                                         |
   //+------------------------------------------------------------------+
                     Regex(const string pattern,RegexOptions options) : m_refsInitialized(0)
     {
      Initialize(pattern,options,DefaultMatchTimeout,false);
     }
   //+------------------------------------------------------------------+
   //| Initializes a new instance of the Regex class for the specified  |
   //| regular expression,with options that modify the pattern and a    |
   //| value that specifies how long a pattern matching method should   |
   //| attempt a match before it times out.                             |
   //+------------------------------------------------------------------+
                     Regex(const string pattern,RegexOptions options,const TimeSpan &matchTimeout) : m_refsInitialized(0)
     {
      Initialize(pattern,options,matchTimeout,false);
     }
   //--- Destructors:     
   //+------------------------------------------------------------------+
   //| Destructor without parameters.                                   |
   //+------------------------------------------------------------------+
                    ~Regex()
     {
      if(CheckPointer(m_tree)==POINTER_DYNAMIC)
        {
         delete m_tree;
        }
      if(CheckPointer(m_caps)==POINTER_DYNAMIC)
        {
         delete m_caps;
        }
      bool deleteRun=true;
      bool deleteRepl = true;
      bool deleteCode = true;
      for(LinkedListNode<CachedCodeEntry*>*current=m_livecode.First(); current!=NULL; current=current.Next())
        {
         if(CheckPointer(current.Value())==POINTER_DYNAMIC)
           {
            if(current.Value().RunnerRef()==m_runnerref)
              {
               deleteRun=false;
              }
            if(current.Value().ReplRef()==m_replref)
              {
               deleteRepl=false;
              }
            if(current.Value().Code()==m_code)
              {
               deleteCode=false;
              }
           }
        }
      if(CheckPointer(m_replref)==POINTER_DYNAMIC && deleteRepl)
        {
         delete m_replref;
        }
      if(CheckPointer(m_runnerref)==POINTER_DYNAMIC && deleteRun)
        {
         delete m_runnerref;
        }
      if(CheckPointer(m_code)==POINTER_DYNAMIC && deleteCode)
        {
         delete m_code;
        }
     }
private:
   //+------------------------------------------------------------------+
   //| General constructor with parameters.                             |
   //+------------------------------------------------------------------+
                     Regex(const string pattern,RegexOptions options,const TimeSpan &matchTimeout,const bool useCache) : m_refsInitialized(0)
     {
      Initialize(pattern,options,matchTimeout,useCache);
     }
   //--- Methods:
   //+------------------------------------------------------------------+
   //| Initialize.                                                      |
   //+------------------------------------------------------------------+
   void Initialize(const string pattern,RegexOptions options,const TimeSpan &matchTimeout,const bool useCache)
     {
      RegexTree *tree;
      CachedCodeEntry *cached=NULL;
      if(pattern==NULL)
        {
         Print("Argument 'pattern'= Null.");
         //--- return
         return;
        }
      if(options<None || (((int) options)>>MaxOptionShift)!=0)
        {
         Print("Argument 'options' out of range.");
         //--- return 
         return;
        }
      if((options    &ECMAScript)!=0
         && (options  &~(ECMAScript|IgnoreCase|Multiline
         #ifdef _DEBUG
         |Debug
         #endif
         ))!=0)
        {
         Print("Argument 'options' out of range");
         //--- return
         return;
        }
      ValidateMatchTimeout(matchTimeout);
      //--- Try to look up this regex in the cache.  We do this regardless of whether useCache is true since there's really no reason not to. 
      string key=IntegerToString(options)+":"+pattern;
      cached=LookupCachedAndUpdate(key);
      this.m_pattern=pattern;
      this.m_roptions=options;
      this.m_internalMatchTimeout=matchTimeout;
      if(cached==NULL)
        {
         //--- Parse the input
         tree=RegexParser::Parse(pattern,(RegexOptions)m_roptions);
         //--- Extract the relevant information
         m_capnames=tree.CapNames();
         tree.GetCapsList(m_capslist);
         m_code       = RegexWriter::Write(tree);
         m_caps       = m_code.Caps();
         m_capsize    = m_code.CapSize();
         InitializeReferences();
         m_tree=tree;
         if(useCache)
           {
            cached=CacheCode(key);
           }
        }
      else
        {
         m_caps       = cached.Caps();
         m_capnames   = cached.CapNames();
         cached.GetCapList(m_capslist);
         m_capsize    = cached.CapSize();
         m_code       = cached.Code();
         m_runnerref  = cached.RunnerRef();
         m_replref    = cached.ReplRef();
         m_refsInitialized=true;
        }
     }
public:
   //--- Methods:
   //+------------------------------------------------------------------+
   //| Pattern.                                                         |
   //+------------------------------------------------------------------+
   string Pattern()
     {
      //--- return pattern
      return (m_pattern);
     }
   //+------------------------------------------------------------------+
   //| Validates that the specified match timeout value is valid.       |
   //| The valid range is:                                              | 
   //| TimeSpan::Zero < matchTimeout <= Regex::MaximumMatchTimeout.     |
   //+------------------------------------------------------------------+
   static void ValidateMatchTimeout(const TimeSpan &matchTimeout)
     {
      if(InfiniteMatchTimeout==matchTimeout)
        {
         //--- return
         return;
        }
      //--- Change this to make sure timeout is not longer then Environment.Ticks cycle length:
      if(TimeSpan::Zero()<matchTimeout && matchTimeout<=MaximumMatchTimeout)
        {
         //--- return
         return;
        }
      Print("Argument 'matchTimeout' out of range.");
     }
   //+------------------------------------------------------------------+
   //| Specifies the default RegEx matching timeout value (i.e. the     |
   //| timeout that will be used if no explicit timeout is specified).  |
   //+------------------------------------------------------------------+
   static TimeSpan InitDefaultMatchTimeout()
     {
      //--- retrun result
      return (FallbackDefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Gets the runner reference.                                       |
   //+------------------------------------------------------------------+
   RunnerReference*RunnerReference()
     {
      //--- return runner reference
      return (m_runnerref);
     }
   //+------------------------------------------------------------------+
   //| Gets the weak reference.                                         |
   //+------------------------------------------------------------------+
   ReplacementReference*ReplacementReference()
     {
      //--- return week reference
      return (m_replref);
     };
   Dictionary<int,int>*Caps()
     {
      //---
      return (m_caps);
     }
   Dictionary<string,int>*CapNames()
     {
      //--- return 
      return (m_capnames);
     }
   int CapSize()
     {
      //--- 
      return (m_capsize);
     }
   //+------------------------------------------------------------------+
   //| Returns the options passed into the constructor.                 |
   //+------------------------------------------------------------------+
   RegexOptions Options()
     {
      //--- return
      return (m_roptions);
     }
   //+------------------------------------------------------------------+
   //| Escape metacharacters within the string.                         |
   //+------------------------------------------------------------------+
   static string Escape(const string str)
     {
      if(StringLen(str)==NULL)
        {
         Print("Argument 'str' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return RegexParser::Escape(str);
     }
   //+------------------------------------------------------------------+
   //| Unescape character codes within the string.                      |
   //+------------------------------------------------------------------+
   static string Unescape(const string str)
     {
      if(StringLen(str)==NULL)
        {
         Print("Argument 'str' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return RegexParser::Unescape(str);
     }
   //+------------------------------------------------------------------+
   //| CacheCount.                                                      |
   //+------------------------------------------------------------------+
   static int CacheCount()
     {
      //--- return count
      return (m_livecode.Count());
     }
   //+------------------------------------------------------------------+
   //| CacheSize.                                                       |
   //+------------------------------------------------------------------+
   static int CacheSize()
     {
      //--- return size
      return (m_cacheSize);
     }
   //+------------------------------------------------------------------+
   //| CacheSize.                                                       |
   //+------------------------------------------------------------------+
   static void CacheSize(const int value)
     {
      if(value<0)
        {
         Print("Argument 'value' out of range.");
         //--- return
         return;
        }
      m_cacheSize=value;
      if(m_livecode.Count()>m_cacheSize)
        {
         while(m_livecode.Count()>m_cacheSize)
           {
            m_livecode.RemoveLast();
           }
        }
     }
   //+------------------------------------------------------------------+
   //| The match timeout used by this Regex instance.                   |
   //+------------------------------------------------------------------+
   TimeSpan MatchTimeout()
     {
      //--- return result
      return (m_internalMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| True if the regex is leftward.                                   |
   //|                                                                  |
   //| Indicates whether the regular expression matches from right to   |
   //| left.                                                            |
   //+------------------------------------------------------------------+
   bool RightToLeft()
     {
      //--- return result
      return UseOptionR();
     }
   //+------------------------------------------------------------------+
   //| Returns the regular expression pattern passed into the           |
   //| constructor.                                                     |
   //+------------------------------------------------------------------+
   string ToString()
     {
      //--- return result
      return (m_pattern);
     }
   //+------------------------------------------------------------------+
   //| Returns an array of the group names that are used to capture     |
   //| groups in the regular expression. Only needed if the regex is not|
   //| known until runtime, and one wants to extract captured groups.   |
   //| (Probably unusual, but supplied for completeness.).              |
   //+------------------------------------------------------------------+
   void GetGroupNames(string &result[])
     {
      if(ArraySize(m_capslist)==NULL)
        {
         int max=m_capsize;
         ArrayResize(result,max);
         for(int i=0; i<max; i++)
           {
            result[i]=IntegerToString(i);
           }
        }
      else
        {
         ArrayCopy(result,m_capslist,0,0);
        }
     }
   //+------------------------------------------------------------------+
   //| Returns an array of the group names that are used to capture     |
   //| groups in the regular expression. Only needed if the regex is not|
   //| known until runtime, and one wants to extract captured groups.   |
   //| (Probably unusual, but supplied for completeness.).              |
   //+------------------------------------------------------------------+
   void GetGroupNumbers(int &result[])
     {
      if(m_caps==NULL)
        {
         int max=m_capsize;
         ArrayResize(result,max);
         for(int i=0; i<max; i++)
           {
            result[i]=i;
           }
        }
      else
        {
         ArrayResize(result,m_caps.Count());
         DictionaryEnumerator<int,int>*de=m_caps.GetEnumerator();
         while(de.MoveNext())
           {
            result[(int)de.Value()]=(int)de.Key();
           }
         delete de;
        }
     }
   //+------------------------------------------------------------------+
   //| Given a group number, maps it to a group name. Note that nubmered|
   //| groups automatically get a group name that is the decimal string |
   //| equivalent of its number.                                        |
   //+------------------------------------------------------------------+
   string GroupNameFromNumber(const int index)
     {
      int i=index;
      if(ArraySize(m_capslist)==NULL)
        {
         if(i>=0 && i<m_capsize)
           {
            //--- return result
            return IntegerToString(i);
           }
         //--- return result
         return ("");
        }
      else
        {
         if(m_caps!=NULL)
           {
            if(!m_caps.ContainsKey(i))
              {
               //--- return result
               return ("");
              }
            i=m_caps[i];
           }
         if(i>=0 && i<ArraySize(m_capslist))
           {
            //--- return result
            return (m_capslist[i]);
           }
         //--- return result
         return ("");
        }
     }
   //+------------------------------------------------------------------+
   //| Given a group name, maps it to a group number. Note that nubmered|
   //| groups automatically get a group name that is the decimal string |   
   //| equivalent of its number.                                        |
   //|                                                                  |
   //| Returns -1 if the name is not a recognized group name.           |
   //+------------------------------------------------------------------+
   int GroupNumberFromName(const string name)
     {
      int result=-1;
      if(name==NULL)
        {
         Print("Argument 'name' = NNULL.");
         //--- return NULL
         return (NULL);
        }
      //--- look up name if we have a hashtable of names
      if(m_capnames!=NULL)
        {
         if(!m_capnames.ContainsKey(name))
           {
            //--- return result
            return (-1);
           }
         //--- return result
         return (m_capnames[name]);
        }
      //--- convert to an int if it looks like a number
      result=0;
      for(int i=0; i<StringLen(name); i++)
        {
         ushort ch=StringGetCharacter(name,i);
         if(ch>'9' || ch<'0')
           {
            //--- return result
            return (-1);
           }
         result *= 10;
         result += (ch - '0');
        }
      //--- return int if it's in range
      if(result>=0 && result<m_capsize)
        {
         //--- return result
         return (result);
        }
      //--- return result
      return (-1);
     }
   //+------------------------------------------------------------------+
   //| Searches the input string for one or more occurrences of the text|
   //| supplied in the pattern parameter.                               |
   //+------------------------------------------------------------------+
   static bool IsMatch(const string in,const string pattern)
     {
      //--- return result
      return IsMatch(in, pattern, None, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more occurrences of the text   |
   //| supplied in the pattern parameter.                               |
   //+------------------------------------------------------------------+
   static bool IsMatch(const string in,const string pattern,const RegexOptions options)
     {
      //--- return result
      return IsMatch(in, pattern, options, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more occurrences of the text   |
   //| supplied in the pattern parameter.                               |
   //+------------------------------------------------------------------+
   static bool IsMatch(const string in,const string pattern,const RegexOptions options,const TimeSpan &matchTimeout)
     {
      Regex regex(pattern,options,matchTimeout,true);
      //--- return result
      return (regex.IsMatch(in));
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more matches using the         |
   //| previous pattern, options, and starting position.                |
   //+------------------------------------------------------------------+
   bool IsMatch(const string in)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return IsMatch(in, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more matches using the         |
   //| previous pattern and options, with a new starting position.      |
   //+------------------------------------------------------------------+
   bool IsMatch(const string in,const int startat)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      Match *run=Run(true,-1,in,0,StringLen(in),startat);
      bool result=(NULL==run);
      if(CheckPointer(run)==POINTER_DYNAMIC)
        {
         delete run;
        }
      //--- return result
      return (result);
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more occurrences of the text|
   //| supplied in the pattern parameter.                               |
   //+------------------------------------------------------------------+
   static Match *Match(const string in,const string pattern)
     {
      //--- return result
      return Regex::Match(in, pattern, None, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more occurrences of the text|
   //| supplied in the pattern parameter. Matching is modified with an  |
   //| option string.                                                   |
   //+------------------------------------------------------------------+
   static Match *Match(const string in,const string pattern,const RegexOptions options)
     {
      //--- return result
      return Regex::Match(in, pattern, options, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Searches the in string for one or more occurrences of the text|
   //| supplied in the pattern parameter. Matching is modified with an  |
   //| option string.                                                   |
   //+------------------------------------------------------------------+
   static Match *Match(string in,string pattern,RegexOptions options,const TimeSpan &matchTimeout)
     {
      Regex *regex=new Regex(pattern,options,matchTimeout,true);
      //--- return result
      return (regex.Match(in));
     }
   //+------------------------------------------------------------------+
   //| Matches a regular expression with a string and returns the       |
   //| precise result as a RegexMatch object.                           |
   //+------------------------------------------------------------------+
   Match *Match(const string in)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return Regex::Match(in, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Matches a regular expression with a string and returns the       |
   //| precise result as a RegexMatch object.                           |
   //+------------------------------------------------------------------+
   Match *Match(const string in,const int startat)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return Run(false, -1, in, 0, StringLen(in), startat);
     }
   //+------------------------------------------------------------------+
   //| Matches a regular expression with a string and returns the       |
   //| precise result as a RegexMatch object.                           |
   //+------------------------------------------------------------------+
   Match *Match(const string in,const int beginning,const int length)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return Run(false, -1, in, beginning, length, UseOptionR() ? beginning + length : beginning);
     }
   //+------------------------------------------------------------------+
   //| Returns all the successful matches as if Match was called        |
   //| iteratively numerous times.                                      |
   //+------------------------------------------------------------------+
   static MatchCollection *Matches(const string in,const string pattern)
     {
      //--- return result
      return Regex::Matches(in, pattern, None, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Returns all the successful matches as if Match was called        |
   //| iteratively numerous times.                                      |
   //+------------------------------------------------------------------+
   static MatchCollection *Matches(const string in,const string pattern,const RegexOptions options)
     {
      //--- return result
      return Regex::Matches(in, pattern, options, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Returns all the successful matches as if Match was called        |
   //| iteratively numerous times.                                      |
   //+------------------------------------------------------------------+
   static MatchCollection *Matches(const string in,const string pattern,const RegexOptions options,const TimeSpan &matchTimeout)
     {
      Regex *regex=new Regex(pattern,options,matchTimeout,true);
      //--- return result
      return (regex.Matches(in));
     }
   //+------------------------------------------------------------------+
   //| Returns all the successful matches as if Match was called        |
   //| iteratively numerous times.                                      |
   //+------------------------------------------------------------------+
   MatchCollection *Matches(const string in)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return Matches(in, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Returns all the successful matches as if Match was called        |
   //| iteratively numerous times.                                      |
   //+------------------------------------------------------------------+
   MatchCollection *Matches(const string in,const int startat)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return new MatchCollection(GetPointer(this), in, 0, StringLen(in), startat);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the pattern with the "replacement"   |
   //| pattern, starting at the first character in the in string.       |
   //+------------------------------------------------------------------+
   static string Replace(const string in,const string pattern,const string replacement)
     {
      //--- return result
      return Replace(in, pattern, replacement, None, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern" with the "replacement" |
   //| pattern, starting at the first character in the in string.       |
   //+------------------------------------------------------------------+
   static string Replace(const string in,const string pattern,const string replacement,const RegexOptions options)
     {
      //--- return result
      return Replace(in, pattern, replacement, options, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern" with the "replacement" |
   //| pattern, starting at the first character in the in string.       |
   //+------------------------------------------------------------------+
   static string Replace(const string in,const string pattern,const string replacement,const RegexOptions options,const TimeSpan &matchTimeout)
     {
      Regex *regex = new Regex(pattern,options,matchTimeout,true);
      string result=regex.Replace(in,replacement);
      delete regex;
      //--- return result
      return (result);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern " with the "replacement"|
   //| pattern, starting at the first character in the in string, using |
   //| the previous patten.                                             |
   //+------------------------------------------------------------------+
   string Replace(const string in,const string replacement)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return Replace(in, replacement, -1, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the (previously defined) "pattern"   |
   //| with the "replacement" pattern, starting at the first character  |
   //| in the in string.                                                |
   //+------------------------------------------------------------------+
   string Replace(const string in,const string replacement,const int count)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result
      return Replace(in, replacement, count, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern" with the recent        |
   //| "replacement" pattern, starting at the character position        |
   //| "startat.".                                                      |
   //+------------------------------------------------------------------+
   string Replace(const string in,const string replacement,const int count,const int startat)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      if(in==NULL)
        {
         Print("Argument 'replacement' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- A little code to grab a cached parsed replacement object
      RegexReplacement *repl=m_replref.Get();
      if(repl==NULL || !(repl.Pattern()==replacement))
        {
         repl=RegexParser::ParseReplacement(replacement,m_caps,m_capsize,m_capnames,this.m_roptions);
         if(CheckPointer(m_replref.Get())==POINTER_DYNAMIC)
           {
            delete m_replref.Get();
           }
         m_replref.Set(repl);
        }
      //--- return result
      return repl.Replace(GetPointer(this), in, count, startat);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern" with the "replacement" |
   //| pattern ".".                                                     |
   //+------------------------------------------------------------------+
   static string Replace(const string in,const string pattern,MatchEvaluator evaluator)
     {
      //--- return result
      return Replace(in, pattern, evaluator, None, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern " with the recent       |
   //| "replacement" pattern, starting at the first character ".".      |
   //+------------------------------------------------------------------+
   static string Replace(const string in,const string pattern,MatchEvaluator evaluator,const RegexOptions options)
     {
      //--- return result
      return Replace(in, pattern, evaluator, options, DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern " with the recent       |
   //| "replacement" pattern, starting at the first character ".".      |
   //+------------------------------------------------------------------+
   static string Replace(const string in,const string pattern,MatchEvaluator evaluator,const RegexOptions options,const TimeSpan &matchTimeout)
     {
      Regex regex(pattern,options,matchTimeout,true);
      string result=regex.Replace(in,evaluator);
      //--- return result
      return (result);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern" with the recent        |
   //| "replacement" pattern, starting at the first character position  |
   //| ".".                                                             |
   //+------------------------------------------------------------------+
   string Replace(const string in,MatchEvaluator evaluator)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result  
      return Replace(in, evaluator, -1, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the "pattern" with the recent        |
   //| "replacement" pattern, starting at the first character position  |
   //| ".".                                                             |
   //+------------------------------------------------------------------+
   string Replace(const string in,MatchEvaluator evaluator,const int count)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result 
      return Replace(in, evaluator, count, UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Replaces all occurrences of the (previouly defined) "pattern"    |
   //| with the recent "replacement" pattern, starting at the character |
   //| position "startat."                                              |
   //+------------------------------------------------------------------+
   string Replace(const string in,MatchEvaluator evaluator,const int count,const int startat)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return NULL
         return (NULL);
        }
      //--- return result 
      return RegexReplacement::Replace(evaluator, GetPointer(this), in, count, startat);
     }
   //+------------------------------------------------------------------+
   //| Splits the "in" string at the position defined by "pattern".     |
   //+------------------------------------------------------------------+
   static void Split(string &result[],const string in,const string pattern)
     {
      Split(result,in,pattern,None,DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Splits the "in" string at the position defined by "pattern".     |
   //+------------------------------------------------------------------+
   static void Split(string &result[],const string in,const string pattern,RegexOptions options)
     {
      Split(result,in,pattern,options,DefaultMatchTimeout);
     }
   //+------------------------------------------------------------------+
   //| Splits the "in" string at the position defined by "pattern".     |
   //+------------------------------------------------------------------+
   static void Split(string &result[],const string in,const string pattern,const RegexOptions options,const TimeSpan &matchTimeout)
     {
      Regex regex(pattern,options,matchTimeout,true);
      regex.Split(result,in);
     }
   //+------------------------------------------------------------------+
   //| Splits the "in" string at the position defined by a previous     |
   //| "pattern".                                                       |
   //+------------------------------------------------------------------+
   void Split(string &result[],const string in)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return 
         return;
        }
      Split(result,in,0,UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Splits the "in" string at the position defined by a previous     |
   //| "pattern".                                                       |
   //+------------------------------------------------------------------+
   void Split(string &result[],const string in,const int count)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return 
         return;
        }
      RegexReplacement::Split(result,GetPointer(this),in,count,UseOptionR() ? StringLen(in) : 0);
     }
   //+------------------------------------------------------------------+
   //| Splits the "in" string at the position defined by a previous     |
   //| "pattern".                                                       |
   //+------------------------------------------------------------------+
   void Split(string &result[],const string in,const int count,const int startat)
     {
      if(in==NULL)
        {
         Print("Argument 'in' = NULL.");
         //--- return 
         return;
        }
      RegexReplacement::Split(result,GetPointer(this),in,count,startat);
     }
protected:
   //+------------------------------------------------------------------+
   //| InitializeReferences.                                            |
   //+------------------------------------------------------------------+
   void InitializeReferences()
     {
      if(m_refsInitialized)
        {
         Print("Internal error! On file:'"+__FILE__+"'; in function:'"+__FUNCTION__+"'.");
         //--- return
         return;
        }
      m_refsInitialized=true;
      m_runnerref  = new RunnerReference();
      m_replref    = new ReplacementReference();
     }
public:
   //+------------------------------------------------------------------+
   //| Internal worker called by all the  APIs.                         |
   //+------------------------------------------------------------------+
   Match *Run(const bool quick,const int prevlen,const string in,const int beginning,const int length,const int startat)
     {
      Match *match;
      RegexRunner *runner=NULL;
      if(startat<0 || startat>StringLen(in))
        {
         Print("Argument 'start' out of range.");
         //--- return NULL
         return (NULL);
        }
      if(length<0 || length>StringLen(in))
        {
         Print("Argument 'length' out of range.");
         //--- return NULL
         return (NULL);
        }
      bool fromCache=false;
      //--- There may be a cached runner; grab ownership of it if we can.
      if(m_runnerref!=NULL)
        {
         fromCache=true;
         runner=m_runnerref.Get();
        }
      //--- Create a RegexRunner instance if we need to
      if(runner==NULL)
        {
         fromCache=false;
         //--- Use the compiled RegexRunner factory if the code was compiled to MSIL
         runner=new RegexInterpreter(m_code);
        }
      //--- Do the scan starting at the requested position            
      match=runner.Scan(GetPointer(this),in,beginning,beginning+length,startat,prevlen,quick,m_internalMatchTimeout);
      if(m_runnerref!=NULL && !fromCache)
        {
         if(CheckPointer(m_runnerref.Get())==POINTER_DYNAMIC)
           {
            delete m_runnerref.Get();
           }
         //--- Release or fill the cache slot
         m_runnerref.Set(runner);
        }
      else if(!fromCache)
        {
         delete runner;
        }
#ifdef _DEBUG
      if(Debug() && match!=NULL)
        {
         match.Dump();
        }
#endif
      //--- return result
      return (match);
     }
   //+------------------------------------------------------------------+
   //| Find code cache based on options+pattern.                        |
   //+------------------------------------------------------------------+
   static CachedCodeEntry *LookupCachedAndUpdate(const string key)
     {
      for(LinkedListNode<CachedCodeEntry*>*current=m_livecode.First(); current!=NULL; current=current.Next())
        {
         if(current.Value().Key()==key)
           {
            //--- If we find an entry in the cache, move it to the head at the same time. 
            m_livecode.Remove(current);
            m_livecode.AddFirst(current);
            //--- retrun result
            return (current.Value());
           }
        }
      //--- return result
      return (NULL);
     }
   //+------------------------------------------------------------------+
   //| Add current code to the cache.                                   |
   //+------------------------------------------------------------------+
   CachedCodeEntry *CacheCode(const string key)
     {
      CachedCodeEntry *newcached=NULL;
      //--- first look for it in the cache and move it to the head
      for(LinkedListNode<CachedCodeEntry*>*current=m_livecode.First(); current!=NULL; current=current.Next())
        {
         if(current.Value().Key()==key)
           {
            m_livecode.Remove(current);
            m_livecode.AddFirst(current);
            //--- return result
            return (current.Value());
           }
        }
      //--- it wasn't in the cache, so we'll add a new one.  Shortcut out for the case where cacheSize is zero.
      if(m_cacheSize!=0)
        {
         newcached=new CachedCodeEntry(key,m_capnames,m_capslist,m_code,m_caps,m_capsize,m_runnerref,m_replref);
         m_livecode.AddFirst(newcached);
         if(m_livecode.Count()>m_cacheSize)
           {
            m_livecode.RemoveLast();
           }
        }
      //--- return result
      return (newcached);
     }
   //+------------------------------------------------------------------+
   //| Delete all objects in cache.                                     |
   //+------------------------------------------------------------------+
   static void ClearCache()
     {
      IEnumerator<CachedCodeEntry*>*en=m_livecode.GetEnumerator();
      while(en.MoveNext())
        {
         if(CheckPointer(en.Current())==POINTER_DYNAMIC)
           {
            if(CheckPointer(en.Current().RunnerRef().Get().RunRegex())==POINTER_DYNAMIC)
              {
               delete en.Current().RunnerRef().Get().RunRegex();
              }
            delete en.Current();
           }
        }
      delete en;
     }
   //+------------------------------------------------------------------+
   //| True if the O option was set.                                    |
   //+------------------------------------------------------------------+
   bool UseOptionC()
     {
      return(m_roptions) != 0;
     }
   //+------------------------------------------------------------------+
   //| True if the L option was set.                                    |
   //+------------------------------------------------------------------+
   bool UseOptionR()
     {
      return(m_roptions & RightToLeft) != 0;
     }
   //+------------------------------------------------------------------+
   //| UseOptionInvariant.                                              |
   //+------------------------------------------------------------------+
   bool UseOptionInvariant()
     {
      return(m_roptions) != 0;
     }
#ifdef _DEBUG
   //+------------------------------------------------------------------+
   //| True if the regex has debugging enabled.                         |
   //+------------------------------------------------------------------+
   bool Debug()
     {
      //--- return result
      return((m_roptions &Debug) != 0);
     }
#endif
  };
static const TimeSpan   Regex::MaximumMatchTimeout=TimeSpan::FromMilliseconds(Int32::MaxValue-1);
static const TimeSpan   Regex::InfiniteMatchTimeout(0,0,0,0,-1);
static const string Regex::DefaultMatchTimeout_ConfigKeyName="REGEX_DEFAULT_MATCH_TIMEOUT";
static const TimeSpan   Regex::FallbackDefaultMatchTimeout=Regex::InfiniteMatchTimeout;
static const TimeSpan   Regex::DefaultMatchTimeout=Regex::InitDefaultMatchTimeout();
static LinkedList<CachedCodeEntry*>Regex::m_livecode();
static int Regex::m_cacheSize=15;
static const int Regex::MaxOptionShift=10;
//+------------------------------------------------------------------+
//| Purpose: Used to cache byte codes or compiled factories.         |
//+------------------------------------------------------------------+
class CachedCodeEntry : public IComparable
  {
private:
   string            m_key;
   RegexCode        *m_code;
   Dictionary<int,int>*m_caps;
   Dictionary<string,int>*m_capnames;
   string            m_capslist[];
   int               m_capsize;
   RunnerReference *m_runnerref;
   ReplacementReference *m_replref;
public:
   //--- Constructors:
   //+------------------------------------------------------------------+
   //| Constructor with parameters.                                     |
   //+------------------------------------------------------------------+
                     CachedCodeEntry(const string key,Dictionary<string,int>*capnames,const string &capslist[],
                                                       RegexCode *code,Dictionary<int,int>*caps,const int capsize,
                                                       RunnerReference *runner,ReplacementReference *repl)
     {
      m_key=key;
      m_capnames=capnames;
      ArrayCopy(m_capslist,capslist);
      m_code= code;
      m_caps= caps;
      m_capsize=capsize;
      m_runnerref=runner;
      m_replref=repl;
     }
   //--- Destructors:
   //+------------------------------------------------------------------+
   //| destructor without parameters.                                   |
   //+------------------------------------------------------------------+
                    ~CachedCodeEntry()
     {
      if(CheckPointer(m_code)==POINTER_DYNAMIC)
        {
         delete m_code;
        }
      if(CheckPointer(m_caps)==POINTER_DYNAMIC)
        {
         delete m_caps;
        }
      if(CheckPointer(m_capnames)==POINTER_DYNAMIC)
        {
         delete m_capnames;
        }
      if(CheckPointer(m_replref)==POINTER_DYNAMIC)
        {
         delete m_replref;
        }
      if(CheckPointer(m_runnerref)==POINTER_DYNAMIC)
        {
         delete m_runnerref;
        }
     }
   //--- Methods:
   //+------------------------------------------------------------------+
   //| Gets the runnerref.                                              |
   //+------------------------------------------------------------------+
   RunnerReference *RunnerRef()
     {
      //--- reurn runnerref
      return (m_runnerref);
     }
   //+------------------------------------------------------------------+
   //| Gets the replref.                                                |
   //+------------------------------------------------------------------+
   ReplacementReference *ReplRef()
     {
      //--- return replref
      return (m_replref);
     }
   //+------------------------------------------------------------------+
   //| Gets the key.                                                    |
   //+------------------------------------------------------------------+
   string Key()
     {
      //--- return key
      return (m_key);
     }
   //+------------------------------------------------------------------+
   //| Gets the caps.                                                   |
   //+------------------------------------------------------------------+
   Dictionary<int,int>*Caps()
     {
      //--- return caps
      return (m_caps);
     }
   //+------------------------------------------------------------------+
   //| Gets the capnames.                                               |
   //+------------------------------------------------------------------+
   Dictionary<string,int>*CapNames()
     {
      //--- return capnames
      return (m_capnames);
     }
   //+------------------------------------------------------------------+
   //| Gets the capsize.                                                |
   //+------------------------------------------------------------------+
   int CapSize()
     {
      //--- retrun capsize
      return (m_capsize);
     }
   //+------------------------------------------------------------------+
   //| Gets the code.                                                   |
   //+------------------------------------------------------------------+
   RegexCode *Code()
     {
      //--- return code
      return (m_code);
     }
   //+------------------------------------------------------------------+
   //| Gets the caplist.                                                |
   //+------------------------------------------------------------------+
   void GetCapList(string &array[])
     {
      ArrayCopy(array,m_capslist);
     }
  };
//+------------------------------------------------------------------+
//| Used to cache a weak reference in a threadsafe way.              |
//+------------------------------------------------------------------+
class ReplacementReference
  {
private:
   RegexReplacement *m_obj;
public:
   //--- Destructors:
   //+------------------------------------------------------------------+
   //| Destructor without parameters.                                   |
   //+------------------------------------------------------------------+   
                    ~ReplacementReference()
     {
      if(CheckPointer(m_obj)==POINTER_DYNAMIC)
        {
         delete m_obj;
        }
     }
   //--- Methods:
   //+------------------------------------------------------------------+
   //| Get RegexReplacement.                                            |
   //+------------------------------------------------------------------+
   RegexReplacement *Get()
     {
      //--- return pointer
      return (m_obj);
     }
   //+------------------------------------------------------------------+
   //| Set RegexReplacement.                                            |
   //+------------------------------------------------------------------+
   void      Set(RegexReplacement *obj)
     {
      m_obj=obj;
     }
  };
//+------------------------------------------------------------------+
//| Used to cache one exclusive runner reference.                    |
//+------------------------------------------------------------------+
class RunnerReference
  {
private:
   RegexRunner      *m_obj;
public:
   //--- Destructors:
   //+------------------------------------------------------------------+
   //| Destructor without parameters.                                   |
   //+------------------------------------------------------------------+   
                    ~RunnerReference()
     {
      if(CheckPointer(m_obj)==POINTER_DYNAMIC)
        {
         delete m_obj;
        }
     }
   //--- Methods:
   //+------------------------------------------------------------------+
   //| Get RegexRunner.                                                 |
   //+------------------------------------------------------------------+
   RegexRunner *Get()
     {
      //--- return pointer
      return (m_obj);
     }
   //+------------------------------------------------------------------+
   //| Set RegexRunner.                                                 |
   //+------------------------------------------------------------------+
   void Set(RegexRunner *obj)
     {
      m_obj=obj;
     }
  };
//+------------------------------------------------------------------+
