/******************************************************************************
                                  ZLib
                    supporting functions for scripts
*******************************************************************************

   Want to say thanks?  Send me (Zarqon) a bat! (Or bat-related item)

   For a list of included functions, check out
   https://kolmafia.us/showthread.php?t=2072
   or type "ashwiki zlib" in the CLI

******************************************************************************/
since r20653;   // Torso Awaregness => Awareness

// stuff that has to be at the top
float abs(float n) { return n < 0 ? -n : n; }
string getvar(string varname);
string[string] vars;     // user's script settings that differ from defaults
string threadhost = "https://kolmafia.us/threads/";

//                             STRING ROUTINES

// returns the string between start and end in source
// passing an empty string for start or end means the end of the string
string excise(string source, string start, string end) {
   if (start != "") {
      if (!source.contains_text(start)) return "";
      source = substring(source,index_of(source,start)+length(start));
   }
   if (end == "") return source;
   if (!source.contains_text(end)) return "";
   return substring(source,0,index_of(source,end));
}

// case-sensitive == operator for strings
// DEPRECATED: ASH's == operator has been case sensitive since r16180
boolean equals(string s1, string s2) {
   static print("ZLib: equals() has been deprecated; please use the == operator", "olive");
   return (length(s1) == length(s2) && contains_text(s1,s2));
}


/*
wraps print(), prints message depending on getvar("verbosity") setting
has boolean return value -- replaces { print(message); return t/f; }
level == 0: abort error
level > 0: prints message if verbosity >= level, returns true
level < 0: prints message if verbosity >= abs(level), returns false
===== Recommended level scale: ====
0: abort error
±1: essential (and non-cluttering) information -- use very sparingly, since a verbosity of 1 is basically "silent mode"
±2: important and useful info -- this should generally be your base level for your most important messages
±4: interesting but non-essential information
±6: info which an overly curious person might like to see on their CLI
±10: details which are only helpful for debugging, such as "begin/end functionname()" or "current value of variable: value"
*/
boolean vprint(string message, string color, int level) {
   if (level == 0) abort(message);
   if (to_int(getvar("verbosity")) >= abs(level)) print(message,color);
   return (level > 0);
}
boolean vprint(string message, int level) { if (level > 0) return vprint(message, is_dark_mode() ? "silver" : "black" ,level); return vprint(message,"red",level); }
boolean vprint_html(string message, int level) {
   if (level == 0) { print_html(message); abort(); }
   if (to_int(getvar("verbosity")) >= abs(level)) print_html(message);
   return (level > 0);
}
boolean vprint(string message, string light_color, string dark_color, int level) {
   return vprint(message, is_dark_mode() ? dark_color : light_color, level);
}

// returns mixvar normalized to the specified basic ASH type, also normalizes comma-delimited lists of same
string normalized(string mixvar, string type, string glue) {
   switch (type) {
      case "boolean": return to_string(to_boolean(mixvar));
      case "bounty": return to_string(to_bounty(mixvar));
      case "class": return to_string(to_class(mixvar));
      case "coinmaster": return to_string(to_coinmaster(mixvar));
      case "effect": return to_string(to_effect(mixvar));
      case "element": return to_string(to_element(mixvar));
      case "familiar": return to_string(to_familiar(mixvar));
      case "float": return to_string(to_float(mixvar));
      case "int": return to_string(to_int(mixvar));
      case "item": return to_string(to_item(mixvar));
      case "location": return to_string(to_location(mixvar));
      case "monster": return to_string(to_monster(mixvar));
      case "phylum": return to_string(to_phylum(mixvar));
      case "servant": return (to_string(to_servant(mixvar)));
      case "skill": return to_string(to_skill(mixvar));
      case "stat": return to_string(to_stat(mixvar));
      case "thrall": return to_string(to_thrall(mixvar));
      case "vykea": return to_string(to_vykea(mixvar));
      case "list of string":
      case "string": return mixvar;
   }
   if (index_of(type,"list of ") == 0) {
       string[int] bits = split_string(mixvar,glue);
       mixvar = "";
       foreach n,bit in bits {
          if (n > 0) mixvar += glue;
          mixvar += normalized(bit,excise(type,"list of ",""), glue);
       }
   } else vprint("Unable to normalize type '"+type+"'.",-3);
   return mixvar;
}
string normalized(string mixvar, string type) { return normalized(mixvar, type, ", "); }

// the opposite of split_string(); useful for working with string lists
string join(string[int] pieces, string glue) {
   buffer res;
   boolean middle;
   foreach index in pieces {
      if (middle) res.append(glue);
      middle = true;
      res.append(pieces[index]);
   }
   return res;
}
string join(string[int] pieces) { return join(pieces, ", "); }
// returns true if a glue-delimited string list contains needle (case-insensitive)
boolean list_contains(string stringlist, string needle, string glue) {
   return create_matcher("(^|"+glue+")\\Q"+to_lower_case(needle)+"\\E($|"+glue+")",to_lower_case(stringlist)).find();
}
boolean list_contains(string stringlist, string needle) { return list_contains(stringlist, needle, ", "); }
// adds a unique entry to a glue-delimited string list (so won't add duplicates), returns modified string list
string list_add(string stringlist, string add, string glue) {
   if (length(stringlist) == 0) return add;
   if (list_contains(stringlist,add,glue)) return stringlist;
   return stringlist+glue+add;
}
string list_add(string stringlist, string add) { return list_add(stringlist, add, ", "); }
// removes any matching entries from a glue-delimited list, returns modified list
string list_remove(string stringlist, string del, string glue) {
   string[int] bits;
   foreach i,b in split_string(stringlist, glue) if (b != del) bits[i] = b;
   return join(bits,glue);
}
string list_remove(string stringlist, string del) { return list_remove(stringlist, del, ", "); }

// human-readable number (formats the number with localized grouping/decimal separators, rounds floats at specified place)
string rnum(int n) {
   return to_string(n,"%,d");
}
string rnum(float n, int place) {
   if (place < 1 || to_float(round(n)) == to_float(to_string(n,"%,."+place+"f"))) return rnum(round(n));
   return replace_all(create_matcher("0+$", to_string(n,"%,."+place+"f")),"");
}
string rnum(float n) { return rnum(n,2); }


//                             NUMBER ROUTINES

float minmax(float a, float min, float max) { return max(min(a,max),min); }

void set_avg(float toadd, string whichprop) {
   string initv = get_property(whichprop);
   if (initv == "") { set_property(whichprop,toadd+":1"); return; }
   float a = to_float(excise(initv,"",":"));
   float b = to_float(excise(initv,":",""));
   a = ((a * b)+toadd) / (b+1);
   b += 1;
   set_property(whichprop,a+":"+b);
}
float get_avg(string whichprop) {
   string initv = get_property(whichprop);
   if (initv == "") return 0;
   return to_float(substring(initv,0,index_of(initv,":")));
}

/*
Evaluates expressions in the format used by variable modifiers:

No spaces allowed, except as part of a zone/location name.
+ - * / ( ) have their usual mathematical meaning and precedence.
^ is exponentiation, with the highest precedence.
Math functions: ceil(x) floor(x) sqrt(x) min(x,y) max(x,y)
Text functions:
  loc(text), zone(text) - 1 if the current adventure location or zone contains the specified text, 0 elsewise.
  fam(text) - 1 if the player's familiar type contains the text, else 0.
  pref(text) - fetches the specified property as a float
Internally-used variables (upper-case letters)
  D - drunkenness
  F - fullness
  G - Grimace darkness (0..5)
  L - player level
  M - total moonlight (0..9)
  S - spleenness
  X - gender (-1=male, 1=female)
User-defined variables (provided in "values") must begin with lowercase letters/underscores
*/
float eval(string expr, float[string] values) {
   buffer b;
   matcher m = create_matcher("\\b[a-z_][a-zA-Z0-9_]*\\b", expr);
   while (m.find()) {
      string zar = m.group(0);
      if (values contains zar) m.append_replacement(b, values[zar].to_string());
   }
   m.append_tail(b);
   m = create_matcher("[a-z]",b);
   vprint("Evaluating '"+b.to_string()+"'...",10+(m.find() ? 0 : 1+to_int(is_integer(b))));
   return modifier_eval(b.to_string()); 
}


//                             SCRIPT ROUTINES

record {
   string ver;
   string vdate;
}[string] zv;

// SVN version of below.  soft = human-readable script name; proj = svn name; thread = thread number on kolmafia.us
string check_version(string soft, string proj, int thread) { buffer msg;
   if (get_property("_svnUpdated").to_boolean()) return "";
   if (count(zv) == 0) file_to_map("zversions.txt",zv);
   if (zv[proj].vdate == today_to_string()) return "";
   vprint_html("Checking for updates (running <a href='"+threadhost+thread+"' target='_blank'>"+soft+"</a> rev. "+svn_info(proj).revision+")...",1);
   zv[proj].vdate = today_to_string();
   map_to_file(zv,"zversions.txt");
   if (!svn_at_head(proj)) {
      cli_execute("svn update " + proj);
      msg.append(soft+" has been updated from r"+zv[proj].ver+" to r"+svn_info(proj).revision+".  The next time it is run, it will be current.");
   }
   if (to_int(zv[proj].ver) == svn_info(proj).revision) { map_to_file(zv,"zversions.txt"); return ""; }
   if (length(msg) == 0) msg.append(soft+" has been updated from r"+zv[proj].ver+" to r"+svn_info(proj).revision+" since you last ran it.");
   msg.insert(0,"<big><font color=red><b>"+soft+" Updated!</b></font></big><br>");	  
   msg.append("<br><a href='"+threadhost+thread+"' target='_blank'><u>Click here "+
      "for discussion of what's new.</u></a> (<a href='"+threadhost+thread+"/latest' target='_blank'><u>last post</u></a>)");
   if (contains_text(svn_info(proj).url,"svn.code.sf.net")) msg.append(" (<a href='"+replace_string(svn_info(proj).url,"svn.code.","")+"/log/' target='_blank'><u>SourceForge</u></a>)");
   zv[proj].ver = svn_info(proj).revision;
   map_to_file(zv,"zversions.txt");
   vprint_html(msg,1);
   return "<div class='versioninfo'>"+msg+"</div>";
}

// checks script version once daily, returns empty string, OR div with update message inside
string check_version(string soft, string prop, string thisver, int thread) { int w = 8; string page; matcher find_ver;
   if (count(zv) == 0) file_to_map("zversions.txt",zv);
   boolean sameornewer(string local, string server) {
      if (local == server) return true;
      string[int] loc = split_string(local,"\\.");
      string[int] ser = split_string(server,"\\.");
      for i from 0 to max(count(loc)-1,count(ser)-1) {
         if (i+1 > count(loc)) return false; if (i+1 > count(ser)) return true;
         if (loc[i].to_int() < ser[i].to_int()) return false;
         if (loc[i].to_int() > ser[i].to_int()) return true;
      }
      return local == server;
   }
   if (zv[prop].vdate != today_to_string()) {
      vprint("Checking for updates (running "+soft+" ver. "+thisver+")...",1);
      page = visit_url(threadhost+thread);
      find_ver = create_matcher("<b>"+soft+" (.+?)</b>",page);
      zv[prop].vdate = today_to_string();
      if (!find_ver.find()) { 
         vprint("Unable to load current version info.",-1);
         map_to_file(zv,"zversions.txt");
         return "";
      } w=1;
      zv[prop].ver = find_ver.group(1);
      map_to_file(zv,"zversions.txt");
   }
   if (sameornewer(thisver,zv[prop].ver)) { vprint("Running "+soft+" version: "+thisver+" (current)","gray",w); return ""; }
   if (svn_exists(prop)) { return check_version(soft, prop, thread); }
   string msg = "<big><font color=red><b>New Version of "+soft+" Available: "+zv[prop].ver+"</b></font></big>"+
      "<br><a href='"+threadhost+thread+"' target='_blank'><u>Upgrade from "+thisver+" to "+zv[prop].ver+" here!</u></a><br>";
   find_ver = create_matcher("\\[requires revision (.+?)\\]",page);
   if (find_ver.find() && find_ver.group(1).to_int() > get_revision())
      msg += " (Note: you will also need to <a href='https://ci.kolmafia.us/' target='_blank'>update mafia to r"+find_ver.group(1)+" or higher</a> to use this update.)";
   vprint_html(msg,1);
   return "<div class='versioninfo'>"+msg+"</div>";
}

// loads specified map file either from disk or from Map Manager if an updated map is available (checks once daily)
boolean load_current_map(string fname, aggregate dest) {
   file_to_map(fname+".txt",dest);
   string key = "map_"+fname+".txt";
   if (count(zv) == 0) file_to_map("zversions.txt",zv);
   if (zv[key].vdate == today_to_string() && count(dest) > 0) return true;
   zv[key].vdate = today_to_string();
   string rem = visit_url("https://zachbardon.com/mafiatools/autoupdate.php?f="+fname+"&act=getver", false);
   if (rem == "" || rem.length() > 150) {       // also check length to make sure we didn't get something else
      map_to_file(zv,"zversions.txt");
      if (count(dest) > 0) return vprint("Unable to access Map Manager.  Using local copy and will not check again today.","olive",3);
      return vprint("There was a problem accessing the Map Manager.",-3);
   }
   if (zv[key].ver == rem && count(dest) > 0) {
      map_to_file(zv,"zversions.txt");
      return vprint("You have the latest "+fname+".txt.  Will not check again today.",3);
   }
   vprint("Updating "+fname+".txt "+(count(dest) > 0 ? "from '"+zv[key].ver+"' " : "")+"to '"+rem+"'...",1);
   if (!file_to_map("https://zachbardon.com/mafiatools/autoupdate.php?f="+fname+"&act=getmap",dest) || count(dest) == 0)
      return vprint("Error loading "+fname+".txt from the Map Manager.","red",-1);
   zv[key].ver = rem;
   map_to_file(dest,fname+".txt");
   map_to_file(zv,"zversions.txt");
   return vprint("..."+fname+".txt updated.",1);
}


//                                 Script Settings

/*
vars_<myname>
    [name] => value

vars_documentation
    [source,name] => documentation  (source can include "user" and "global" for mafia properties)

vars_defaults
    [name] => record
        type
        default value
        maker
*/

record singlesettingdefault {
   string type;         // int, string, boolean, etc. (later can be "list of X" or filter/mask)
   string val;          // default value as initialized in setvar
   string init;         // use date for now to form groups; ideally establishing script
};
static {
   singlesettingdefault[string] vardefaults;  // stores all script setting default values
   file_to_map("vars_defaults.txt",vardefaults);
}
if (count(vardefaults) == 0) file_to_map("vars_defaults.txt",vardefaults);
file_to_map("vars_"+replace_string(my_name()," ","_")+".txt",vars);
foreach key,rec in vardefaults if (!(vars contains key)) vars[key] = rec.val;   // Add all defaults back into vars[], not just those initialized by setvar.  WILL REMOVE LATER!

// writes local vars map
boolean updatevars() {
   string[string] newvars;
   foreach pref,val in vars {    // won't need this later
      if (vardefaults contains pref && val == vardefaults[pref].val) continue;
      newvars[pref] = val;
   }
   return map_to_file(newvars,"vars_"+replace_string(my_name()," ","_")+".txt");
}

void setvar(string varname,string dfault,string type) {
   varname = replace_string(varname," ","_");
   if (vars contains varname) {
      if (vars[varname] != normalized(vars[varname],type)) {
         vprint("ZLib setting "+varname+" normalized: '"+vars[varname]+"' => '"+normalized(vars[varname],type)+"'","purple",4);
         vars[varname] = normalized(vars[varname],type);
         updatevars();
      }
   }
   if (!(vardefaults contains varname) || vardefaults[varname].val != dfault || vardefaults[varname].type != type) {
      vardefaults[varname].type = type;
      vardefaults[varname].val = dfault;
      vardefaults[varname].init = today_to_string()+" "+time_to_string();
      vprint("New default value for ZLib "+type+" setting: "+varname+" => "+dfault,"purple",4);
      map_to_file(vardefaults,"vars_defaults.txt");
   }
}

void setvar(string varname,string dfault)  {  setvar(varname,dfault,"string");  }
void setvar(string varname,boolean dfault) {  setvar(varname,to_string(dfault),"boolean");  }
void setvar(string varname,bounty dfault)  {  setvar(varname,to_string(dfault),"bounty");  }
void setvar(string varname,class dfault)   {  setvar(varname,to_string(dfault),"class");  }
void setvar(string varname,coinmaster dfault){setvar(varname,to_string(dfault),"coinmaster");  }
void setvar(string varname,effect dfault)  {  setvar(varname,to_string(dfault),"effect");  }
void setvar(string varname,element dfault) {  setvar(varname,to_string(dfault),"element");  }
void setvar(string varname,familiar dfault){  setvar(varname,to_string(dfault),"familiar");  }
void setvar(string varname,float dfault)   {  setvar(varname,to_string(dfault),"float");  }
void setvar(string varname,int dfault)     {  setvar(varname,to_string(dfault),"int");  }
void setvar(string varname,item dfault)    {  setvar(varname,to_string(dfault),"item");  }
void setvar(string varname,location dfault){  setvar(varname,to_string(dfault),"location");  }
void setvar(string varname,monster dfault) {  setvar(varname,to_string(dfault),"monster");  }
void setvar(string varname,phylum dfault)  {  setvar(varname,to_string(dfault),"phylum");  }
void setvar(string varname,servant dfault) {  setvar(varname,to_string(dfault),"servant");  }
void setvar(string varname,skill dfault)   {  setvar(varname,to_string(dfault),"skill");  }
void setvar(string varname,stat dfault)    {  setvar(varname,to_string(dfault),"stat");  }
void setvar(string varname,thrall dfault)  {  setvar(varname,to_string(dfault),"thrall");  }
void setvar(string varname,vykea dfault)   {  setvar(varname,to_string(dfault),"vykea");  }

string getvar(string varname) {   // this should replace using a direct vars[] lookup
   if (vars contains varname) return vars[varname];
   if (vardefaults contains varname) return vardefaults[varname].val;
   print("Attempt to access missing script setting '"+varname+"'!","purple");
   return "";
}


//                           ADVENTURING ROUTINES

// determine if something is path-safe
boolean be_good(string johnny) {
   switch (my_path()) {
      case "Bees Hate You": if (johnny.to_lower_case().index_of("b") > -1) return false; break;
      case "Trendy": if (!is_trendy(johnny)) return false; break;
      case "G-Lover": if (johnny.to_lower_case().index_of("g") == -1) return false; break;
   }
   return is_unrestricted(johnny);
}
boolean be_good(item johnny) {
   switch (my_path()) {
      case "Bees Hate You": if (johnny.to_lower_case().index_of("b") > -1) return false; break;
      case "Trendy": if (!is_trendy(johnny)) return false; break;
      case "Avatar of Boris": if (johnny == $item[trusty]) return true;
      case "Way of the Surprising Fist": if ($slots[weapon,off-hand] contains johnny.to_slot()) return false; break;
      case "KOLHS": if (johnny.inebriety > 0 && !contains_text(johnny.notes, "KOLHS")) return false; break;
      case "Zombie Slayer": if (johnny.fullness > 0 && !contains_text(johnny.notes, "Zombie Slayer")) return false; break;
      case "License to Adventure": if (johnny.inebriety > 0 && johnny.image != "martini.gif") return false; break;
      case "G-Lover": if (johnny.to_slot() != $slot[none] || $items[beehive, star chart, Cobb's Knob map] contains johnny) break;
         if (johnny.to_lower_case().index_of("g") == -1) return false; break;
      case "Path of the Plumber": if (johnny.minhp > 0 && johnny.maxmp <= 0 && johnny.seller != $coinmaster[Mushroom District Item Shop]) return false; break;
      case "You, Robot": if (johnny.string_modifier("Skill") != "") return false; break;  // can't use skill books
   }
   if (johnny == $item[grimstone mask] && my_class().to_int() > 6) return false;
   if (class_modifier(johnny,"Class") != $class[none] && class_modifier(johnny,"Class") != my_class()) return false;
   if (johnny.inebriety + johnny.fullness > 0 && johnny.notes == "Vampyre") return my_path() == "Dark Gyffte";
   return is_unrestricted(johnny);
}
boolean be_good(familiar johnny) {
   switch (my_path()) {
      case "Trendy": if (!is_trendy(johnny)) return false; break;
      case "Avatar of Boris":
      case "Avatar of Jarlsberg":
      case "Avatar of Sneaky Pete":
      case "Actually Ed the Undying":
      case "License to Adventure":
      case "Pocket Familiars": 
      case "Dark Gyffte":
      case "Quantum Terrarium": return false;
      case "G-Lover": if (johnny.to_lower_case().index_of("g") == -1) return false; break;
      case "You, Robot": if (get_property("youRobotTop") != "2") return false; break;
   }
   return is_unrestricted(johnny);
}
boolean be_good(skill johnny) {
   switch (my_path()) {
      case "Trendy": if (!is_trendy(johnny)) return false; break;
      case "G-Lover": if (!johnny.passive && johnny.to_lower_case().index_of("g") == -1) return false; break;
   }
   return is_unrestricted(johnny);
}

// check a quest property, e.g. qprop("questL11MacGuffin >= step3")
boolean qprop(string test) {
   if (!test.contains_text(" ")) return get_property(test) == "finished";
   string[int] tbits = split_string(test," ");
   if (count(tbits) != 3) return vprint("'"+test+"' not valid parameter for qprop().  Syntax is '<property> <relational operator> <value>'",-3);
   if (get_property(tbits[0]) == "") return vprint("'"+tbits[0]+"' is not a valid quest property.",-9);
   int numerize(string progress) {
      if (is_integer(progress)) return progress.to_int();
      switch (progress) {
         case "unstarted": return -1;
         case "started": return 0;
         case "finished": return 999;
      }
      return excise(progress,"step","").to_int();
   }
   switch (tbits[1]) {
      case "==": case "=": return numerize(get_property(tbits[0])) == numerize(tbits[2]);
      case "!=": case "<>": return numerize(get_property(tbits[0])) != numerize(tbits[2]);
      case ">": return numerize(get_property(tbits[0])) > numerize(tbits[2]);
      case "=>": case ">=": return numerize(get_property(tbits[0])) >= numerize(tbits[2]);
      case "<": return numerize(get_property(tbits[0])) < numerize(tbits[2]);
      case "=<": case "<=": return numerize(get_property(tbits[0])) <= numerize(tbits[2]);
   } return vprint("'"+tbits[1]+"' is not a valid relational operator.", -3);
}

// check the mall price of an item
// expirydays: optional, default 0. the number of days historical_price is valid, after which mall_price is used
// combatsafe: optional, default false.  specify as true to override expirydays and force the function to use only historical price
int mall_val(item it, float expirydays, boolean combatsafe) {
   if (!is_tradeable(it)) return 0;
   if (historical_price(it) > 0 && (combatsafe || expirydays > 99 || historical_age(it) < expirydays)) return historical_price(it);
   return combatsafe ? 0 : mall_price(it);
}
int mall_val(item it, float expirydays) { return mall_val(it,expirydays,false); }
int mall_val(item it, boolean combatsafe) { return mall_val(it,0,combatsafe); }

// returns the result of mall_val, or autosell if mall_val isn't more than mallmin
int sell_val(item it, float expirydays, boolean combatsafe) {
   int mall = mall_val(it,expirydays,combatsafe);
   if (mall > max(100,2*autosell_price(it))) return mall;
   return autosell_price(it);
}
int sell_val(item it, float expirydays) { return sell_val(it,expirydays,false); }
int sell_val(item it, boolean combatsafe) { return sell_val(it,0,combatsafe); }
int sell_val(item it) { return sell_val(it,0,false); }

// returns how many of an item you have ONLY in inventory and equipped
int have_item(string tolookup) {
   return item_amount(to_item(tolookup)) + equipped_amount(to_item(tolookup));
}

// returns whether you've pulled the given item today
boolean pulled_today(item which) {
   return list_contains(get_property("_roninStoragePulls"), which.to_int(), ",");
}

// returns the brain dropped by a given monster
item braindrop(monster patient) {
   if (my_path() != "Zombie Slayer" || $phyla[bug,constellation,elemental,construct,plant,slime] contains patient.phylum) return $item[none];
   if (index_of(patient.image,"hunter") == 0) return $item[hunter brain];
   if ($monsters[Boss Bat, Baron von Ratsworth, Knob Goblin King, giant skeelton, huge ghuol, conjoined zmombie, gargantulihc, Bonerdagon, 
      Groar, Dr. Awkward, Lord Spookyraven, Protector Spectre, The Big Wisniewski, Guy Made Of Bees] contains patient) return $item[boss brain];
   if (to_string(patient).contains_text("Ed the Undying")) return $item[none];
   if (monster_attack(patient) + monster_level_adjustment() >= 100) return $item[good brain];
   if (monster_attack(patient) + monster_level_adjustment() > 50) return $item[decent brain];
   return $item[crappy brain];
}
// returns the amount of ka dropped by a given monster
float kadrop(monster m) {
   if (my_class() != $class[ed the undying]) return 0;
   float res;
   switch (m.phylum) {
      case $phylum[dude]: case $phylum[hippy]: case $phylum[hobo]: case $phylum[pirate]: res = 1;
      case $phylum[beast]: case $phylum[bug]: case $phylum[elf]: case $phylum[fish]: case $phylum[goblin]: case $phylum[humanoid]: 
      case $phylum[mer-kin]: case $phylum[orc]: case $phylum[penguin]: case $phylum[elemental]: res += 1;
         if (res > 0 && my_servant() == $servant[priest] && $servant[priest].level >= 14) res += 1; break;
      case $phylum[undead]: if (have_equipped($item[the crown of ed the undying])) return 0.2;   // total guess
   }
   return res;
}

// returns true if the given stat is a goal (from setting "level X" or "X <stat>" as a condition)
boolean is_goal(stat gstat) {
   foreach i,g in get_goals() if (create_matcher("\\d+ "+to_lower_case(gstat),g).find()) return true;
//   if (my_primestat() == gstat && my_level() < 13) return true;
   return false;
}

float[item,item] pieces;
if (numeric_modifier("Generated:_spec","Buffed Muscle") == 0) cli_execute("whatif quiet");
// returns what fraction of an item (ancestor) a given ingredient (child) is
float isxpartof(item child, item ancestor) {
   if (pieces[child] contains ancestor) return pieces[child,ancestor];
   item get_parent(item child, item ancestor, int level) {
      int[item] unit = get_ingredients(ancestor);
      if (unit contains child) return ancestor;
      foreach i in unit {
         if (level > 5) return $item[none];  // avoid infinite loop
         item it = get_parent(child,i,level+1);
         if (it != $item[none]) return it;
      }
      return $item[none];
   }
   boolean[item] lineage;
   repeat {
      child = get_parent(child,ancestor,0);
      if (child != $item[none]) lineage[child] = true;
       else {
          pieces[child,ancestor] = 0;
          return 0;
       }
   } until (child == ancestor);
   int count;
   foreach i in lineage foreach j,k in get_ingredients(i) count += k;
   count += 1 - count(lineage);
   pieces[child,ancestor] = 1.0/count;
   return 1.0/count;
}

static {
   float [item,item] useforitems;
   load_current_map("use_for_items", useforitems);
}
float has_goal(item whatsit) {                   // chance of getting a goal from an item
   if (!goal_exists("item")) return 0;
   float has_goal(item whatsit,int level) {
     if (whatsit == $item[none]) return 0;
     if (is_goal(whatsit)) return 1.0;
     if (count(useforitems) == 0) {
        vprint("Unable to load file \"use_for_items.txt\".",-3); return 0;
     }
     if (level > 5) return 0;  // avoid infinite recursion
     float res;
     if (useforitems contains whatsit) foreach key,perc in useforitems[whatsit]
        if (has_goal(key,level+1) > 0) res = max(res,perc);
     return res;
   }
   float tot;
   foreach i,s in get_goals() {
      matcher numthings = create_matcher("\\d+ (.*)",s);
      if (!numthings.find()) continue;
      item testor = numthings.group(1).to_item();
      if (testor == whatsit || !is_goal(testor)) continue;
      tot += isxpartof(whatsit,testor);
   }
   return tot + has_goal(whatsit,0);
}
float has_goal(monster m, boolean usespec) {                      // chance of getting a goal from a monster
   float res, temp;
   foreach num,rec in item_drops_array(m) {
      temp = has_goal(rec.drop);
      if (temp == 0) continue;
      switch (rec.type) {
         case "b": res += temp; continue;
         case "p": if (my_primestat() == $stat[moxie] || have_effect($effect[Form of...Bird!]) > 0)
            res += temp*minmax(max(rec.rate,0.001)*((usespec ? numeric_modifier("Generated:_spec","Pickpocket Chance") :
                   numeric_modifier("Pickpocket Chance"))+100)/100.0,0,100)/100.0; continue;
         case "c": if (item_type(rec.drop) == "shirt" && !have_skill($skill[torso awareness])) continue;
            if (item_type(rec.drop) == "pasta guardian" && my_class() != $class[pastamancer]) continue;  // skip these for non-PMs
            if (m == $monster[pygmy witch accountant] && contains_text(rec.drop.to_string(),"McClusky") &&            
               item_amount(rec.drop) + item_amount($item[McClusky file (page 5)]) + item_amount($item[McClusky file (complete)]) > 0) continue;
            if (rec.drop == $item[bunch of square grapes] && my_level() < 11) continue;  // grapes drop at 11
            if (rec.drop == $item[beer lens] && get_property("_beerLensDrops").to_int() > 2) continue;   // can only get 3 a day
         case "":                                // TODO: pp chance
         case "n": res += temp*minmax(max(rec.rate,0.001)*((usespec ? numeric_modifier("Generated:_spec","Item Drop") :
                          numeric_modifier("Item Drop"))+100)/100.0,0,100)/100.0; continue;
         case "0": res += .001; continue;
      }
   }
   if (is_goal(braindrop(m))) switch (braindrop(m)) {       // include brains for Zombie Slayers
      case $item[hunter brain]: break; case $item[boss brain]: res += 1; break;
      default: res += minmax((have_skill($skill[skullcracker]) ? 0.6 : 0.3)*((usespec ? numeric_modifier("Generated:_spec","Item Drop") :
         numeric_modifier("Item Drop"))+100)/100.0,0,100)/100.0;
   }
   if (is_goal($item[ka coin])) res += kadrop(m);
   return res;
}
float has_goal(monster m) { return has_goal(m,false); }
float has_goal(location l, boolean usespec) {                     // chance of getting a goal from a location (-noncombats)
   float res;
   float[monster] rates = appearance_rates(l);
   if (rates[$monster[none]] == 100) return 0;
   float cradj = rates[$monster[none]] == -1 ? 0 :
      minmax(usespec ? numeric_modifier("Generated:_spec","Combat Rate") : numeric_modifier("Combat Rate"), -rates[$monster[none]], rates[$monster[none]]);
   foreach m,r in rates if (r <= 0 || m == $monster[none]) remove rates[m];
//   int countadj = count(rates) + 3*to_int(rates contains to_monster(get_property("olfactedMonster")));
   foreach m,r in rates
      res += has_goal(m,usespec)*max(r + cradj/count(rates),0)/100.0;
   return res;
}
float has_goal(location l) { return has_goal(l,false); }

// gets n (-existing) of cond, either by purchasing, pulling from Hangk's, or
// adventuring at the given location.  also works with most non-item goals, like choiceadvs, insults, stats, etc
boolean obtain(int n, string cond, location locale, string filter) {
   if (cond == "level") cli_execute("conditions clear; conditions set "+cond+" "+n);
   else if ($strings[choiceadv, autostop, arena flyer ml, pirate insult, factoid, muscle, mysticality, moxie] contains cond || to_item(cond) == $item[none])
      cli_execute("conditions clear; conditions set "+n+" "+cond);
   else {
      if (retrieve_item(n, to_item(cond))) return vprint("You have "+n+" "+cond+", no adventuring necessary.",5);
      if (storage_amount(to_item(cond)) > 0) switch (pulls_remaining()) {
         case 0: break;
         case -1: take_storage(n-have_item(cond),to_item(cond)); break;
         default: if (!pulled_today(to_item(cond))) take_storage(1,to_item(cond));
      }
      if (have_item(cond) >= n) return vprint("You have taken your needed items from storage.",5);
      if (count(get_goals()) > 0) cli_execute("conditions clear");
      add_item_condition(n - have_item(cond), to_item(cond));
   }
   if (count(get_goals()) == 0) return vprint("No goals left after setting '"+rnum(n)+" "+cond+"' as goals; no adventuring necessary.",5);
   set_location(locale);
   if (length(filter) > 0) { if (adventure(my_adventures(), locale, filter)) return vprint("Out of adventures.",-1); }
    else if (adventure(my_adventures(), locale)) return vprint("Out of adventures.",-1);
   if ($strings[choiceadv, autostop, arena flyer ml, pirate insult] contains cond || to_item(cond) == $item[none]) return (my_adventures() > 0);
   return (have_item(cond) >= n);
}
boolean obtain(int n, string cond, location locale) { return obtain(n, cond, locale, ""); }

// gets (if purchase) and uses n doodads if possible
// otherwise, uses as many as you have up to n
boolean use_upto(int n, item doodad, boolean purchase) {
   if (my_sign() == "Way of the Surprising Fist") purchase = false;
   if (doodad == $item[deodorant] && item_amount($item[chunk of rock salt]) > item_amount(doodad)) doodad = $item[chunk of rock salt];
   if (!be_good(doodad)) return vprint("Refusing to use a '"+doodad+"' since it is disallowed in "+my_path()+".",-4);
   if (item_amount(doodad) >= n || (purchase && retrieve_item(n, doodad))) return use(n, doodad);
   if (item_amount(doodad) == 0) return false;
   return use(item_amount(doodad), doodad);
}

// attempts to resist the given element
boolean resist(element req, boolean reallydoit) {
   vprint("Checking resistance to "+req+"...",2);
   if (req == $element[none]) return true;
   if (elemental_resistance(req) >= 10) return vprint("You can already resist "+req+".",5);
  // first, try catchall buffs (more expensive but least likely to screw someone up)
   foreach s in $skills[astral shell, elemental saucesphere, elemental obliviousness] if (have_skill(s) && (
       (!reallydoit && my_mp() >= mp_cost(s)) ||
       (reallydoit && use_skill(1,s)))) return vprint("Resistance achieved via "+s+".",5);
  // next, try resistance from gear
   int[item] mgear = get_inventory();
   string rtype = to_string(req) + " resistance";
   vprint("Searching items for "+rtype+"...",3);
   foreach doodad in mgear {
      if (to_slot(doodad) != $slot[none] && numeric_modifier(doodad,rtype) >= 1.0 && can_equip(doodad)) {
         vprint("Resistance-granting item found: "+doodad,3);
         if (reallydoit) {
            equip(to_slot(doodad) == $slot[acc1] ? $slot[acc3] : to_slot(doodad),doodad);
            if (elemental_resistance(req) < 10) return vprint("Unable to equip your "+doodad+".",-5);
         }
         return vprint("Resistance achieved via gear.",5);
      }
   }
  // last, try resistance from the parrot
   if (have_familiar($familiar[exotic parrot]) && be_good($familiar[exotic parrot])) {
       int necessary_parrot_weight(element which) {
          switch (which) {
             case $element[hot]: return 1;      case $element[cold]: return 5;
             case $element[spooky]: return 9;   case $element[stench]: return 13;
             case $element[sleaze]: return 17;
          }
          return 0;
       }
       int possible_parrot_weight() {
          int result = familiar_weight($familiar[exotic parrot]);
          if (familiar_equipped_equipment($familiar[exotic parrot]) == $item[cracker] || item_amount($item[cracker]) > 0)
             result += 15;
          return result + numeric_modifier("Familiar Weight") - numeric_modifier(equipped_item($slot[familiar]),"Familiar Weight");
       }
       if (possible_parrot_weight() >= necessary_parrot_weight(req)) {
          vprint("Your parrot is able to resist "+req+".",3);
          if (reallydoit) {
             use_familiar($familiar[exotic parrot]);
             if (familiar_equipped_equipment($familiar[exotic parrot]) != $item[cracker] && item_amount($item[cracker]) > 0)
                equip($item[cracker]);
          }
          return vprint("Resistance achieved via parrot.",5);
       }
   }
   return vprint("Unable to resist "+req+"!",-2);
}

// returns the value of your buffed defense stat, factoring in Hero of the Half-Shell
int my_defstat(boolean usespec) {
   int mox = usespec ? numeric_modifier("Generated:_spec","Buffed Moxie") : my_buffedstat($stat[moxie]);
   if (have_skill($skill[hero of the half-shell]) && item_type(equipped_item($slot[off-hand])) == "shield")
      return max(usespec ? numeric_modifier("Generated:_spec","Buffed Muscle") : my_buffedstat($stat[muscle]),mox);
   return mox;
}
int my_defstat() { return my_defstat(false); }

// returns safe moxie for a given location (includes +ML and MCD, skips bosses)
int get_safemox(location wear) {
   int high;
   foreach m,r in appearance_rates(wear) {
      switch(m) {
         case $monster[hulking construct]: continue;
         case $monster[The Clownlord Beelzebozo]: if (get_property("choiceAdventure151") != "1") continue; break;
         case $monster[conjoined zmombie]: if (get_property("choiceAdventure154") != "1") continue; break;
         case $monster[giant skeelton]: if (get_property("choiceAdventure156") != "1") continue; break;
         case $monster[gargantulihc]: if (get_property("choiceAdventure158") != "1") continue; break;
         case $monster[huge ghuol]: if (get_property("choiceAdventure160") != "1") continue; break;
         default: if (r <= 0) continue;
      }
      high = max(monster_attack(m),high);
   }
   if (high == 0 || high == monster_level_adjustment()) return 0;
   if (wear == $location[barrrney's barrr] && item_amount($item[the big book of pirate insults]) > 0) high += 0.3*my_defstat();
   if (my_path() == "Heavy Rains" && wear.water_level != my_location().water_level) high += 10*(wear.water_level - my_location().water_level);
   return high + 7 - current_mcd();
}

// if 'automcd' is true, adjusts your MCD for the specified safemox based on your threshold
boolean auto_mcd(int safemox) {
   if (!to_boolean(getvar("automcd")) || my_ascensions() < 1 || in_bad_moon() || ($strings[Grey You, Kingdom of Exploathing] contains my_path())) return true;
   if ((knoll_available() && !retrieve_item(1,$item[detuned radio])) ||
     (gnomads_available() && item_amount($item[bitchin' meatcar]) + item_amount($item[desert bus pass]) +
      item_amount($item[pumpkin carriage]) == 0))
      return vprint("MCD: unavailable","olive",5);
   if (safemox == 0) {
      vprint("MCD: Using your 'unknown_ml' value ("+getvar("unknown_ml")+").","olive",2);
      safemox = to_int(getvar("unknown_ml")) + 7;
   }
   int adj = minmax(my_defstat() + to_int(getvar("threshold")) - safemox, 0, 10+canadia_available().to_int());
   if (current_mcd() == adj) return true;
   else return (vprint("MCD: adjusting to "+adj+"...","olive",2) && change_mcd(adj));
}
boolean auto_mcd(monster mob) {                               // automcd for a single monster
   if (monster_attack(mob) == monster_level_adjustment()) return auto_mcd(0);
   return auto_mcd(monster_attack(mob) + 7 - current_mcd());
}
boolean auto_mcd(location place) {                            // automcd for locations
   if ($locations[the typical tavern cellar, oil peak, the boss bat's lair, throne room, haert of the cyrpt, the slime tube] contains my_location())
      return vprint("MCD: Sensitive location, not adjusting.","olive",4);
   if (my_location() == $location[the smut orc logging camp] && qprop("questL09Topping < 1")) return current_mcd() == 0 || change_mcd(0);
   if (count(get_monsters(place)) == 0) return vprint("MCD: "+place+" has no known combats.","olive",4);
   return auto_mcd(get_safemox(place));
}

// returns your heaviest familiar of a given type (currently possible: items, meat, produce, stat, dodge, delevel, restore hp, restore mp, elemental damage, water)
familiar best_fam(string ftype) {
   if (to_familiar(getvar("is_100_run")) != $familiar[none]) return to_familiar(getvar("is_100_run"));
   familiar result = $familiar[none];
   string[familiar] fams;
   if (!load_current_map("bestfamiliars",fams)) return result;
   vprint("Finding best familiar of type \""+ftype+"\"...",3);
   int bestweight, beststrength;
   int famstrength(string alltypes, string targettype) {
     matcher m = create_matcher("(\\w+)\\s+(\\d+)", alltypes);          // thanks for the regex Jason
     while (m.find()) if (m.group(1) == targettype) return to_int(m.group(2));
     return 0;
   }
   foreach i in fams {
     if (!have_familiar(i) || !be_good(i) || !contains_text(fams[i],ftype)) continue;
     if (familiar_weight(i) > bestweight || (familiar_weight(i) == bestweight && famstrength(fams[i],ftype) > beststrength)) {
        if (result == $familiar[none]) vprint("First match for type \""+ftype+"\": "+i,3);
         else vprint(i+" is better than "+result+"...",3);
        result = i;
        bestweight = familiar_weight(i);
        beststrength = famstrength(fams[i],ftype);
     }
   }
   if (result == $familiar[none]) {
      vprint("No familiar found matching type \""+ftype+"\"",-3);
      return my_familiar();
   }
   return result;
}

//                              KMAIL FUNCTIONS

record kmessage {
   int id;                   // message id
   string type;              // possible values observed thus far: normal, giftshop
   int fromid;               // sender's playerid (0 for npc's)
   int azunixtime;           // KoL server's unix timestamp
   string message;           // message (including items/meat)
   int[item] items;          // items included in the message
   int meat;                 // meat included in the message
   string fromname;          // sender's playername
   string localtime;         // your local time according to your KoL account, human-readable string
};
kmessage[int] mail;
void load_kmail(string calledby) { // loads all of your inbox (up to 100) into the global "mail"
   mail.clear();                   // optionally, specify your script in "calledby"
  // heeheehee's JSON matcher
   matcher k = create_matcher('"id":"(\\d+)","type":"(.+?)","fromid":"(-?\\d+)","azunixtime":"(\\d+)","message":"(.+?)","fromname":"(.+?)","localtime":"(.+?)"'
      ,visit_url("api.php?pwd&what=kmail&count=100&for="+url_encode(calledby)));
   int n;
   while (k.find()) {
      n = count(mail);
      mail[n].id = to_int(k.group(1));
      mail[n].type = k.group(2);
      mail[n].fromid = to_int(k.group(3));
      mail[n].azunixtime = to_int(k.group(4));
      matcher mbits = create_matcher("(.*?)\\<center\\>(.+?)$",k.group(5).replace_string("\\'","'"));
      if (mbits.find()) {
         mail[n].meat = extract_meat(mbits.group(2));
         mail[n].items = extract_items(mbits.group(2));
         mail[n].message = mbits.group(0);
      } else mail[n].message = k.group(5);
      mail[n].fromname = k.group(6);
      mail[n].localtime = replace_string(k.group(7),"\\","");
   }
}
void load_kmail() { load_kmail("ZLib-powered-script"); }

void process_kmail(string functionname) {   // calls a function designed to parse a kmail.  It must accept a single kmessage parameter,
   if (count(mail) == 0) load_kmail();      // and return a boolean -- true if it wants the kmail to be deleted afterwards
   boolean[int] processed;
   foreach i,m in mail if (call boolean functionname(m)) {
      processed[m.id] = true;
      remove mail[i];
   }
  // delete successfully processed mail
   if (count(processed) > 0) {
      vprint("Deleting processed mail...",2);
      string del = "messages.php?the_action=delete&box=Inbox&pwd";
      foreach k in processed del += "&sel"+k+"=on";
      del = visit_url(del);
      if (contains_text(del,count(processed)+" message"+(count(processed) > 1 ? "s" : "")+" deleted."))
         vprint(count(processed)+" message"+(count(processed) > 1 ? "s" : "")+" deleted.",2);
       else vprint("There was a problem deleting the processed mail.  Check your inbox.",0);
   }
}

boolean send_gift(string to, string message, int meat, int[item] goodies, string insidenote) {
 // parse items into query string
   string itemstring;
   int j = 0;
   int[item] extra;
   foreach i in goodies {
      if (!is_tradeable(i) && !is_giftable(i)) continue;
      j += 1;
      if (j < 3)
        itemstring += "&howmany"+j+"="+goodies[i]+"&whichitem"+j+"="+to_int(i);
       else extra[i] = goodies[i];
   }
   int pnum = minmax(count(goodies),1,2);
   int shipping = 50*pnum;
   if (my_meat() < meat+shipping) return vprint("Not enough meat to send the package.",-2);
  // send gift
   string url = visit_url("town_sendgift.php?pwd=&towho="+to+"&note="+message+"&insidenote="+insidenote+"&whichpackage="+pnum+"&fromwhere=0&sendmeat="+meat+"&action=Yep."+itemstring);
   if (!contains_text(url,"Package sent.")) return vprint("The message didn't send for some reason.",-2);
   if (count(extra) > 0) return send_gift(to,message,0,extra,insidenote);
   return true;
}
boolean send_gift(string to, string message, int meat, int[item] goodies) { return send_gift(to,message,meat,goodies,""); }

boolean kmail(string to, string message, int meat, int[item] goodies, string insidenote) {
   if (meat > my_meat()) return vprint("You don't have "+meat+" meat.",-2);
  // parse items into query strings
   string itemstring;
   int j = 0;
   string[int] itemstrings;
   foreach i in goodies {
      if (!is_tradeable(i) && !is_giftable(i)) continue;
      j += 1;
      itemstring += "&howmany"+j+"="+goodies[i]+"&whichitem"+j+"="+to_int(i);
      if (j > 10) {
         itemstrings[count(itemstrings)] = itemstring;
         itemstring = '';
         j = 0;
      }
   }
   if (itemstring != "") itemstrings[count(itemstrings)] = itemstring;
   if (count(itemstrings) == 0) itemstrings[0] = "";
    else vprint(count(goodies)+" item types split into "+count(itemstrings)+" separate kmails.",5);
  // send message(s)
   foreach q in itemstrings {
      string url = visit_url("sendmessage.php?pwd=&action=send&towho="+to+"&message="+message+"&savecopy=on&sendmeat="+(q == 0 ? meat : 0)+itemstrings[q]);
      if (contains_text(url,"That player cannot receive Meat or items"))
        return (vprint("That player cannot receive stuff, sending gift instead...",4) && send_gift(to, message, meat, goodies, insidenote));
      if (!contains_text(url,"Message sent.")) return vprint("The message didn't send for some reason.",-2);
   }
   return true;
}
boolean kmail(string to, string message, int meat, int[item] goodies) { return kmail(to,message,meat,goodies,""); }
boolean kmail(string to, string message, int meat) { int[item] nothing; return kmail(to,message,meat,nothing,""); }
boolean kmail(kmessage km) { return kmail(km.fromname, km.message, km.meat, km.items, ""); }  // 'fromname' is your intended recipient in this case

// NOTE: This code sets variable defaults in vars_defaults.txt.  To edit these variables for your character,
// you can view ("zlib vars") or edit ("zlib <settingname> = <value>") values in the CLI, or run Prefref Plus.
setvar("verbosity",3);
setvar("automcd",true);
setvar("threshold",4);
setvar("unknown_ml",170);
setvar("is_100_run",$familiar[none]);
setvar("defaultoutfit","current");
check_version("ZLib","zlib",2072);

void main(string setval) {
   if (!setval.contains_text(" = ")) {
      boolean[string] matches;
      foreach key in vardefaults if (setval == "vars" || setval == "" || key.to_lower_case().contains_text(setval.to_lower_case()) || getvar(key).to_lower_case().contains_text(setval.to_lower_case()))
         matches[key] = false;
      foreach key in vars {
         if (matches contains key) continue;
         if (setval == "vars" || setval == "" || key.to_lower_case().contains_text(setval.to_lower_case()) || getvar(key).to_lower_case().contains_text(setval.to_lower_case()))
            matches[key] = true;
      }
      if (count(matches) == 0) { print("(No settings or values matched your input text. Type \"zlib vars\" to see all.)","gray"); return; }
      print_html("<b>Copy/paste/modify/enter any of the following lines in the CLI to edit settings:</b><br>");
      foreach key,val in matches print("zlib "+key+" = "+getvar(key), val ? "#1f5c1d" : "#666");
      return;
   }
   string n = excise(setval,""," = ");
   if (!(vars contains n) && !(vardefaults contains n)) { print("No setting named '"+n+"' exists.","olive"); return; }
   string v = excise(setval," = ","");
   print("Previous value of "+n+": "+vars[n]);
   if (vars[n] == v) return;
   if (n == "threshold") {
      if (v == "up") v = to_string(to_int(getvar("threshold"))+1);
      if (v == "down") v = to_string(to_int(getvar("threshold"))-1);
   }
   if (vardefaults contains n) v = normalized(v,vardefaults[n].type);
   vars[n] = v;
   if (updatevars()) print("Changed to "+v+".");
}
