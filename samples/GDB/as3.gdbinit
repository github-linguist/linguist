# -*- gdb-script -*-
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Interactive commands to help inspecting ActionScript execution state while debugging in gdb.
#
# To use this file, type from the gdb prompt: 
# source $(hg root)/utils/as3.gdbinit
# or include in your .gdbinit.
#
# See gdb help for individual commands.
#
# In case of undefined symbols, try running the gdb 'where' command before aswhere.
#
# Typical use case:
# (gdb) run
# ^C
# (gdb) where
# (gdb) aswhere
# (gdb) asframe 4    # select 4th AS3 frame from the top
# (gdb) aslocal      # print locals
# (gdb) aslocal 2    # print 2nd local as a C++ value
# $42 = ....
# (gdb) print *$42   # dereference  $42, assuming it's a pointer (to a ScriptObject or String)
#
# Note that the gdb scripting language is rather limited.
#

define aswhere
  set var $fcount = avmplus::AvmCore::getActiveCore()->debugger()->frameCount()
  set var $i = 0
  while ($i < $fcount) 
    #  set var $frame = avmplus::AvmCore::getActiveCore()->debugger()->frameAt($k)
    #  call (void)avmplus::Debugger::printFrame($k)
    asprintframe $i
    set var $i = $i + 1
  end
end

document aswhere
Print backtrace of all the ActionScript stack frames.
May not work in all contexts (notably inside MMgc).
May not work properly until gdb 'where' is called at least once.
end

set var $_asframe_selected=-1

define asprintframe 
  set var $frame = avmplus::AvmCore::getActiveCore()->debugger()->frameAt($arg0)
  if ($frame == 0)
    echo no frame\n
  else
    aspstring avmplus::Debugger::methodNameAt($frame)
    echo (
    set var $vcount = avmplus::Debugger::autoVarCount($frame, avmplus::Debugger::AUTO_ARGUMENT)
    set var $j = 0
    while ($j < $vcount) 
      set var $argname = avmplus::Debugger::autoVarName($frame, $j, avmplus::Debugger::AUTO_ARGUMENT)
      aspstring $argname 
      echo =
      set var $_atom = avmplus::Debugger::autoAtomAt($frame, $j, avmplus::Debugger::AUTO_ARGUMENT)
      call (void)avmplus::Debugger::printAtom($_atom)
      set var $j = $j + 1
      if ($j != $vcount) 
        echo ,
      end
    end
    echo )
    echo \n
  end
end


define asframe 
  if ($argc != 0)
    set var $_asframe_selected=$arg0
  end
  if ($_asframe_selected >= 0)
    asprintframe $_asframe_selected
  else
    echo no frame\n
  end
end

document asframe
Select and print an ActionScript stack frame.
With no argument, print the selected AS stack frame.
An argument specifies the number of the stack frame to select.
end


define aspstring
  call (void)avmplus::Debugger::printString($arg0)
end

document aspstring
Print the AS3 string.
end

# not pretty, but there's no switch
define aslocal
  set var $frame = avmplus::AvmCore::getActiveCore()->debugger()->frameAt($_asframe_selected)
  if ($frame == 0)
    echo no frame\n
  else
    if $argc == 0
      set var $lcount = avmplus::Debugger::autoVarCount($frame, avmplus::Debugger::AUTO_LOCAL)
      set var $k = 0
      while ($k < $lcount) 
        set var $lname = avmplus::Debugger::autoVarName($frame, $k, avmplus::Debugger::AUTO_LOCAL)
        output $k
        echo :\ \  
        aspstring $lname
        echo :\ \ 
        aslocal $k
        set var $k = $k + 1
      end
    else
      set var $_last_type=avmplus::Debugger::autoAtomKindAt($frame, $arg0, avmplus::Debugger::AUTO_LOCAL) 
      if ($_last_type == 0)
        echo unknown\n
      end
      if ($_last_type == 1)
        call avmplus::Debugger::autoVarAsObject($frame, $arg0, avmplus::Debugger::AUTO_LOCAL) 
      end
      if ($_last_type == 2)
        call avmplus::Debugger::autoVarAsString($frame, $arg0, avmplus::Debugger::AUTO_LOCAL) 
      end
      if ($_last_type == 3)
        ecno namespace (unfinished)\n
      end
      if ($_last_type == 4)
        echo undefined\n
      end
      if ($_last_type == 5)
        call avmplus::Debugger::autoVarAsBoolean($frame, $arg0, avmplus::Debugger::AUTO_LOCAL) 
      end
      if ($_last_type == 6)
        call avmplus::Debugger::autoVarAsInteger($frame, $arg0, avmplus::Debugger::AUTO_LOCAL) 
      end
      if ($_last_type == 7)
        call avmplus::Debugger::autoVarAsDouble($frame, $arg0, avmplus::Debugger::AUTO_LOCAL) 
      end
    end
  end
end

document aslocal 
Print local variables of the currently selected AS stack frame, if debuging information present.
Information may be incorrect if not at a debugger safepoint.
With no argument, print all the local variables.
With a numeric argument print the specific local variable (gdb will store value in history 
for further manipulation).
end

define asarg
  set var $frame = avmplus::AvmCore::getActiveCore()->debugger()->frameAt($_asframe_selected)
  if ($frame == 0)
    echo no frame\n
  else
    if $argc == 0
      set var $acount = avmplus::Debugger::autoVarCount($frame, avmplus::Debugger::AUTO_ARGUMENT)
      set var $k = 0
      while ($k < $acount) 
        set var $name = avmplus::Debugger::autoVarName($frame, $k, avmplus::Debugger::AUTO_ARGUMENT)
        output $k
        echo :\ \  
        aspstring $name
        echo :\ \ 
        asarg $k
        set var $k = $k + 1
      end
    else
      set var $_last_type=avmplus::Debugger::autoAtomKindAt($frame, $arg0, avmplus::Debugger::AUTO_ARGUMENT) 
      if ($_last_type == 0)
        echo unknown\n
      end
      if ($_last_type == 1)
        print avmplus::Debugger::autoVarAsObject($frame, $arg0, avmplus::Debugger::AUTO_ARGUMENT) 
      end
      if ($_last_type == 2)
        print avmplus::Debugger::autoVarAsString($frame, $arg0, avmplus::Debugger::AUTO_ARGUMENT) 
      end
      if ($_last_type == 3)
        echo namespace (unfinished)\n
      end
      if ($_last_type == 4)
        echo undefined\n
      end
      if ($_last_type == 5)
        print avmplus::Debugger::autoVarAsBoolean($frame, $arg0, avmplus::Debugger::AUTO_ARGUMENT) 
      end
      if ($_last_type == 6)
        print avmplus::Debugger::autoVarAsInteger($frame, $arg0, avmplus::Debugger::AUTO_ARGUMENT) 
      end
      if ($_last_type == 7)
        print avmplus::Debugger::autoVarAsDouble($frame, $arg0, avmplus::Debugger::AUTO_ARGUMENT) 
      end
    end
  end
end

document asarg 
Print arguments of the currently selected AS stack frame.
If no debugging information is available, argument names will not be printed.
With no argument, print all the arguments.
With a numeric argument print the specific argument (gdb will store value in history 
for further manipulation).
end


define asthis
  set var $frame = avmplus::AvmCore::getActiveCore()->debugger()->frameAt($_asframe_selected)
  if ($frame == 0)
    echo no frame\n
  else
    print avmplus::Debugger::autoVarAsObject($frame, 0, avmplus::Debugger::AUTO_THIS) 
  end
end

document asthis
Print the receiver of the currently selected frame.
end

define asmixon
  print avmshell::DebugCLI::debuggerInterruptOnEnter
  print avmshell::DebugCLI::debuggerInterruptOnEnter = true
end

document asmixon
  turn on stepping.
  Execution will return to gdb propmpt after asstep* instructions.
  Requires debugging symbols in .abcs
end

define asstepout
  call  avmplus::AvmCore::getActiveCore()->debugger()->stepOut()
  continue
  asprintframe 0
end

define asstepinto
  output avmplus::AvmCore::getActiveCore()->debugger()->stepInto()
  continue
  asprintframe 0
end

define asstepover
  output avmplus::AvmCore::getActiveCore()->debugger()->stepOver()
  continue
  asprintframe 0
end


define asprint
  aspstring $arg0->traits()->name()
  echo \n
end
