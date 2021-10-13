// From: https://github.com/microsoft/Ironclad/blob/b2afa7e3eb65285fd3dba4ffa9dc10c6da593727/ironfleet/src/Dafny/Distributed/Common/Native/Io.s.dfy

include "NativeTypes.s.dfy"
include "../Framework/Environment.s.dfy"

module Native__Io_s {
import opened Native__NativeTypes_s
import opened Environment_s

class HostEnvironment
{
    ghost var constants:HostConstants;
    ghost var ok:OkState;
    ghost var now:NowState;
    ghost var udp:UdpState;
    ghost var files:FileSystemState;

    predicate Valid()
        reads this;
    {
           constants != null
        && ok != null
        && now != null
        && udp != null
        && files != null
    }
}

//////////////////////////////////////////////////////////////////////////////
// Per-host constants
//////////////////////////////////////////////////////////////////////////////

class HostConstants
{
    constructor{:axiom} () requires false;

    function{:axiom} LocalAddress():seq<byte> reads this; // REVIEW: Do we need this anymore?  We now allow different UdpClients to have different addresses anyway.
    function{:axiom} CommandLineArgs():seq<seq<uint16>> reads this; // result of C# System.Environment.GetCommandLineArgs(); argument 0 is name of executable

    static method{:axiom} NumCommandLineArgs(ghost env:HostEnvironment) returns(n:uint32)
        requires env != null && env.Valid();
        ensures  int(n) == |env.constants.CommandLineArgs()|;

    static method{:axiom} GetCommandLineArg(i:uint64, ghost env:HostEnvironment) returns(arg:array<uint16>)
        requires env != null && env.Valid();
        requires 0 <= int(i) < |env.constants.CommandLineArgs()|;
        ensures  arg != null;
        ensures  fresh(arg);
        ensures  arg[..] == env.constants.CommandLineArgs()[i];
}

//////////////////////////////////////////////////////////////////////////////
// Failure
//////////////////////////////////////////////////////////////////////////////

// not failed; IO operations only allowed when ok() == true
class OkState
{
    constructor{:axiom} () requires false;
    function{:axiom} ok():bool reads this;
}

//////////////////////////////////////////////////////////////////////////////
// Time
//////////////////////////////////////////////////////////////////////////////

// current local real time in milliseconds
// (current actually means "current as of last waiting operation or call to GetTime")
class NowState
{
    constructor{:axiom} () requires false;
    function{:axiom} now():int reads this;
}

// maximum assumed time taken by any non-waiting code (in milliseconds)
function{:axiom} realTimeBound():int
predicate AdvanceTime(oldTime:int, newTime:int, delay:int) { oldTime <= newTime < oldTime + delay + realTimeBound() }

class Time
{
    static method{:axiom} GetTime(ghost env:HostEnvironment) returns(t:uint64)
        requires env != null && env.Valid();
        modifies env.now; // To avoid contradiction, GetTime must advance time, because successive calls to GetTime can return different values
        modifies env.udp;
        ensures  int(t) == env.now.now();
        ensures  AdvanceTime(old(env.now.now()), env.now.now(), 0);
        ensures  env.udp.history() == old(env.udp.history()) + [LIoOpReadClock(int(t))];

    // Used for performance debugging
    static method{:axiom} GetDebugTimeTicks() returns(t:uint64)
    static method{:axiom} RecordTiming(name:array<char>, time:uint64)
}

//////////////////////////////////////////////////////////////////////////////
// Networking
//////////////////////////////////////////////////////////////////////////////

datatype EndPoint = EndPoint(addr:seq<byte>, port:uint16)
    // UdpPacket_ctor has silly name to ferret out backwards calls
type UdpPacket = LPacket<EndPoint, seq<byte>>
type UdpEvent = LIoOp<EndPoint, seq<byte>>

class UdpState
{
    constructor{:axiom} () requires false;
    function{:axiom} history():seq<UdpEvent> reads this;
}

class IPEndPoint
{
    ghost var env:HostEnvironment;
    function{:axiom} Address():seq<byte> reads this;
    function{:axiom} Port():uint16 reads this;
    function EP():EndPoint reads this; { EndPoint(Address(), Port()) }
    constructor{:axiom} () requires false;

    method{:axiom} GetAddress() returns(addr:array<byte>)
        ensures  addr != null;
        ensures  fresh(addr);
        ensures  addr[..] == Address();
        ensures  addr.Length == 4;      // Encoding current IPv4 assumption

    function method{:axiom} GetPort():uint16 reads this;
        ensures  GetPort() == Port();

    static method{:axiom} Construct(ipAddress:array<byte>, port:uint16, ghost env:HostEnvironment) returns(ok:bool, ep:IPEndPoint)
        requires env != null && env.Valid();
        requires ipAddress != null;
        modifies env.ok;
        ensures  env.ok.ok() == ok;
        ensures  ok ==> ep != null && fresh(ep) && ep.env == env && ep.Address() == ipAddress[..] && ep.Port() == port;
}

function MaxPacketSize() : int { 65507 }

class UdpClient
{
    ghost var env:HostEnvironment;
    function{:axiom} LocalEndPoint():EndPoint reads this;
    function{:axiom} IsOpen():bool reads this;
    constructor{:axiom} () requires false;

    static method{:axiom} Construct(localEP:IPEndPoint, ghost env:HostEnvironment)
        returns(ok:bool, udp:UdpClient)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires localEP != null;
        modifies env.ok;
        ensures  env.ok.ok() == ok;
        ensures  ok ==>
                       udp != null
                    && fresh(udp)
                    && udp.env == env
                    && udp.IsOpen()
                    && udp.LocalEndPoint() == localEP.EP();

    method{:axiom} Close() returns(ok:bool)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires this.IsOpen();
        modifies this;
        modifies env.ok;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;

    method{:axiom} Receive(timeLimit:int32) returns(ok:bool, timedOut:bool, remote:IPEndPoint, buffer:array<byte>)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires IsOpen();
        requires timeLimit >= 0;
        requires int(timeLimit) * 1000 < 0x80000000; // only needed when the underlying implementation uses Socket.Poll instead of Task.Wait
        modifies this;
        modifies env.ok;
        modifies env.now;
        modifies env.udp;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;
        ensures  AdvanceTime(old(env.now.now()), env.now.now(), int(timeLimit));
        ensures  LocalEndPoint() == old(LocalEndPoint());
        ensures  ok ==> IsOpen();
        ensures  ok ==> timedOut  ==> env.udp.history() == old(env.udp.history()) + [LIoOpTimeoutReceive()];
        ensures  ok ==> !timedOut ==>
            remote != null
            && buffer != null
            && fresh(remote)
            && fresh(buffer)
            && env.udp.history() == old(env.udp.history()) +
                [LIoOpReceive(LPacket(LocalEndPoint(), remote.EP(), buffer[..]))]
            && buffer.Length < 0x1_0000_0000_0000_0000;

    method{:axiom} Send(remote:IPEndPoint, buffer:array<byte>) returns(ok:bool)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires IsOpen();
        requires remote != null;
        requires buffer != null;
        requires buffer.Length <= MaxPacketSize();
        modifies this;
        modifies env.ok;
        modifies env.udp;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;
        ensures  LocalEndPoint() == old(LocalEndPoint());
        ensures  ok ==> IsOpen();
        ensures  ok ==> env.udp.history() == old(env.udp.history()) + [LIoOpSend(LPacket(remote.EP(), LocalEndPoint(), buffer[..]))];

}

// jonh temporarily neutered this because the opaque type can't be compiled
class FileSystemState
{
}

class MutableSet<T(==)>
{
    static function method {:axiom} SetOf(s:MutableSet<T>) : set<T>
        reads s;

    static method {:axiom} EmptySet() returns (s:MutableSet<T>)
        ensures SetOf(s) == {};
        ensures fresh(s); 

    constructor{:axiom} () requires false;

    method {:axiom} Size() returns (size:int) 
        ensures size == |SetOf(this)|;

    method {:axiom} SizeModest() returns (size:uint64) 
        requires |SetOf(this)| < 0x1_0000_0000_0000_0000;
        ensures int(size) == |SetOf(this)|;

    method {:axiom} Contains(x:T) returns (contains:bool)
        ensures contains == (x in SetOf(this));

    method {:axiom} Add(x:T) 
        modifies this;
        ensures SetOf(this) == old(SetOf(this)) + {x};

    method {:axiom} AddSet(s:MutableSet<T>) 
        modifies this;
        ensures SetOf(this) == old(SetOf(this)) + old(SetOf(s));

    method {:axiom} TransferSet(s:MutableSet<T>) 
        modifies this;
        modifies s;
        ensures SetOf(this) == old(SetOf(s));
        ensures SetOf(s) == {};

    method {:axiom} Remove(x:T) 
        modifies this;
        ensures SetOf(this) == old(SetOf(this)) - {x};

    method {:axiom} RemoveAll()
        modifies this;
        ensures SetOf(this) == {};
}

class MutableMap<K(==),V>
{
    static function method {:axiom} MapOf(m:MutableMap<K,V>) : map<K,V> 
        reads m;

    static method {:axiom} EmptyMap() returns (m:MutableMap<K,V>)
        ensures MapOf(m) == map [];
        ensures fresh(m); 

    static method {:axiom} FromMap(dafny_map:map<K,V>) returns (m:MutableMap<K,V>)
        ensures MapOf(m) == dafny_map;
        ensures fresh(m); 

    constructor{:axiom} () requires false;

    function method {:axiom} Size() : int
        reads this;
        ensures this.Size() == |MapOf(this)|;

    method {:axiom} SizeModest() returns (size:uint64) 
        requires |MapOf(this)| < 0x1_0000_0000_0000_0000;
        ensures int(size) == |MapOf(this)|;

    method {:axiom} Contains(key:K) returns (contains:bool)
        ensures contains == (key in MapOf(this));

    method {:axiom} TryGetValue(key:K) returns (contains:bool, val:V)
        ensures contains == (key in MapOf(this));
        ensures contains ==> val == MapOf(this)[key];

    method {:axiom} Set(key:K, val:V) 
        modifies this;
        ensures MapOf(this) == old(MapOf(this))[key := val];

    method {:axiom} Remove(key:K) 
        modifies this;
        ensures MapOf(this) == map k | k != key && k in old(MapOf(this)) :: old(MapOf(this))[k];
}

// Leverage .NET's ability to perform copies faster than one element at a time
class Arrays
{
    static method{:axiom} CopySeqIntoArray<A>(src:seq<A>, srcIndex:uint64, dst:array<A>, dstIndex:uint64, len:uint64)
        requires dst != null;
        requires int(srcIndex) + int(len) <= |src|;
        requires int(dstIndex) + int(len) <= dst.Length;
        modifies dst;
        ensures  forall i :: 0 <= i < dst.Length ==> dst[i] == (
                    if int(dstIndex) <= i < int(dstIndex) + int(len)
                    then src[i - int(dstIndex) + int(srcIndex)]
                    else old(dst[..])[i]);
        ensures  forall i :: int(srcIndex) <= i < int(srcIndex) + int(len) ==>
                    src[i] == dst[i - int(srcIndex) + int(dstIndex)];
}


/*
//////////////////////////////////////////////////////////////////////////////
// File System
//////////////////////////////////////////////////////////////////////////////

type FileSystem

datatype FileOp =
  FileRead(fileReadOffset:int, fileReadBytes:seq<byte>)
| FileWrite(fileWriteOffset:int, fileWriteBytes:seq<byte>)
| FileFlush

class FileSystemState
{
    constructor{:axiom} () requires false;
    function{:axiom} state():FileSystem reads this;
}

function{:axiom} FileOpRequires(fs:FileSystem, fileName:string, op:FileOp):bool
function{:axiom} FileOpEnsures(fsOld:FileSystem, fsNew:FileSystem, fileName:string, op:FileOp):bool

class FileStream
{
    ghost var env:HostEnvironment;
    function{:axiom} Name():string reads this;
    function{:axiom} IsOpen():bool reads this;
    constructor{:axiom} () requires false;

    static method{:axiom} Open(name:array<char>, ghost env:HostEnvironment)
        returns(ok:bool, f:FileStream)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires name != null;
        modifies env.ok;
        ensures  env.ok.ok() == ok;
        ensures  ok ==> f != null && fresh(f) && f.env == env && f.IsOpen() && f.Name() == name[..];

    method{:axiom} Close() returns(ok:bool)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires IsOpen();
        modifies this;
        modifies env.ok;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;

    method{:axiom} Read(fileOffset:nat32, buffer:array<byte>, start:int32, end:int32) returns(ok:bool)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires IsOpen();
        requires buffer != null;
        requires 0 <= int(start) <= int(end) <= buffer.Length;
        requires FileOpRequires(env.files.state(), Name(), FileRead(int(fileOffset), buffer[start..end]));
        modifies this;
        modifies env.ok;
        modifies env.files;
        modifies buffer;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;
        ensures  Name() == old(Name());
        ensures  forall i:int :: 0 <= i < buffer.Length && !(int(start) <= i < int(end)) ==> buffer[i] == old(buffer[i]);
        ensures  ok ==> IsOpen();
        ensures  ok ==> FileOpEnsures(old(env.files.state()), env.files.state(), Name(), FileRead(int(fileOffset), buffer[start..end]));

    method{:axiom} Write(fileOffset:nat32, buffer:array<byte>, start:int32, end:int32) returns(ok:bool)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires IsOpen();
        requires buffer != null;
        requires 0 <= int(start) <= int(end) <= buffer.Length;
        requires FileOpRequires(env.files.state(), Name(), FileWrite(int(fileOffset), buffer[start..end]));
        modifies this;
        modifies env.ok;
        modifies env.files;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;
        ensures  Name() == old(Name());
        ensures  ok ==> IsOpen();
        ensures  ok ==> FileOpEnsures(old(env.files.state()), env.files.state(), Name(), FileWrite(int(fileOffset), buffer[start..end]));

    method{:axiom} Flush() returns(ok:bool)
        requires env != null && env.Valid();
        requires env.ok.ok();
        requires IsOpen();
        requires FileOpRequires(env.files.state(), Name(), FileFlush);
        modifies this;
        modifies env.ok;
        modifies env.files;
        ensures  env == old(env);
        ensures  env.ok.ok() == ok;
        ensures  Name() == old(Name());
        ensures  ok ==> IsOpen();
        ensures  ok ==> FileOpEnsures(old(env.files.state()), env.files.state(), Name(), FileFlush);
}

*/

} 