// From: https://github.com/microsoft/Ironclad/blob/master/ironfleet/src/Dafny/Distributed/Protocol/Lock/Node.i.dfy

include "../../Protocol/Lock/Node.i.dfy"
include "Message.i.dfy"
include "../Common/UdpClient.i.dfy"
include "../../Common/Logic/Option.i.dfy"
include "PacketParsing.i.dfy"
include "../Common/SeqIsUniqueDef.i.dfy"

module Impl_Node_i {
import opened Protocol_Node_i
import opened Message_i
import opened Common__UdpClient_i
import opened Logic__Option_i
import opened PacketParsing_i 
import opened Common__SeqIsUniqueDef_i

datatype CNode = CNode(held:bool, epoch:uint64, my_index:uint64, config:Config)

predicate ValidConfig(c:Config)
{
    0 < |c| < 0x1_0000_0000_0000_0000
 && (forall e :: e in c ==> EndPointIsValidIPV4(e))
 && SeqIsUnique(c)
}

predicate ValidConfigIndex(c:Config, index:uint64)
{
    0 <= int(index) < |c|
}

predicate CNodeValid(c:CNode)
{
       ValidConfig(c.config)
    && ValidConfigIndex(c.config, c.my_index)
}

function AbstractifyCNode(n:CNode) : Node
{
    Node(n.held, int(n.epoch), int(n.my_index), n.config)
}

method NodeInitImpl(my_index:uint64, config:Config) returns (node:CNode)
    requires 0 < |config| < 0x1_0000_0000_0000_0000;
    requires 0 <= int(my_index) < |config|;
    requires ValidConfig(config);
    ensures CNodeValid(node);
    ensures NodeInit(AbstractifyCNode(node), int(my_index), config);
    ensures node.my_index == my_index;
    ensures node.config == config;
{
    node := CNode(my_index == 0, if my_index == 0 then 1 else 0, my_index, config);
    if node.held {
        print "I start holding the lock\n";
    }
}

method NodeGrantImpl(s:CNode) returns (s':CNode, packet:Option<CLockPacket>, ghost ios:seq<LockIo>)
    requires CNodeValid(s);
    ensures  NodeGrant(AbstractifyCNode(s), AbstractifyCNode(s'), ios);
    ensures  s'.my_index == s.my_index && s'.config == s.config;
    ensures  |ios| == 0 || |ios| == 1;
    ensures  packet.Some? ==> |ios| == 1 && ios[0].LIoOpSend? 
                           && ios[0].s == AbstractifyCLockPacket(packet.v);
    ensures    OptionCLockPacketValid(packet) 
            && (packet.Some? ==> packet.v.src == s.config[s.my_index]); 
    ensures  packet.None? ==> ios == [] && s' == s;
    ensures  CNodeValid(s');
{
    if s.held && s.epoch < 0xFFFF_FFFF_FFFF_FFFF {
        s' := s[held := false];
        var dst_index := (s.my_index + 1) % uint64(|s.config|);
        packet := Some(LPacket(s.config[dst_index], s.config[s.my_index], CTransfer(s.epoch + 1)));
        ios := [LIoOpSend(AbstractifyCLockPacket(packet.v))];
        print "I grant the lock ", s.epoch, "\n";
    } else {
        s' := s;
        ios := [];
        packet := None();
    }
}

method NodeAcceptImpl(s:CNode, transfer_packet:CLockPacket) 
    returns (s':CNode, locked_packet:Option<CLockPacket>, ghost ios:seq<LockIo>)
    requires CNodeValid(s);
    ensures  NodeAccept(AbstractifyCNode(s), AbstractifyCNode(s'), ios);
    ensures  s'.my_index == s.my_index && s'.config == s.config;
    ensures  |ios| == 1 || |ios| == 2;
    ensures  locked_packet.None? ==> |ios| == 1 && ios[0].LIoOpReceive? 
                                  && ios[0].r == AbstractifyCLockPacket(transfer_packet);
    ensures  locked_packet.Some? ==> |ios| == 2 
                                  && ios == [LIoOpReceive(AbstractifyCLockPacket(transfer_packet)), 
                                             LIoOpSend(AbstractifyCLockPacket(locked_packet.v))];
    ensures    OptionCLockPacketValid(locked_packet) 
            && (locked_packet.Some? ==> locked_packet.v.src == s.config[s.my_index]); 
    ensures  CNodeValid(s');
{
    ios := [LIoOpReceive(AbstractifyCLockPacket(transfer_packet))];

    if    !s.held 
       && transfer_packet.src in s.config
       && transfer_packet.msg.CTransfer? 
       && transfer_packet.msg.transfer_epoch > s.epoch {
        s' := s[held := true][epoch := transfer_packet.msg.transfer_epoch];
        locked_packet := Some(LPacket(transfer_packet.src, 
                                      s.config[s.my_index],
                                      CLocked(transfer_packet.msg.transfer_epoch)));
        ios := ios + [LIoOpSend(AbstractifyCLockPacket(locked_packet.v))];
        print "I hold the lock!\n";
    } else  {
        s' := s;
        locked_packet := None();
    }
}

}