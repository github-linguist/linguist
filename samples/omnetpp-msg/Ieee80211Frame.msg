//
// Copyright (C) 2006 OpenSim Ltd.
// Copyright (C) 2001 Eric Wu and Steve Woon, Monash University, Melbourne, Australia
//
// SPDX-License-Identifier: LGPL-3.0-or-later
//

import inet.common.INETDefs;
import inet.common.packet.chunk.Chunk;
import inet.linklayer.common.FcsMode;
import inet.linklayer.common.MacAddress;

cplusplus {{
#include "inet/common/BitVector.h"
#include "inet/linklayer/ieee80211/mac/common/Ieee80211Defs.h"
}}

namespace inet;

class BitVector
{
    @existingClass;
    @opaque;
    @toString(.toString());
}

namespace inet::ieee80211;

class SequenceNumberCyclic
{
    @existingClass;
    @opaque;
}

cplusplus {{
const b LENGTH_RTS = b(160);    //bits
const b LENGTH_CTS = b(112);    //bits
const b LENGTH_ACK = b(112);    //bits
const b LENGTH_MGMT = B(28);    //bits
const b LENGTH_ADDBAREQ = LENGTH_MGMT + B(9); // mgmt length + action body length
const b LENGTH_ADDBARESP = LENGTH_MGMT + B(9); // mgmt length + action body length
const b LENGTH_DELBA = LENGTH_MGMT + B(6); // mgmt length + action body length
const b LENGTH_BASIC_BLOCKACK = B(16 + 2 + (2 + 128) + 4); // header + ba control + ba information + fcs
const b DATAFRAME_HEADER_MINLENGTH = B(2 + 2 + 3 * 6 + 2);    //bits without QoS, without Address4: 2 + 2 + 3*6(addresses) + 2
const b QOSCONTROL_PART_LENGTH = b(2 * 8);    // QoS Control field length (bits)
const short int MAX_NUM_FRAGMENTS = 16;
const b LENGTH_A_MSDU_SUBFRAME_HEADER = B(14); //48 + 48 + 16 bits
const b LENGTH_A_MPDU_SUBFRAME_HEADER = b(4 + 12 + 8 + 8);
}}

//
// 802.11 frame type constants (type+subtype), for the "type" field of
// Ieee80211FrameControl
//
enum Ieee80211FrameType
{
    // management:
    ST_ASSOCIATIONREQUEST = 0x00;
    ST_ASSOCIATIONRESPONSE = 0x01;
    ST_REASSOCIATIONREQUEST = 0x02;
    ST_REASSOCIATIONRESPONSE = 0x03;
    ST_PROBEREQUEST = 0x04;
    ST_PROBERESPONSE = 0x05;
    ST_BEACON = 0x08;
    ST_ATIM = 0x09;
    ST_DISASSOCIATION = 0x0a;
    ST_AUTHENTICATION = 0x0b;
    ST_DEAUTHENTICATION = 0x0c;
    ST_ACTION = 0x0d;
    ST_NOACKACTION = 0x0e;

    // control (CFEND/CFEND_CFACK omitted):
    ST_PSPOLL = 0x1a;
    ST_RTS = 0x1b;
    ST_CTS = 0x1c;
    ST_ACK = 0x1d;
    ST_BLOCKACK_REQ = 0x18;
    ST_BLOCKACK     = 0x19;

    // data (CFPOLL/CFACK subtypes omitted):
    ST_DATA = 0x20;
    ST_DATA_WITH_QOS = 0x28;
    //Feedback frame for multicast tramsmission
    ST_LBMS_REQUEST = 0x30;
    ST_LBMS_REPORT = 0x31;
}

//
// Ack policy values for the 802.11 QoS control field
//
enum AckPolicy
{
    NORMAL_ACK = 0;
    NO_ACK = 1;
    NO_EXPLICIT_ACK = 2;
    BLOCK_ACK = 3;
}

class Ieee80211MacTrailer extends FieldsChunk
{
    chunkLength = B(4);
    uint32_t fcs = 0;
    FcsMode fcsMode = FCS_MODE_UNDEFINED;
}

//
// The common part of 802.11 frames.
//
// NOTE:
// FCS value is not explicitly modeled, but it is included in the length.
// Frame control format fields not supported by this model are omitted:
// MoreFlag, PowerMgmt, MoreData, WEP, Order.
//
class Ieee80211MacHeader extends FieldsChunk
{
    chunkLength = LENGTH_ACK - B(4);
    Ieee80211FrameType type; // type and subtype
    bool toDS;
    bool fromDS;
    bool moreFragments;
    bool retry;
    bool powerMgmt = false; // unused
    bool moreData = false; // unused
    bool protectedFrame = false; // unused
    bool order = false; // unused
    simtime_t durationField = -1; // "duration" in the Duration/ID field (-1=no duration)
    short AID = -1;          // "id" (Association ID) in the Duration/ID field (-1=no ID)
    MacAddress receiverAddress; // aka address1 (RA)
    simtime_t MACArrive;    // FIXME remove it, technical data, used inside of MAC module
}

cplusplus(Ieee80211MacHeader) {{
    virtual uint8_t getProtocolVersion() const { return ((getType() & 0b11000000) >> 6); }
    virtual uint8_t getFrameType() const { return ((getType() & 0b00110000) >> 4); }
    virtual uint8_t getSubType() const { return (getType() & 0b00001111); }
    virtual void setType(uint8_t protocolVersion, uint8_t frameType, uint8_t subType) {
        setType(static_cast<inet::ieee80211::Ieee80211FrameType>((protocolVersion << 6) | (frameType << 4) | subType));
    }
}}

//
// Format of a 802.11 frame with address1 present, like ACK and CTS
//
class Ieee80211OneAddressHeader extends Ieee80211MacHeader
{
}

//
// Format of the 802.11 ACK frame
//
class Ieee80211AckFrame extends Ieee80211OneAddressHeader
{
    chunkLength = LENGTH_ACK - B(4);
    type = ST_ACK;
}

//
// Format of a 802.11 frame with address1 and address2 present
//
class Ieee80211TwoAddressHeader extends Ieee80211OneAddressHeader
{
    chunkLength = LENGTH_RTS - B(4);
    MacAddress transmitterAddress; // aka address2 (TA)
}

//
// Format of the 802.11 RTS frame
//
class Ieee80211RtsFrame extends Ieee80211TwoAddressHeader
{
    chunkLength = LENGTH_RTS - B(4);
    type = ST_RTS;
}

//
// Format of the 802.11 CTS frame
//
class Ieee80211CtsFrame extends Ieee80211OneAddressHeader
{
    chunkLength = LENGTH_CTS - B(4);
    type = ST_CTS;
}

//
// Common base class for 802.11 data and management frames
//
class Ieee80211DataOrMgmtHeader extends Ieee80211TwoAddressHeader
{
    chunkLength = LENGTH_MGMT - B(4);
    MacAddress address3;
    short fragmentNumber;
    SequenceNumberCyclic sequenceNumber;
}

//
// Format of the 802.11 data frame
//
class Ieee80211DataHeader extends Ieee80211DataOrMgmtHeader
{
//  The presence of the Address 4 field is determined by the setting of the To DS and From DS subfields of
//  the Frame Control field (see below). The QoS Control field is present when the QoS subfield of the Subtype
//  field is set to 1 (8.3.2.1 Data frame format)
//  FIXME chunkLength is not a constant value DATAFRAME_HEADER_MINLENGTH (24 bytes).
//  For example, with Address 4 and without QoS the correct length is 30 bytes.
//  Without Address 4 and QoS the correct value is indeed 24 bytes.
//  With Address 4 and QoS the correct value is indeed 32 bytes.
    chunkLength = DATAFRAME_HEADER_MINLENGTH;
    type = ST_DATA;         // or ST_DATA_WITH_QOS
    MacAddress address4;    // optional field, exists when toDS and fromDS are true
    AckPolicy ackPolicy = NORMAL_ACK;
    uint8_t tid;              // optional field, exists when type == ST_DATA_WITH_QOS
    bool aMsduPresent = false; // 8.2.4.5.9 A-MSDU Present subfield
}

//TODO this can be introduced when the SNAP header
//class Ieee80211QoSDataHeader extends Ieee80211DataHeader
//{
//    type = ST_DATA_WITH_QOS;
//    chunkLength = DATAFRAME_HEADER_MINLENGTH + QOSCONTROL_PART_LENGTH;
//    AckPolicy ackPolicy = NORMAL_ACK; //FIXME in real life this is part of the optional qos field above...
//    uint8_t tid;
//    // TODO other QoS control field parts
//}

//
// Base class for 802.11 management frames (subclasses will add frame body contents)
//
class Ieee80211MgmtHeader extends Ieee80211DataOrMgmtHeader
{
}

// 8.3.3.13 Action frame format -- mgmt frame with subtype action
class Ieee80211ActionFrame extends Ieee80211MgmtHeader
{
    type = ST_ACTION;
    short category;
    uint8_t blockAckAction;
    // TODO subclasses will add action subfield contents
    //WirelessNetworkManagementAction action; // action TODO remove it
}

class Ieee80211MsduSubframeHeader extends FieldsChunk
{
    chunkLength = LENGTH_A_MSDU_SUBFRAME_HEADER;
    MacAddress sa; // source addr.
    MacAddress da; // destination addr.
    int length;
}

class Ieee80211MpduSubframeHeader extends FieldsChunk
{
    chunkLength = LENGTH_A_MPDU_SUBFRAME_HEADER;
    int length;
}

//
// 8.5.5.2 ADDBA Request frame format
//
class Ieee80211AddbaRequest extends Ieee80211ActionFrame
{
    chunkLength = LENGTH_ADDBAREQ - B(4);
    category = 3; // category field set to 3 (representing Block Ack). (1 byte)
    blockAckAction = 0; // Block Ack Action field is set to 0 (representing ADDBA request). (1 byte)
    uint8_t dialogToken = 1; // Dialog Token field is set to a nonzero value chosen by the STA. (1 byte)

    // Block Ack Parameter Set field (2 bytes)
    bool aMsduSupported; // The A-MSDU Supported subfield determines whether an A-MSDU may be carried in a QoS data MPDU sent under this Block Ack agreement. (1 bit)
    bool blockAckPolicy; // The Block Ack Policy subfield is set to 1 for immediate Block Ack and 0 for delayed Block Ack. (1 bit)
    uint8_t tid; // The TID subfield contains the value of the TC or TS for which the BlockAck is being requested. (4 bits)
    uint16_t bufferSize; // The Buffer Size subfield indicates the number of buffers available for this particular TID. (10 bits)

    // Block Ack Timeout Value field (2 bytes)
    // The Block Ack Timeout Value field contains the duration, in TUs, after which the Block Ack setup is
    // terminated, if there are no frame exchanges (see 10.5.4) within this duration using this Block Ack
    // agreement. A value of 0 disables the timeout.
    simtime_t blockAckTimeoutValue;

    // Block Ack Starting Sequence Control (2 bytes)
    // The Starting Sequence Number subfield of the Block Ack Starting Sequence Control subfield contains the sequence number of the first MSDU for which this Basic
    // BlockAckReq frame is sent. The Fragment Number subfield is set to 0.
    uint8_t _fragmentNumber = 0; // FIXME DataOrMgmtFreme has a fragmentNumber (4 bits)
    SequenceNumberCyclic startingSequenceNumber; // the sequence number of the first MSDU (12 bits)
}

//
// 8.5.5.3 ADDBA Response frame format
//
class Ieee80211AddbaResponse extends Ieee80211ActionFrame
{
    chunkLength = LENGTH_ADDBARESP - B(4);
    category = 3; // Category field is set to 3 (representing Block Ack). (1 byte)
    blockAckAction = 1; // Block Ack Action field is set to 1 (representing ADDBA response). (1 byte)
    uint8_t dialogToken = 1; // The Dialog Token field value is copied from the corresponding received ADDBA Request frame. (1 byte)
    uint16_t statusCode; // The Status Code field is used in a response management frame to indicate the success or failure of a requested operation. (2 bytes)

    // Block Ack Parameter Set field (2 bytes)
    bool aMsduSupported; // The A-MSDU Supported subfield determines whether an A-MSDU may be carried in a QoS data MPDU sent under this Block Ack agreement. (1 bit)
    bool blockAckPolicy; // The Block Ack Policy subfield is set to 1 for immediate Block Ack and 0 for delayed Block Ack. (1 bit)
    uint8_t tid; // The TID subfield contains the value of the TC or TS for which the BlockAck is being requested. (4 bits)
    uint16_t bufferSize; // The Buffer Size subfield indicates the number of buffers available for this particular TID. (10 bits)

    // Block Ack Timeout Value field (2 bytes)
    // The Block Ack Timeout Value field contains the duration, in TUs, after which the Block Ack setup is
    // terminated, if there are no frame exchanges (see 10.5.4) within this duration using this Block Ack
    // agreement. A value of 0 disables the timeout.
    simtime_t blockAckTimeoutValue;
}

//
// 8.5.5.4 DELBA frame format
//
class Ieee80211Delba extends Ieee80211ActionFrame
{
    chunkLength = LENGTH_DELBA;
    category = 3; // Category field is set to 3 (representing DELBA). (1 byte)
    blockAckAction = 2; // Block Ack Action field is set to 2 (representing DELBA). (1 byte)

    // DELBA Parameter Set (2 bytes)
    uint16_t reserved = 0; // Reserved. (11 bits)
    bool initiator; // It is set to 1 to indicate the originator and is set to 0 to indicate the recipient.
                    // The Initiator subfield indicates if the originator or the recipient of the data is sending this frame. (1 bit)
    uint8_t tid; // The TID subfield indicates the TSID or the UP for which the Block Ack has been originally set up. (4 bits)

    // This Reason Code field is used to indicate the reason that an unsolicited notification management frame of
    // type Disassociation, Deauthentication, DELTS, DELBA, DLS Teardown, or Mesh Peering Close was
    // generated. (2 bytes)
    uint16_t reasonCode;
}

//
// 8.3.1.8 BlockAckReq frame format
//
class Ieee80211BlockAckReq extends Ieee80211TwoAddressHeader
{
    // The RA field of the BlockAck frame is the address of the recipient STA that requested the Block Ack.
    // The TA field is the address of the STA transmitting the BlockAck frame.
    type = ST_BLOCKACK_REQ;

    bool multiTid;
    bool compressedBitmap;
    bool barAckPolicy; // 0-Normal Acknowledgment, 1-No Acknowledgment.
    uint16_t reserved = 0;
}

class Ieee80211BasicBlockAckReq extends Ieee80211BlockAckReq
{
    chunkLength = B(38);

    int tidInfo; //  The TID_INFO subfield of the BAR Control field of the Basic BlockAckReq frame contains the TID for which a Basic BlockAck frame is requested.

    // The BAR Information field of the Basic BlockAckReq frame contains the Block Ack Starting Sequence
    // Control subfield, as shown in Figure 8-21. The Starting Sequence Number subfield of the Block Ack
    // Starting Sequence Control subfield contains the sequence number of the first MSDU for which this Basic
    // BlockAckReq frame is sent. The Fragment Number subfield is set to 0.
    int fragmentNumber = 0;
    SequenceNumberCyclic startingSequenceNumber;

    multiTid = 0;
    compressedBitmap = 0;
}

class Ieee80211CompressedBlockAckReq extends Ieee80211BlockAckReq
{
    chunkLength = B(38);

    int tidInfo; // The TID_INFO subfield of the BAR Control field of the Compressed BlockAckReq frame contains the TID for which a BlockAck frame is requested.
    // The BAR Information field of the Compressed BlockAckReq frame contains the Block Ack Starting
    // Sequence Control subfield, as shown in Figure 8-21. The Starting Sequence Number subfield of the Block
    // Ack Starting Sequence Control subfield contains the sequence number of the first MSDU or A-MSDU for
    // which this BlockAckReq frame is sent. The Fragment Number subfield of the Block Ack Starting Sequence
    // Control subfield is set to 0.

    int fragmentNumber = 0;
    SequenceNumberCyclic startingSequenceNumber;

    multiTid = 0;
    compressedBitmap = 1;
}

class Ieee80211MultiTidBlockAckReq extends Ieee80211BlockAckReq
{
    // chunkLength TODO
    // The BAR Information field of the Compressed BlockAckReq frame contains the Block Ack Starting
    // Sequence Control subfield, as shown in Figure 8-21. The Starting Sequence Number subfield of the Block
    // Ack Starting Sequence Control subfield contains the sequence number of the first MSDU or A-MSDU for
    // which this BlockAckReq frame is sent. The Fragment Number subfield of the Block Ack Starting Sequence
    // Control subfield is set to 0.

    multiTid = 1;
    compressedBitmap = 1;

    // TODO unimplemented
}

//
// 8.3.1.9 BlockAck frame format -- 410p.
//
class Ieee80211BlockAck extends Ieee80211TwoAddressHeader
{
    // The RA field of the BlockAck frame is the address of the recipient STA that requested the Block Ack.
    // The TA field is the address of the STA transmitting the BlockAck frame.
    type = ST_BLOCKACK;

    // BA Control
    bool blockAckPolicy; // Table 8-17—BA Ack Policy subfield: 0-Normal Acknowledgment, 1-No Acknowledgment.

    // Table 8-18—BlockAck frame variant encoding
    bool multiTid;
    bool compressedBitmap;
    uint16_t reserved = 0;
}

//
// 8.3.1.9.2 Basic BlockAck variant -- 411p.
//
class Ieee80211BasicBlockAck extends Ieee80211BlockAck
{
    chunkLength = LENGTH_BASIC_BLOCKACK - B(4);
    multiTid = 0;
    compressedBitmap = 0;

    // The BA Information field of the Basic BlockAck frame comprises the Block Ack Starting Sequence Control
    // subfield and the Block Ack Bitmap subfield, as shown in Figure 8-26.

    int fragmentNumber = 0; // ??
    SequenceNumberCyclic startingSequenceNumber;
    // The Block Ack Bitmap subfield is 128 octets in length and is used to indicate the received status of up to
    // 64 MSDUs. Bit position n of the Block Ack bitmap, if equal to 1, acknowledges receipt of an MPDU with an
    // MPDU sequence control value equal to (Block Ack Starting Sequence Control + n). Bit position n of the
    // Block Ack bitmap, if equal to 0, indicates that an MPDU with MPDU sequence control value equal to
    // (Block Ack Starting Sequence Control + n) has not been received. Each of the MPDU Sequence Control
    // field and Block Ack Starting Sequence Control subfield values are treated as a 16-bit unsigned integer. For
    // unused fragment numbers of an MSDU, the corresponding bits in the bitmap are set to 0.
    BitVector blockAckBitmap[64]; // 64 * 16 bitmatrix

    // The TID_INFO subfield of the BA Control field of the Basic BlockAck frame contains the TID for which
    // this BlockAck frame is sent.
    int tidInfo;
}

//
// 8.3.1.9.3 Compressed BlockAck variant
//
class Ieee80211CompressedBlockAck extends Ieee80211BlockAck
{
    // chunkLength TODO
    multiTid = 0;
    compressedBitmap = 1;

    // The BA Information field of the Compressed BlockAck frame comprises the Block Ack Starting Sequence
    // Control subfield and the Block Ack Bitmap subfield, as shown in Figure 8-27.

    int fragmentNumber = 0;
    SequenceNumberCyclic startingSequenceNumber;
    BitVector blockAckBitmap; // 64 bits

    // The TID_INFO subfield of the BA Control field of the Compressed BlockAck frame contains the TID for
    // which this BlockAck frame is sent.
    int tidInfo;
}

//
// 8.3.1.9.4 Multi-TID BlockAck variant
//
class Ieee80211MultiTidBlockAck extends Ieee80211BlockAck
{
    // chunkLength TODO
    multiTid = 1;
    compressedBitmap = 1;

    // TODO unimplemented
}
