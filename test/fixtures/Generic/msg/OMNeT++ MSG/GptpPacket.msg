//
// Copyright (C) 2020 OpenSim Ltd.
//
// SPDX-License-Identifier: LGPL-3.0-or-later
//

//
// @authors: Enkhtuvshin Janchivnyambuu
//           Henning Puttnies
//           Peter Danielis
//           University of Rostock, Germany
//

import inet.clock.common.ClockEvent;
import inet.clock.contract.ClockTime;
import inet.common.TagBase;
import inet.common.packet.chunk.Chunk;
import inet.common.packet.Packet;

cplusplus {{
#include "inet/common/Units.h"
#include "inet/clock/common/ClockEvent.h"

namespace inet {

using namespace inet::units::values;
const B GPTP_HEADER_SIZE = B(34);
const B GPTP_TLV_SIZE = B(4);
const B GPTP_FOLLOW_UP_INFORMATION_TLV_BODYSIZE = B(28);
const B GPTP_SYNC_1STEP_PACKET_SIZE = GPTP_HEADER_SIZE + B(10) + GPTP_TLV_SIZE + GPTP_FOLLOW_UP_INFORMATION_TLV_BODYSIZE;
const B GPTP_SYNC_2STEP_PACKET_SIZE = GPTP_HEADER_SIZE + B(10);
const B GPTP_FOLLOW_UP_PACKET_SIZE = GPTP_HEADER_SIZE + B(10) + GPTP_TLV_SIZE + GPTP_FOLLOW_UP_INFORMATION_TLV_BODYSIZE;
const B GPTP_PDELAY_REQ_PACKET_SIZE = GPTP_HEADER_SIZE + B(20);
const B GPTP_PDELAY_RESP_PACKET_SIZE = GPTP_HEADER_SIZE + B(20);
const B GPTP_PDELAY_RESP_FOLLOW_UP_PACKET_SIZE = GPTP_HEADER_SIZE + B(20);
}

}}

namespace inet;

@property[bit](type=int; usage=field; desc="define stored size in bits");

enum GptpNodeType
{
    MASTER_NODE = 11;
    BRIDGE_NODE = 12;
    SLAVE_NODE  = 13;
}

enum GptpPortType
{
    MASTER_PORT  = 2;
    SLAVE_PORT   = 1;
    PASSIVE_PORT = 0;
}

enum GptpMessageType
{
    GPTPTYPE_SYNC    = 0x0;
    GPTPTYPE_FOLLOW_UP = 0x8;
    GPTPTYPE_PDELAY_REQ = 0x2;
    GPTPTYPE_PDELAY_RESP = 0x3;
    GPTPTYPE_PDELAY_RESP_FOLLOW_UP = 0xA;
}

enum GptpSelfMsgKind {
    GPTP_SELF_REQ_ANSWER_KIND = 101;
    GPTP_SELF_MSG_SYNC = 103;
    GPTP_REQUEST_TO_SEND_SYNC = 104;
    GPTP_SELF_MSG_PDELAY_REQ = 105;
}

// ieee802.1AS-2020 10.6.2.2.8: flags (Octet2)
// Table 10-9—Values of flag bits
enum GptpFlags {
    alternateMasterFlag = 1; // Not used in this standard; transmitted as FALSE and ignored on reception
    twoStepFlag = 2; // For Sync messages:
                     //   a) For a one-step transmitting PTP Port (see 11.1.3 and 11.2.13.9), the value is FALSE.
                     //   b) For a two-step transmitting PTP Port, the value is TRUE.
                     // For Pdelay_Resp messages: The value is transmitted as TRUE and ignored on reception

    // TODO add more flags
}

enum GptpTlvType {
    GPTP_FOLLOW_UP_INFORMATION_TLV = 0x03;
}

//struct Timestamp
//{
//    UInteger48 seconds;
//    UInteger32 nanoseconds;
//};

//struct ExtendedTimestamp
//{
//    UInteger48 seconds;
//    UInteger48 fractionalNanoseconds;
//};

//typedef Octet8 ClockIdentity;

struct PortIdentity
{
    @packetData;
    uint64_t clockIdentity;
    uint16_t portNumber;
};

//struct ClockQuality
//{
//    UInteger8 clockClass;
//    Enumeration8 clockAccuracy;
//    UInteger16 offsetScaledLogVariance;
//};

message GptpReqAnswerEvent extends ClockEvent
{
    int portId;
    clocktime_t ingressTimestamp;
    PortIdentity sourcePortIdentity;
    uint16_t sequenceId;
}

// ieee802.1AS-2020 10.6.2.1, 11.4.2
class GptpBase extends FieldsChunk
{
    uint8_t majorSdoId @bit(4); // The value is specified in 8.1 for all transmitted PTP messages of a gPTP domain. The value is specified in
                                // 11.2.17 for all transmitted PTP messages of the Common Mean Link Delay Service. Any PTP message
                                // received for which the value is not one of the values specified in those subclauses shall be ignored.
    GptpMessageType messageType @bit(4);
    uint8_t minorVersionPTP @bit(4) = 1; // For transmitted messages, the value shall be 1 (see 7.5.4 and 13.3.2.5 of IEEE Std 1588-2019).
                                     // For received messages, the value is ignored.
    uint8_t versionPTP @bit(4) = 2; // For transmitted messages, the value shall be 2 (see 7.5.4 and 13.3.2.4 of IEEE Std 1588-2019). For received
                                // messages, if the value is not 2, the entire message shall be ignored.
    uint16_t messageLengthField @bit(16); // The value is the total number of octets that form the PTP message.
    uint8_t domainNumber @bit(8); // The domainNumber for Pdelay_Req, Pdelay_Resp, and Pdelay_Resp_Follow_Up messages shall be 0.
                                  // The domainNumber for all other PTP messages is as specified in 10.6.2.2.6.
    uint8_t minorSdoId @bit(8); // The value is specified in 8.1 for all transmitted PTP messages of a gPTP domain. The value is specified in
                                // 11.2.17 for all transmitted PTP messages of the Common Mean Link Delay Service. Any PTP message
                                // received for which the value is not one of the values specified in those subclauses shall be ignored.
                                // Currently unused in INET
    uint16_t flags @bit(16); // 10.6.2.2.8 flags (Octet2)
                             // The value of the bits of the array are defined in Table 10-9. For message types where the bit is not defined in
                             // Table 10-9, the value of the bit is set to FALSE.
                             // Use ~GptpFlags enums.
    clocktime_t correctionField @bit(64) = 0; // The correctionField is the value of the correction as specified in Table 11-6, measured in nanoseconds and
                                              // multiplied by 2^16. For example, 2.5 ns is represented as 0x0000000000028000.
    uint32_t messageTypeSpecific @bit(32); // The value of the messageTypeSpecific field varies, based on the value of the messageType field, as described in Table 10-10.
    PortIdentity sourcePortIdentity @bit(80); // The value is the PTP Port identity attribute (see 8.5.2) of the PTP Port that transmits the PTP message.
    uint16_t sequenceId @bit(16); // The sequenceId field is assigned as specified in 10.5.7.
    uint8_t controlField @bit(8) = 0; // The value is 0.
    uint8_t logMessageInterval @bit(8); // For Sync and Follow_Up messages, the value is the value of currentLogSyncInterval (see 10.2.5.4 and
                                        // 10.7.2.3). For Pdelay_Req messages, the value is the value of currentLogPdelayReqInterval. For
                                        // Pdelay_Resp and Pdelay_Resp_Follow_Up messages, the value is transmitted as 0x7F and ignored on
                                        // reception.
}

class GptpTlv extends cObject {
    @packetData;
    GptpTlvType tlvType @bit(16);
    uint16_t lengthField @bit(16);
}

// The fields of the Follow_Up information TLV shall be as specified in Table 11-11 and in 11.4.4.3.2 through
// 11.4.4.3.9. This TLV is a standard organization extension TLV for the Follow_Up message, as specified in
// 14.3 of IEEE Std 1588-2019.
// NOTE—The Follow_Up information TLV is different from the CUMULATIVE_RATE_RATIO TLV of IEEE Std 1588-
// 2019 (see 16.10 and Table 52 of IEEE Std 1588-2019). While both TLVs carry cumulative rate offset information, the
// Follow_Up information TLV also carries information on the Grandmaster Clock time base, most recent phase change,
// and most recent frequency change. The CUMULATIVE_RATE_RATIO TLV is not used by gPTP.
class GptpFollowUpInformationTlv extends GptpTlv
{
    tlvType = GPTP_FOLLOW_UP_INFORMATION_TLV;
    lengthField @bit(16) = B(GPTP_FOLLOW_UP_INFORMATION_TLV_BODYSIZE).get();
    uint32_t organizationId @bit(24) = 0x0080C2;
    uint32_t organizationSubType @bit(24) = 1;
    double rateRatio @bit(32); // 11.4.4.3.6 The value of cumulativeScaledRateOffset is equal to (rateRatio – 1.0) / (2^41), truncated to the next smaller
                               // signed integer, where rateRatio is the ratio of the frequency of the Grandmaster Clock to the frequency of the
                               // LocalClock entity in the PTP Instance that sends the message.
    uint16_t gmTimeBaseIndicator @bit(16); // The value of gmTimeBaseIndicator is the timeBaseIndicator of the ClockSource entity for the current
                                           // Grandmaster PTP Instance (see 9.2.2.3).
    clocktime_t lastGmPhaseChange @bit(96); // The value of lastGmPhaseChange is the time of the current Grandmaster Clock minus the time of the
                                            // previous Grandmaster Clock, at the time that the current Grandmaster PTP Instance became the
                                            // Grandmaster PTP Instance. The value is copied from the lastGmPhaseChange member of the MDSyncSend
                                            // structure whose receipt causes the MD entity to send the Follow_Up message (see 11.2.11).
    int32_t scaledLastGmFreqChange @bit(32); // The value of scaledLastGmFreqChange is the fractional frequency offset of the current Grandmaster Clock
                                             // relative to the previous Grandmaster Clock, at the time that the current Grandmaster PTP Instance became
                                             // the Grandmaster PTP Instance, or relative to itself prior to the last change in gmTimeBaseIndicator,
                                             // multiplied by 241 and truncated to the next smaller signed integer. The value is obtained by multiplying the
                                             // lastGmFreqChange member of MDSyncSend whose receipt causes the MD entity to send the Follow_Up
                                             // message (see 11.2.11) by 241, and truncating to the next smaller signed integer.
}

cplusplus(GptpFollowUpInformationTlv) {{
    void setCumulativeScaledRateOffset(int32_t x) { setRateRatio(1.0 + (double)(x) / (double)((uint64_t)1<<41)); }
    int32_t getCumulativeScaledRateOffset() const { return (int32_t)ceil((getRateRatio() - 1.0) * (double)((uint64_t)1<<41)); }
}}

class GptpSync extends GptpBase
{
    messageType = GPTPTYPE_SYNC;
    flags = twoStepFlag;
    chunkLength = GPTP_SYNC_2STEP_PACKET_SIZE;
    messageLengthField = B(GPTP_SYNC_2STEP_PACKET_SIZE).get();
    // clocktime_t originTimestamp; // filled when twoStep flag is FALSE
    // followUpInformationTLV; // filled when twoStep flag is FALSE
}

class GptpFollowUp extends GptpBase
{
    messageType = GPTPTYPE_FOLLOW_UP;
    chunkLength = GPTP_FOLLOW_UP_PACKET_SIZE;
    messageLengthField = B(GPTP_FOLLOW_UP_PACKET_SIZE).get();
    clocktime_t preciseOriginTimestamp;  // 11.4.4.2.1
    GptpFollowUpInformationTlv followUpInformationTLV;
}

class GptpPdelayReq extends GptpBase
{
    messageType = GPTPTYPE_PDELAY_REQ;
    chunkLength = GPTP_PDELAY_REQ_PACKET_SIZE;
    messageLengthField = B(GPTP_PDELAY_REQ_PACKET_SIZE).get();
    clocktime_t reserved1;
    clocktime_t reserved2;
}

class GptpPdelayResp extends GptpBase
{
    messageType = GPTPTYPE_PDELAY_RESP;
    chunkLength = GPTP_PDELAY_RESP_PACKET_SIZE;
    messageLengthField = B(GPTP_PDELAY_RESP_PACKET_SIZE).get();
    clocktime_t requestReceiptTimestamp;
    PortIdentity requestingPortIdentity;
}

class GptpPdelayRespFollowUp extends GptpBase
{
    messageType = GPTPTYPE_PDELAY_RESP_FOLLOW_UP;
    chunkLength = GPTP_PDELAY_RESP_FOLLOW_UP_PACKET_SIZE;
    messageLengthField = B(GPTP_PDELAY_RESP_FOLLOW_UP_PACKET_SIZE).get();
    clocktime_t responseOriginTimestamp;
    PortIdentity requestingPortIdentity;
}

class GptpIngressTimeInd extends TagBase
{
    clocktime_t arrivalClockTime;
}
