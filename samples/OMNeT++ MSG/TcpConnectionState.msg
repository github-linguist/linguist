//
// Copyright (C) 2022 OpenSim Ltd.
//
// SPDX-License-Identifier: LGPL-3.0-or-later
//

import inet.common.INETDefs;
import inet.transportlayer.tcp_common.TcpHeader;

namespace inet::tcp;

cplusplus {{
typedef std::list<Sack> SackList;
}}

class SackList { @existingClass; }

//
// TCP FSM states
//
// Brief descriptions (cf RFC 793, page 20):
//
// LISTEN - waiting for a connection request
// SYN-SENT - part of 3-way handshake (waiting for peer's SYN+ACK or SYN)
// SYN-RECEIVED - part of 3-way handshake (we sent SYN too, waiting for it to be acked)
// ESTABLISHED - normal data transfer
// FIN-WAIT-1 - FIN sent, waiting for its ACK (or peer's FIN)
// FIN-WAIT-2 - our side of the connection closed (our FIN acked), waiting for peer's FIN
// CLOSE-WAIT - FIN received and acked, waiting for local user to close
// LAST-ACK - remote side closed, FIN sent, waiting for its ACK
// CLOSING - simultaneous close: sent FIN, then got peer's FIN
// TIME-WAIT - both FIN's acked, waiting for some time to be sure remote TCP received our ACK
// CLOSED - represents no connection state at all.
//
// Note: FIN-WAIT-1, FIN-WAIT-2, CLOSING, TIME-WAIT represents active close (that is,
// local user closes first), and CLOSE-WAIT and LAST-ACK represents passive close.
//
enum TcpState {
    TCP_S_INIT        = 0;
    TCP_S_CLOSED      = 1;
    TCP_S_LISTEN      = 2;
    TCP_S_SYN_SENT    = 3;
    TCP_S_SYN_RCVD    = 4;
    TCP_S_ESTABLISHED = 5;
    TCP_S_CLOSE_WAIT  = 6;
    TCP_S_LAST_ACK    = 7;
    TCP_S_FIN_WAIT_1  = 8;
    TCP_S_FIN_WAIT_2  = 9;
    TCP_S_CLOSING     = 10;
    TCP_S_TIME_WAIT   = 11;
};

//
// Event, strictly for the FSM state transition purposes.
// DO NOT USE outside performStateTransition()!
//
enum TcpEventCode {
    TCP_E_IGNORE = 0;

    // app commands
    // (Note: no RECEIVE command, data are automatically passed up)
    TCP_E_OPEN_ACTIVE = 1;
    TCP_E_OPEN_PASSIVE = 2;
    TCP_E_ACCEPT = 3;
    TCP_E_SEND = 4;
    TCP_E_CLOSE = 5;
    TCP_E_ABORT = 6;
    TCP_E_DESTROY = 7;
    TCP_E_STATUS = 8;
    TCP_E_QUEUE_BYTES_LIMIT = 9;
    TCP_E_READ = 10;
    TCP_E_SETOPTION = 11;

    // TPDU types
    TCP_E_RCV_DATA = 12;
    TCP_E_RCV_ACK = 13;
    TCP_E_RCV_SYN = 14;
    TCP_E_RCV_SYN_ACK = 15;
    TCP_E_RCV_FIN = 16;
    TCP_E_RCV_FIN_ACK = 17;
    TCP_E_RCV_RST = 18; // covers RST+ACK too

    TCP_E_RCV_UNEXP_SYN = 19; // unexpected SYN

    // timers
    TCP_E_TIMEOUT_2MSL = 20; // RFC 793, a.k.a. TIME-WAIT timer
    TCP_E_TIMEOUT_CONN_ESTAB = 21;
    TCP_E_TIMEOUT_FIN_WAIT_2 = 22;

    // All other timers (REXMT, PERSIST, DELAYED-ACK, KEEP-ALIVE, etc.),
    // are handled in TcpAlgorithm.
};

//
// Contains state variables ("TCB") for TCP.
//
// TcpStateVariables is effectively a "struct" -- it only contains
// public data members. (Only declared as a class so that we can use
// cObject as base class and make it possible to inspect
// it in Tkenv.)
//
// TcpStateVariables only contains variables needed to implement
// the "base" (RFC 793) TCP. More advanced TCP variants are encapsulated
// into TcpAlgorithm subclasses which can have their own state blocks,
// subclassed from TcpStateVariables. See TcpAlgorithm::createStateVariables().
//
struct TcpStateVariables
{
    @implements(cObject);
    @descriptor(readonly);

    bool active = false;            // set if the connection was initiated by an active open
    bool fork = false;              // if passive and in LISTEN: whether to fork on an incoming connection

    uint32_t snd_mss = 0;           // sender maximum segment size (without headers, i.e. only segment text); see RFC 2581, page 1.
                                    // This will be set to the minimum of the local smss parameter and the value specified in the
                                    // MSS option received during connection setup.

    // send sequence number variables (see RFC 793, "3.2. Terminology")
    uint32_t snd_una = 0;           // send unacknowledged
    uint32_t snd_nxt = 0;           // send next (drops back on retransmission)
    uint32_t snd_max = 0;           // max seq number sent (needed because snd_nxt is re-set on retransmission)
    uint32_t snd_wnd = 0;           // send window
    uint32_t snd_up = 0;            // send urgent pointer
    uint32_t snd_wl1 = 0;           // segment sequence number used for last window update
    uint32_t snd_wl2 = 0;           // segment ack. number used for last window update
    uint32_t iss = 0;               // initial sequence number (ISS)

    // receive sequence number variables
    uint32_t rcv_nxt = 0;           // receive next
    uint32_t rcv_wnd = 0;           // receive window
    uint32_t rcv_up = 0;            // receive urgent pointer;
    uint32_t irs = 0;               // initial receive sequence number
    uint32_t rcv_adv = 0;           // advertised window

    // SYN, SYN+ACK retransmission variables (handled separately
    // because normal rexmit belongs to TcpAlgorithm)
    int syn_rexmit_count = 0;       // number of SYN/SYN+ACK retransmissions (=1 after first rexmit)
    simtime_t syn_rexmit_timeout;   // current SYN/SYN+ACK retransmission timeout

    // whether ACK of our FIN has been received. Needed in FIN bit processing
    // to decide between transition to TIME-WAIT and CLOSING (set event code
    // TCP_E_RCV_FIN or TCP_E_RCV_FIN_ACK).
    bool fin_ack_rcvd = false;

    bool send_fin = false;          // true if a user CLOSE command has been "queued"
    uint32_t snd_fin_seq = 0;       // if send_fin == true: FIN should be sent just before this sequence number

    bool fin_rcvd = false;          // whether FIN received or not
    uint32_t rcv_fin_seq = 0;       // if fin_rcvd: sequence number of received FIN

    bool nagle_enabled = false;                 // set if Nagle's algorithm (RFC 896) is enabled
    bool delayed_acks_enabled = false;          // set if delayed ACK algorithm (RFC 1122) is enabled
    bool limited_transmit_enabled = false;      // set if Limited Transmit algorithm (RFC 3042) is enabled
    bool increased_IW_enabled = false;          // set if Increased Initial Window (RFC 3390) is enabled

    uint32_t full_sized_segment_counter = 0;    // this counter is needed for delayed ACK
    bool ack_now = false;                       // send ACK immediately, needed if delayed_acks_enabled is set
                                                // Based on [Stevens, W.R.: TCP/IP Illustrated, Volume 2, page 861].
                                                // ack_now should be set when:
                                                //   - delayed ACK timer expires
                                                //   - an out-of-order segment is received
                                                //   - SYN is received during the three-way handshake
                                                //   - a persist probe is received
                                                //   - FIN is received

    bool afterRto = false;                      // set at RTO, reset when snd_nxt == snd_max or snd_una == snd_max

    // WINDOW_SCALE related variables
    bool ws_support = false;                    // set if the host supports Window Scale (header option) (RFC 1323)
    bool ws_enabled = false;                    // set if the connection uses Window Scale (header option)
    int  ws_manual_scale = -1;                  // the value of scale parameter if it was set manually (-1 otherwise)
    bool snd_ws = false;                        // set if initial WINDOW_SCALE has been sent
    bool rcv_ws = false;                        // set if initial WINDOW_SCALE has been received
    unsigned int rcv_wnd_scale = 0;             // RFC 1323, page 31: "Receive window scale power"
    unsigned int snd_wnd_scale = 0;             // RFC 1323, page 31: "Send window scale power"

    // TIMESTAMP related variables
    bool ts_support = false;        // set if the host supports Timestamps (header option) (RFC 1322)
    bool ts_enabled = false;        // set if the connection uses Window Scale (header option)
    bool snd_initial_ts = false;    // set if initial TIMESTAMP has been sent
    bool rcv_initial_ts = false;    // set if initial TIMESTAMP has been received
    uint32_t ts_recent = 0;         // RFC 1323, page 31: "Latest received Timestamp"
    uint32_t last_ack_sent = 0;     // RFC 1323, page 31: "Last ACK field sent"
    simtime_t time_last_data_sent;  // time at which the last data segment was sent (needed to compute the IDLE time for PAWS)

    // SACK related variables
    bool sack_support = false;      // set if the host supports selective acknowledgment (header option) (RFC 2018, 2883, 3517)
    bool sack_enabled = false;      // set if the connection uses selective acknowledgment (header option)
    bool snd_sack_perm = false;     // set if SACK_PERMITTED has been sent
    bool rcv_sack_perm = false;     // set if SACK_PERMITTED has been received
    uint32_t start_seqno = 0;       // start sequence number of last received out-of-order segment
    uint32_t end_seqno = 0;         // end sequence number of last received out-of-order segment
    bool snd_sack = false;          // set if received vaild out-of-order segment or rcv_nxt changed, but receivedQueue is not empty
    bool snd_dsack = false;         // set if received duplicated segment (sequenceNo+PLength < rcv_nxt) or (segment is not acceptable)
    SackList sacks_array;    // MAX_SACK_BLOCKS is set to 60
    uint32_t highRxt = 0;    // RFC 3517, page 3: ""HighRxt" is the highest sequence number which has been retransmitted during the current loss recovery phase."
    uint32_t pipe = 0;    // RFC 3517, page 3: ""Pipe" is a sender's estimate of the number of bytes outstanding in the network."
    uint32_t recoveryPoint = 0;    // RFC 3517
    uint32_t sackedBytes = 0;    // number of sackedBytes
    uint32_t sackedBytes_old = 0;    // old number of sackedBytes - needed for RFC 3042 to check if last dupAck contained new sack information
    bool lossRecovery = false;    // indicates if algorithm is in loss recovery phase

    // queue management
    uint32_t sendQueueLimit = 0;
    bool queueUpdate = true;

    // those counters would logically belong to TcpAlgorithm, but it's a lot easier to manage them here
    uint32_t dupacks = 0;    // current number of received consecutive duplicate ACKs
    uint32_t snd_sacks = 0;    // number of sent sacks
    uint32_t rcv_sacks = 0;    // number of received sacks
    uint32_t rcv_oooseg = 0;    // number of received out-of-order segments
    uint32_t rcv_naseg = 0;    // number of received not acceptable segments

    // receiver buffer / receiver queue related variables
    uint32_t maxRcvBuffer = 0;    // maximal amount of bytes in tcp receive queue
    uint32_t usedRcvBuffer = 0;    // current amount of used bytes in tcp receive queue
    uint32_t freeRcvBuffer = 0;    // current amount of free bytes in tcp receive queue
    uint32_t tcpRcvQueueDrops = 0;    // number of drops in tcp receive queue

    // ECN
    bool ecnEchoState = false; // indicates if connection is in echo mode (got CE indication from IP and didn't get CWR from sender yet)
    bool sndCwr = false; // set if ECE was handled
    bool gotEce = false; // set if packet with ECE arrived
    bool gotCeIndication = false; // set if CE was set in controlInfo from IP
    bool ect = false; // set if this connection is ECN Capable (ECT stands for ECN-Capable transport - rfc-3168)
    bool endPointIsWillingECN = false; // set if the other end-point is willing to use ECN
    bool ecnSynSent = false; // set if ECN-setup SYN packet was sent
    bool ecnWillingness = false; // set if current host is willing to use ECN
    bool sndAck = false; // set if sending Ack packet, used to set relevant info in controlInfo.
    bool rexmit = false; // set if retransmitting data, used to send not-ECT codepoint (rfc3168, p. 20)
    simtime_t eceReactionTime; // records the time of the last ECE reaction

    uint32_t dupthresh = 0; // used for TcpTahoe, TcpReno and SACK (RFC 3517)
};

cplusplus(TcpStateVariables) {{
  public:
    virtual ~TcpStateVariables();
    virtual std::string str() const override;
    virtual std::string detailedInfo() const;
}}
