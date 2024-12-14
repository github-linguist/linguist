//
// Copyright (C) 2016 OpenSim Ltd.
//
// SPDX-License-Identifier: LGPL-3.0-or-later
//

import inet.common.INETDefs;
import inet.common.packet.ChunkBuffer;
import inet.common.packet.ChunkQueue;
import inet.common.packet.Packet;
import inet.networklayer.common.L3Address;
import inet.transportlayer.tcp.TcpConnectionState;

cplusplus {{
#include "inet/transportlayer/tcp/Tcp.h"
#include "inet/transportlayer/tcp/TcpAlgorithm.h"
#include "inet/transportlayer/tcp/TcpConnection.h"
#include "inet/transportlayer/tcp/TcpReceiveQueue.h"
#include "inet/transportlayer/tcp/TcpSackRexmitQueue.h"
#include "inet/transportlayer/tcp/TcpSendQueue.h"
#include "inet/transportlayer/tcp_common/TcpHeader.h"
}}

namespace inet::tcp;

class TcpSackRexmitQueue { @existingClass; }
class TcpAlgorithm extends cObject { @existingClass; }

class TcpSendQueue extends cObject
{
    @existingClass;
    @descriptor(readonly);
    ChunkQueue dataBuffer;
}

class TcpReceiveQueue extends cObject
{
    @existingClass;
    @descriptor(readonly);
    ReorderBuffer reorderBuffer;
}

class TcpConnection extends cObject
{
    @existingClass;
    @descriptor(readonly);

    int socketId;    // identifies connection within the app
    int listeningSocketId;    // identifies listening connection within the app

    // socket pair
    L3Address localAddr @getter(getLocalAddress);
    L3Address remoteAddr @getter(getRemoteAddress);
    int localPort;
    int remotePort;

    TcpStateVariables *state;

    // TCP queues
    TcpSendQueue *sendQueue;
    TcpReceiveQueue *receiveQueue;
    TcpSackRexmitQueue *rexmitQueue;

    TcpAlgorithm *tcpAlgorithm;
    int fsmState;
};
