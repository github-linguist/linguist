//
// Copyright (C) 2006 OpenSim Ltd.
//
// SPDX-License-Identifier: LGPL-3.0-or-later
//


package inet.linklayer.ieee80211;

import inet.linklayer.common.IIeee8021dQosClassifier;
import inet.linklayer.contract.IWirelessInterface;
import inet.linklayer.ieee80211.llc.IIeee80211Llc;
import inet.linklayer.ieee80211.mgmt.IIeee80211Agent;
import inet.linklayer.ieee80211.mgmt.IIeee80211Mgmt;
import inet.linklayer.ieee80211.mib.Ieee80211Mib;
import inet.networklayer.common.NetworkInterface;
import inet.physicallayer.wireless.common.contract.packetlevel.IRadio;

//
// This module implements an IEEE 802.11 network interface. It implements
// a large subset of the IEEE 802.11 standard, and may use radio models
// and wireless signal representations of varying levels of detail.
// It is also extremely configurable, and its component structure makes
// it easy to experiment with various details of the protocol.
//
// The main components of the interface are the MAC and the radio submodules.
// Most configuration parameters are in the MAC (~Ieee80211Mac) and its
// numerous subcomponents. Most subcomponents are replaceable to facilitate
// experimentation by using typename assignments in the configuration.
//
// The default radio is ~Ieee80211ScalarRadio, but it can be replaced
// with any of several compatible radio types using typename assignments
// in the configuration. Several radio models of varying levels of detail
// are available, from simple unit disk radio models to layered radio models
// that explicitly model forward error correction, scrambling, symbol
// encoding/decoding, etc, and may represent radio signals with a
// multi-dimensional power density function over time and frequency.
// The INET User Guide elaborates on the possibilities.
//
// The type of the management submodule is also configurable. The type of this
// submodule decides whether the interfaces acts as an AP, a STA, or is in ad hoc mode.
// Use ~Ieee80211MgmtSta or ~Ieee80211MgmtStaSimplified for STA,
// ~Ieee80211MgmtAp or ~Ieee80211MgmtApSimplified for AP, and
// ~Ieee80211MgmtAdhoc for ad hoc mode.
//
// The agent submodule is responsible for initiating the process of connecting
// to an AP and similar tasks normally done from "user space".
//
// The LLC submodule is responsible for adding/removing the LLC header on packets.
//
// A classifier is responsible for assigning a 802.1d User Priority (UP) to
// packets and is only needed if QoS is involved.
//
// The clock submodule would allow clock skew modeling -- this is currently not used.
//
// Note about the implementation:
//
// Despite its appearance, Ieee80211Interface is not a plain compound module.
// It has an underlying custom C++ class that inherits from `cModule`.
//
module Ieee80211Interface extends NetworkInterface like IWirelessInterface
{
    parameters:
        string interfaceTableModule;
        string energySourceModule = default("");
        string opMode @enum("a","b","g(erp)","g(mixed)","n(mixed-2.4Ghz)","p","ac") = default("g(mixed)");
        string address @mutable = default("auto"); // MAC address as hex string (12 hex digits), or
                                                   // "auto". "auto" values will be replaced by
                                                   // a generated MAC address in init stage 0.
        string protocol = default("");
        double bitrate @unit(bps) = default(-1bps);
        **.opMode = this.opMode;
        **.bitrate = this.bitrate;
        mac.modeSet = default(this.opMode);
        mac.*.rateSelection.dataFrameBitrate = default(this.bitrate);
        radio.signalAnalogRepresentation = default("scalar");
        *.macModule = default(absPath(".mac"));
        *.mibModule = default(absPath(".mib"));
        *.interfaceTableModule = default(absPath(this.interfaceTableModule));
        *.energySourceModule = default(absPath(this.energySourceModule));
    gates:
        input upperLayerIn;
        output upperLayerOut;
        input radioIn @labels(IWirelessSignal);
    submodules:
        mib: Ieee80211Mib {
            parameters:
                @display("p=100,300;is=s");
        }
        llc: <default(opMode == "p" ? "Ieee80211LlcEpd" : "Ieee80211LlcLpd")> like IIeee80211Llc {
            parameters:
                @display("p=300,200");
        }
        classifier: <default("OmittedIeee8021dQosClassifier")> like IIeee8021dQosClassifier {
            parameters:
                @display("p=500,100");
        }
        agent: <default("Ieee80211AgentSta")> like IIeee80211Agent if typename != "" {
            parameters:
                @display("p=700,300");
        }
        mgmt: <default("Ieee80211MgmtSta")> like IIeee80211Mgmt {
            parameters:
                @display("p=500,300");
        }
        mac: <default("Ieee80211Mac")> like IIeee80211Mac {
            parameters:
                @display("p=300,300");
        }
        radio: <default("Ieee80211Radio")> like IRadio {
            parameters:
                @display("p=300,400");
        }
    connections:
        radioIn --> { @display("m=s"); } --> radio.radioIn;
        radio.upperLayerIn <-- mac.lowerLayerOut;
        radio.upperLayerOut --> mac.lowerLayerIn;

        mac.mgmtOut --> mgmt.macIn;
        mac.mgmtIn <-- mgmt.macOut;

        mgmt.agentOut --> agent.mgmtIn if exists(agent);
        mgmt.agentIn <-- agent.mgmtOut if exists(agent);

        llc.upperLayerOut --> { @display("m=n"); } --> upperLayerOut;
        llc.upperLayerIn <-- { @display("m=n"); } <-- classifier.out;

        llc.lowerLayerOut --> mac.upperLayerIn;
        llc.lowerLayerIn <-- mac.upperLayerOut;

        classifier.in <-- { @display("m=n"); } <-- upperLayerIn;
}

