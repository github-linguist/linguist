// A ground station and a spacecraft on a communication link, after the
// OpenSE Cookbook's "Spacecraft Example" (SysML2x/Spacecraft Example/
// Spacecraft_Example_SysML2.ipynb, written in the 2019 pre-release notation)
// re-spelled in current SysML v2.
//
// The station pings the spacecraft 30 s into the mission; the spacecraft answers
// with one 1024-byte frame per second while a parallel branch drains the battery
// 2 % per frame. Under 80 % the charging region recharges 1 % per second; under
// 40 % the transmission is interrupted and resumes once the battery is back over
// 80 %. The question the model exists to answer: how long does it take to send
// 100 kB, and what does the battery do meanwhile?
package SpacecraftComms {
    private import ScalarValues::*;
    private import SI::*;

    item def Data;
    item def GroundStationPing;
    item def StartSignal;
    item def ShutDownSignal;
    item def BatteryLow;
    item def BatteryFullyCharged;
    item def TransmissionDone;

    port def CommunicationInterface {
        out item data : Data;
        in item ping : GroundStationPing;
    }

    interface def CommunicationLink {
        end port spacePort : CommunicationInterface;
        end port groundPort : ~CommunicationInterface;
        flow spacePort.data to groundPort.data;
        flow groundPort.ping to spacePort.ping;
    }

    part def GroundStation {
        port commIF : ~CommunicationInterface;
        attribute framesReceived : Integer = 0;

        exhibit state modes {
            entry; then idle;
            state idle;
            state operation {
                entry send new GroundStationPing() via commIF;
                do action receiveData {
                    first start;
                    then merge repeat;
                    then action receive accept d : Data via commIF;
                    then action count assign framesReceived := framesReceived + 1;
                    then repeat;
                }
            }
            transition first idle accept after 30 [s] then operation;
            transition first idle accept StartSignal then operation;
            transition first operation accept ShutDownSignal then idle;
        }
    }

    part def SpacecraftVehicle {
        port commIF : CommunicationInterface;
        attribute data : Real = 102400;
        attribute battery : Real = 100;
        attribute frameSize : Real = 1024;
        attribute drainPerFrame : Real = 2;
        attribute chargePerSecond : Real = 1;
        attribute lowLevel : Real = 40;
        attribute resumeLevel : Real = 80;
        attribute fullLevel : Real = 100;

        exhibit state modes parallel {
            state dataTransit {
                entry; then waitingGSPing;
                state waitingGSPing;
                state transmitting {
                    do action transmitData {
                        first start;
                        then fork split;
                        succession first split then consumePower;
                        succession first split then sendFrames;

                        action consumePower {
                            first start;
                            then merge again;
                            then action tick accept after 1 [s];
                            then action drain assign battery := battery - drainPerFrame;
                            then decide;
                                if battery >= lowLevel then again;
                                else lowBattery;
                            action lowBattery send new BatteryLow();
                            then done;
                        }

                        action sendFrames {
                            first start;
                            then merge repeat;
                            then decide;
                                if data > 0 then sendFrame;
                                else finish;
                            action sendFrame {
                                first start;
                                then action wait accept after 1 [s];
                                then action consume assign data := data - frameSize;
                                then action transmit send new Data() via commIF;
                                then done;
                            }
                            then repeat;
                            action finish send new TransmissionDone();
                            then done;
                        }

                        join meet;
                        succession first consumePower then meet;
                        succession first sendFrames then meet;
                        succession first meet then done;
                    }
                }
                state lowPower;
                state transmitted;
                transition first waitingGSPing accept GroundStationPing via commIF then transmitting;
                transition first transmitting accept BatteryLow then lowPower;
                transition first lowPower accept when battery > resumeLevel then transmitting;
                transition first transmitting accept TransmissionDone then transmitted;
            }

            state charging {
                entry; then notRecharging;
                state notRecharging;
                state recharging {
                    do action recharge {
                        first start;
                        then merge again;
                        then decide;
                            if battery >= fullLevel then charged;
                            else charge;
                        action charge {
                            first start;
                            then action wait accept after 1 [s];
                            then action add assign battery := battery + chargePerSecond;
                            then done;
                        }
                        then again;
                        action charged send new BatteryFullyCharged();
                        then done;
                    }
                }
                transition first notRecharging accept when battery < resumeLevel then recharging;
                transition first recharging accept BatteryFullyCharged then notRecharging;
            }
        }
    }

    part def Mission {
        part groundStation : GroundStation;
        part spacecraftVehicle : SpacecraftVehicle;
        interface link : CommunicationLink
            connect spacecraftVehicle.commIF to groundStation.commIF;
    }

    part mission : Mission;
}
