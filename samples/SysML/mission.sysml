// A deep-space relay probe across its mission phases.
//
// One individual probe — the flight article, not a class of probes — changes
// configuration as the mission unfolds. Its history is modeled with the
// occurrence vocabulary: event occurrences mark the moments that matter,
// snapshots freeze its state at those moments, timeslices span the phases
// between them, and `first ... then ...` orders the portions in time.
// A beacon running inside the cruise timeslice sends telemetry out of the
// probe's own antenna, across the mission's connector, to the ground station.

package RelayMission {
    private import ScalarValues::*;

    // ------------------------------------------------------------------
    // Signals and ports
    // ------------------------------------------------------------------

    attribute def Telemetry {
        attribute frames : Real;
    }

    port def XBand;

    // ------------------------------------------------------------------
    // The individual probe and its history
    // ------------------------------------------------------------------

    individual part def Probe {
        attribute mass : Real;
    }

    // The flight article. Its feature values are the probe as built; the
    // portions below are the same individual at particular times.
    individual part scout : Probe {
        attribute :>> mass default = 120.0;

        // The moments that structure the mission, in time order.
        event occurrence separation;
        event occurrence closestApproach;
        first separation then closestApproach;

        // Snapshots: the probe's state frozen at a moment.
        snapshot postSeparation {
            attribute :>> mass = 118.0;
        }
        snapshot postFlyby {
            attribute :>> mass = 96.0;
        }
        first postSeparation then postFlyby;

        // A timeslice: the probe over the whole cruise phase, with the
        // configuration it only has then.
        timeslice cruise {
            attribute spinRate : Real = 2.0;
        }

        // Two trim burns happen during the mission — occurrences with
        // multiplicity, each carrying its own delta-v.
        occurrence burns [2] {
            attribute dv : Real = 1.5;
        }

        // Tracking passes: any number may happen, none are planned yet.
        occurrence dsnPasses [0..*];
    }

    // Propellant spent between the two snapshots, read across portions of
    // one individual.
    calc massSpent {
        in a : Real default scout.postSeparation.mass;
        in b : Real default scout.postFlyby.mass;
        a - b
    }

    // The requirement's subject is a snapshot: the probe as it was after the
    // flyby must still make its mass budget.
    requirement def MassBudget {
        subject probeThen : Probe;

        require constraint {
            probeThen.mass >= 90.0
        }
    }

    requirement flybyMassBudget : MassBudget {
        subject probeThen = scout.postFlyby;
    }

    // ------------------------------------------------------------------
    // Ground segment
    // ------------------------------------------------------------------

    part def Station {
        port dish : XBand;
        attribute frames : Real = 0.0;

        exhibit state listen {
            entry; then standingBy;

            state standingBy;

            state logged;

            transition first standingBy
                accept t : Telemetry via dish
                do assign frames := frames + t.frames
                then logged;
        }
    }

    // ------------------------------------------------------------------
    // The relay probe as flown: a beacon inside the cruise timeslice sends
    // telemetry through the probe's own antenna.
    // ------------------------------------------------------------------

    part def RelayProbe {
        port antenna : XBand;

        timeslice part cruise {
            // A suboccurrence of the cruise phase: one checkout happens
            // within it.
            occurrence checkout {
                attribute passed : Boolean = true;
            }

            exhibit state beacon {
                entry; then idle;

                state idle;

                state sent {
                    entry action downlink {
                        send new Telemetry(frames = 3.0) via antenna;
                    }
                }

                transition first idle then sent;
            }
        }
    }

    part def Mission {
        part relay : RelayProbe;
        part goldstone : Station;

        connect relay.antenna to goldstone.dish;
    }

    part mission : Mission;
}
