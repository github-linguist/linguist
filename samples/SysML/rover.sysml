// A surface rover and the assertions about it, for the verdict table
// `Verdicts(...)` answers over the object a session holds. See README.md.
package RoverChecks {
	private import DocumentQueries::*;
	private import KerML::Root::Element;
	private import ScalarValues::*;

	part def Battery {
		attribute charge : Real = 100.0;
		attribute floor : Real default = 20.0;
		assert constraint aboveFloor { charge >= floor }

		action drain {
			in amount : Real;
			first take;
			action take {
				assign charge := charge - amount;
			}
		}
	}

	part def Wheel {
		attribute wear : Real = 0.0;
		assert constraint treadLeft { wear < 0.5 }

		action roll {
			in km : Real;
			first grind;
			action grind {
				assign wear := wear + km * 0.01;
			}
		}
	}

	// The setpoint is left to the mission: nothing values it, so `warmEnough`
	// has nothing to decide against.
	part def Heater {
		attribute setpoint : Real;
		attribute output : Real = 15.0;
		assert constraint warmEnough { output >= setpoint }
	}

	part def Rover {
		attribute mass : Real = 899.0;
		part battery : Battery;
		part wheels : Wheel[6];
		part heater : Heater;

		assert constraint underLimit { mass <= 1000.0 }

		requirement drivable {
			attribute soc : Real = battery.charge;
			require constraint { soc > 30.0 }
		}

		action drive {
			in km : Real;
			first spend;
			action spend {
				assign battery.charge := battery.charge - km * 1.5;
			}
		}
	}

	// The flight rover draws its battery down to 30, not the default 20.
	part rover : Rover {
		part :>> battery : Battery {
			attribute :>> floor = 30.0;
		}
	}

	requirement def PowerMargin {
		subject b : Battery;
		require constraint { b.charge - b.floor >= 25.0 }
	}
	requirement powerMargin : PowerMargin;
	satisfy powerMargin by rover.battery;

	// Verifies the requirement over the battery the case binds as its subject:
	// the declared one under the declared rover, the held one under a held rover.
	verification def MarginCheck {
		subject b : Battery;
		objective {
			verify powerMargin;
		}
		VerificationCases::PassIf(b.charge - b.floor >= 25.0)
	}
	verification marginCheck : MarginCheck {
		subject b = rover.battery;
	}

	// Every assertion about the object bound to `root` and the objects it holds.
	calc def Checks :> Query {
		in root : Element;
		Project(
			source = Verdicts(source = root),
			properties = ("path", "kind", "name", "verdict", "reason")
		)
	}

	// What does not hold, violated before undecided.
	calc def Findings :> Query {
		in root : Element;
		OrderBy(
			source = WhereFeature(
				source = Verdicts(source = root),
				'feature' = "verdict", operator = "!=", value = "holds"
			),
			property = "verdict",
			direction = "descending",
			missing = "last",
			multiple = "error"
		)
	}

	// Requirements, satisfactions and verification cases, each with the
	// outcome of the cases verifying it.
	calc def Verified :> Query {
		in root : Element;
		Project(
			source = WhereFeature(
				source = Verdicts(source = root),
				'feature' = "kind", operator = "!=", value = "constraint"
			),
			properties = ("path", "kind", "name", "verdict", "verification")
		)
	}

	part def RoverReport :> Document {
		attribute redefines title = "Rover Checks";

		part intro : Paragraph {
			attribute redefines text = "What holds of the rover and of every object it holds, as it stands.";
		}
		part checks : Table {
			attribute redefines caption = "Every assertion";
			calc rows : Checks {
				in root = rover;
			}
		}
		part findings : Section {
			attribute redefines title = "Findings";
			part open : List {
				calc items : Findings {
					in root = rover;
				}
			}
		}
		part verified : Table {
			attribute redefines caption = "Requirements and their verification";
			calc rows : Verified {
				in root = rover;
			}
		}
	}
}
