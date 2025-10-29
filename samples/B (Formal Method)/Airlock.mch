/* Airlock
 * Author: JJWRoeloffs
 * Creation date: 10/21/25
 */
MACHINE
    Airlock
SEES
    Airlock_pressure_bs
SETS
    PHASE = {ACQ, CTRL}
VARIABLES
    airlock_pressure1,
    airlock_pressure2,
    airlock_pressure3,
    is_indoor_door_openable,
    is_outdoor_door_openable,
    cycle
INVARIANT
    airlock_pressure1 : PRESSURE &
    airlock_pressure2 : PRESSURE &
    airlock_pressure3 : PRESSURE &
    is_indoor_door_openable : BOOL &
    is_outdoor_door_openable : BOOL &
    cycle : PHASE &
    not(is_indoor_door_openable = TRUE & is_outdoor_door_openable = TRUE)
INITIALISATION
    airlock_pressure1 :: PRESSURE ||
    airlock_pressure2 :: PRESSURE ||
    airlock_pressure3 :: PRESSURE ||
    is_indoor_door_openable := FALSE ||
    is_outdoor_door_openable := FALSE ||
    cycle := ACQ
OPERATIONS

    actualize_pressure =
    PRE
        cycle = ACQ
    THEN
        airlock_pressure1 :: PRESSURE ||
        airlock_pressure2 :: PRESSURE ||
        airlock_pressure3 :: PRESSURE ||
        cycle := CTRL
    END;

    enable_doors_opening =
    PRE
        cycle = CTRL
    THEN
        is_indoor_door_openable, is_outdoor_door_openable: (
            is_indoor_door_openable: BOOL &
            is_outdoor_door_openable: BOOL &
            (is_indoor_door_openable = TRUE => (indoor_pressure: {airlock_pressure1, airlock_pressure2, airlock_pressure3} & outdoor_pressure /: {airlock_pressure1, airlock_pressure2, airlock_pressure3})) &
            (is_outdoor_door_openable = TRUE => (outdoor_pressure: {airlock_pressure1, airlock_pressure2, airlock_pressure3} & indoor_pressure /: {airlock_pressure1, airlock_pressure2, airlock_pressure3}))
        ) ||
        cycle := ACQ
    END;

    ret <-- get_airlock_pressure1 =
    PRE
        ret : PRESSURE
    THEN
        ret := airlock_pressure1
    END;
    ret <-- get_airlock_pressure2 =
    PRE
        ret : PRESSURE
    THEN
        ret := airlock_pressure2
    END;
    ret <-- get_airlock_pressure3 =
    PRE
        ret : PRESSURE
    THEN
        ret := airlock_pressure3
    END;

    ret <-- get_is_indoor_door_openable=
    PRE
        ret : BOOL
    THEN
        ret := is_indoor_door_openable
    END;

    ret <-- get_is_outdoor_door_openable=
    PRE
        ret : BOOL
    THEN
        ret := is_outdoor_door_openable
    END;

    ret <-- get_cycle =
    PRE
        ret : PHASE
    THEN
        ret := cycle
    END
END
