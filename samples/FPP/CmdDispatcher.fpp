
module Svc {

  @ A component for dispatching commands
  active component CommandDispatcher {

    # ----------------------------------------------------------------------
    # General ports
    # ----------------------------------------------------------------------

    @ Command dispatch port
    output port compCmdSend: [CmdDispatcherComponentCommandPorts] Fw.Cmd

    @ Command Registration Port. max_number should match dispatch port.
    guarded input port compCmdReg: [CmdDispatcherComponentCommandPorts] Fw.CmdReg

    @ Input Command Status Port
    async input port compCmdStat: Fw.CmdResponse

    @ Output Command Status Port
    output port seqCmdStatus: [CmdDispatcherSequencePorts] Fw.CmdResponse

    @ Command buffer input port for sequencers or other sources of command buffers
    async input port seqCmdBuff: [CmdDispatcherSequencePorts] Fw.Com hook

    @ Ping input port
    async input port pingIn: Svc.Ping

    @ Run port used to emit telemetry
    async input port run: Svc.Sched

    @ Ping output port
    output port pingOut: Svc.Ping

    # ----------------------------------------------------------------------
    # Port matching specifiers
    # ----------------------------------------------------------------------

    match compCmdSend with compCmdReg

    match seqCmdStatus with seqCmdBuff

    # ----------------------------------------------------------------------
    # Special ports
    # ----------------------------------------------------------------------

    @ Command receive port
    command recv port CmdDisp

    @ Command registration port
    command reg port CmdReg

    @ Command response port
    command resp port CmdStatus

    @ Event port
    event port Log

    @ Text event port
    text event port LogText

    @ Time get port
    time get port Time

    @ Telemetry port
    telemetry port Tlm

    # ----------------------------------------------------------------------
    # Commands
    # ----------------------------------------------------------------------

    @ No-op command
    async command CMD_NO_OP \
      opcode 0

    @ No-op string command
    async command CMD_NO_OP_STRING(
                                    arg1: string size 40 @< The String command argument
                                  ) \
      opcode 1

    @ No-op command
    async command CMD_TEST_CMD_1(
                                  arg1: I32 @< The I32 command argument
                                  arg2: F32 @< The F32 command argument
                                  arg3: U8 @< The U8 command argument
                                ) \
      opcode 2

    @ Clear command tracking info to recover from components not returning status
    async command CMD_CLEAR_TRACKING \
      opcode 3

    # ----------------------------------------------------------------------
    # Events
    # ----------------------------------------------------------------------

    event OpCodeRegistered(
                            Opcode: FwOpcodeType @< The opcode to register
                            $port: I32 @< The registration port
                            slot: I32 @< The dispatch slot it was placed in
                          ) \
      severity diagnostic \
      id 0 \
      format "Opcode 0x{x} registered to port {} slot {}"

    @ Op code dispatched event
    event OpCodeDispatched(
                            Opcode: FwOpcodeType @< The opcode dispatched
                            $port: I32 @< The port dispatched to
                          ) \
      severity command \
      id 1 \
      format "Opcode 0x{x} dispatched to port {}"

    @ Op code completed event
    event OpCodeCompleted(
                           Opcode: FwOpcodeType @< The I32 command argument
                         ) \
      severity command \
      id 2 \
      format "Opcode 0x{x} completed"

    @ Op code completed with error event
    event OpCodeError(
                       Opcode: FwOpcodeType @< The opcode with the error
                       error: Fw.CmdResponse @< The error value
                     ) \
      severity command \
      id 3 \
      format "Opcode 0x{x} completed with error {}"

    @ Received a malformed command packet
    event MalformedCommand(
                            Status: Fw.DeserialStatus @< The deserialization error
                          ) \
      severity warning high \
      id 4 \
      format "Received malformed command packet. Status: {}"

    @ Received an invalid opcode
    event InvalidCommand(
                          Opcode: FwOpcodeType @< Invalid opcode
                        ) \
      severity warning high \
      id 5 \
      format "Invalid opcode 0x{x} received"

    @ Exceeded the number of commands that can be simultaneously executed
    event TooManyCommands(
                           Opcode: FwOpcodeType @< The opcode that overflowed the list
                         ) \
      severity warning high \
      id 6 \
      format "Too many outstanding commands. opcode=0x{x}"

    @ The command dispatcher has successfully received a NO-OP command
    event NoOpReceived \
      severity activity high \
      id 7 \
      format "Received a NO-OP command"

    @ The command dispatcher has successfully received a NO-OP command from GUI with a string
    event NoOpStringReceived(
                              message: string size 40 @< The NO-OP string that is generated
                            ) \
      severity activity high \
      id 8 \
      format "Received a NO-OP string={}"

    @ This log event message returns the TEST_CMD_1 arguments.
    event TestCmd1Args(
                        arg1: I32 @< Arg1
                        arg2: F32 @< Arg2
                        arg3: U8 @< Arg3
                      ) \
      severity activity high \
      id 9 \
      format "TEST_CMD_1 args: I32: {}, F32: {f}, U8: {}"

    @ Op code reregistered event
    event OpCodeReregistered(
                              Opcode: FwOpcodeType @< The opcode reregistered
                              $port: I32 @< The reregistration port
                            ) \
      severity diagnostic \
      id 10 \
      format "Opcode 0x{x} is already registered to port {}"

    @ This log event reports the Command Sequence Buffer port queue has overflowed.
    event CommandDroppedQueueOverflow(
                              OpCode: FwOpcodeType @< The command opcode dropped
                              Context: U32 @< The call order
                            ) \
      severity warning high \
      id 11 \
      format "Opcode 0x{x} was dropped due to buffer overflow and not processed. Context {}" \
      throttle 5

    # ----------------------------------------------------------------------
    # Telemetry
    # ----------------------------------------------------------------------

    @ Number of commands dispatched
    telemetry CommandsDispatched: U32 id 0 update on change

    @ Number of command errors
    telemetry CommandErrors: U32 id 1 update on change

    @ Number of commands drooped due to buffer overflow
    telemetry CommandsDropped: U32 id 2 update on change
  }

}
