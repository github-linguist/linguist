mtype = { SYN, FIN, ACK, SYN_ACK }

chan AtoN = [1] of { mtype };
chan NtoA = [1] of { mtype };
chan BtoN = [1] of { mtype };
chan NtoB = [1] of { mtype };

int state[2];
int pids[2];

#define ClosedState    0
#define ListenState    1
#define SynSentState   2
#define SynRecState    3
#define EstState       4
#define FinW1State     5
#define CloseWaitState 6
#define FinW2State     7
#define ClosingState   8
#define LastAckState   9
#define TimeWaitState  10
#define EndState       -1

#define leftConnecting (state[0] == ListenState && state[1] == SynSentState)
#define leftEstablished (state[0] == EstState)
#define rightEstablished (state[1] == EstState)
#define leftClosed (state[0] == ClosedState)

proctype TCP(chan snd, rcv; int i) {
	pids[i] = _pid;
CLOSED:
	state[i] = ListenState;
	if
	/* Passive open */
	:: goto LISTEN;
	/* Active open */
	:: snd ! SYN; goto SYN_SENT;
	/* Terminate */
	:: goto end;
	fi
LISTEN:
	state[i] = ListenState;
	if
	:: rcv ? SYN -> snd ! SYN_ACK; goto SYN_RECEIVED;
	/* Simultaneous LISTEN */
	:: timeout -> goto CLOSED; 
	/* recently added the 'timout.' - Max */
	fi
SYN_SENT:
	state[i] = SynSentState;
	if
	/* Simultaneous open */
	:: rcv ? SYN -> snd ! ACK; goto SYN_RECEIVED;
	/* Standard behavior */
	:: rcv ? SYN_ACK -> snd ! ACK; goto ESTABLISHED;
	/* Timeout */
	:: timeout -> goto CLOSED;
	fi
SYN_RECEIVED:
	state[i] = SynRecState;
	rcv ? ACK; goto ESTABLISHED;
	/* We may want to consider putting a timeout -> CLOSED here. */
ESTABLISHED:
	state[i] = EstState;
	if
	/* Close - initiator sequence */
	:: snd ! FIN; goto FIN_WAIT_1;
	/* Close - responder sequence */
	:: rcv ? FIN -> snd ! ACK; goto CLOSE_WAIT;
	fi
FIN_WAIT_1:
	state[i] = FinW1State;
	if
	/* Simultaneous close */
	:: rcv ? FIN -> snd ! ACK; goto CLOSING;
	/* Standard close */
	:: rcv ? ACK -> goto FIN_WAIT_2;
	fi
CLOSE_WAIT:
	state[i] = CloseWaitState;
	snd ! FIN; goto LAST_ACK;
FIN_WAIT_2:
	state[i] = FinW2State;
	rcv ? FIN -> snd ! ACK; goto TIME_WAIT;
CLOSING:
	state[i] = ClosingState;
	rcv ? ACK -> goto TIME_WAIT;
LAST_ACK:
	state[i] = LastAckState;
	rcv ? ACK -> goto CLOSED;
TIME_WAIT:
	state[i] = TimeWaitState;
	goto CLOSED;
end:
	state[i] = EndState;
}

init {
	state[0] = ClosedState;
	state[1] = ClosedState;
	run TCP(AtoN, NtoA, 0);
	run TCP(BtoN, NtoB, 1);
}