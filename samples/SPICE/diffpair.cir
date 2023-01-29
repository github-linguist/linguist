simple differential pair - CM and DM dc sensitivity

* Models:
.model qnl npn(level=1 bf=80 rb=100 ccs=2pf tf=0.3ns tr=6ns cje=3pf cjc=2pf va=50)
.model qnr npn(level=1 bf=80 rb=100 ccs=2pf tf=0.3ns tr=6ns cje=3pf cjc=2pf va=50)

.options noacct

* Circuit description:
q1 4 2 6 qnr
q2 5 3 6 qnl
rs1 11 2 1k
rs2 3 1 1k
rc1 4 8 10k
rc2 5 8 10k
q3 7 7 9 qnl
q4 6 7 9 qnr
rbias 7 8 20k

* Inputs/Supplies:
vcm 1 0 dc 0 sin(0 0.1 5meg) ac 1
vdm 1 11 dc 0 sin(0 0.1 5meg) ac 1
vcc 8 0 12
vee 9 0 -12

* Analysys:
.tf v(5) vcm
.tf v(5) vdm
.sens v(5,4)

.end
