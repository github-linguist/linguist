HV := new HistoryVariable
HV.var := 1
HV.var := 2
HV.var := "Latest value"
Msgbox % HV.var "`n" HV[2] "`n" HV[3]

class HistoryVariable {
	__Set(aName, aValue) {
		this.Insert(1,aValue)
		Return aValue
	}
	__Get(aName) {
		Return this[1]
	}
}
