Departments = D050, D101, D190, D202
StringSplit, Department_, Departments, `,, %A_Space%

; Employee Name, Employee ID, Salary, Department
Add_Employee("Tyler Bennett  ", "E10297", 32000, "D101")
Add_Employee("John Rappl     ", "E21437", 47000, "D050")
Add_Employee("George Woltman ", "E00127", 53500, "D101")
Add_Employee("Adam Smith     ", "E63535", 18000, "D202")
Add_Employee("Claire Buckman ", "E39876", 27800, "D202")
Add_Employee("David McClellan", "E04242", 41500, "D101")
Add_Employee("Rich Holcomb   ", "E01234", 49500, "D202")
Add_Employee("Nathan Adams   ", "E41298", 21900, "D050")
Add_Employee("Richard Potter ", "E43128", 15900, "D101")
Add_Employee("David Motsinger", "E27002", 19250, "D202")
Add_Employee("Tim Sampair    ", "E03033", 27000, "D101")
Add_Employee("Kim Arlich     ", "E10001", 57000, "D190")
Add_Employee("Timothy Grove  ", "E16398", 29900, "D190")

; display top 3 ranks for each department
Loop, %Department_0% ; all departments
    MsgBox,, % "Department:  " Department_%A_Index%
           , % TopRank(3, Department_%A_Index%)

;---------------------------------------------------------------------------
TopRank(N, Department) { ; find the top N salaries in each department
;---------------------------------------------------------------------------
    local Collection := Msg := ""
    Loop, %m% ; all employees
        If (Employee_%A_Index%_Dept = Department)
            ; collect all the salaries being paid in this department
            Collection .= (Collection ? "," : "") Employee_%A_Index%_Salary
    Sort, Collection, ND,R
    StringSplit, Collection, Collection, `,
    Loop, % (N < Collection0) ? N : Collection0 {
        Salary := Collection%A_Index%
        Loop, %m% ; find the respective employee
            If (Employee_%A_Index%_Salary = Salary)
                ; and put out his/her details
                Msg .= Employee_%A_Index%_Name "`t"
                    .  Employee_%A_Index%_ID "`t"
                    .  Employee_%A_Index%_Salary "`t"
                    .  Employee_%A_Index%_Dept "`t`n"
    }
    Return, Msg
}

;---------------------------------------------------------------------------
Add_Employee(Name, ID, Salary, Department) {
;---------------------------------------------------------------------------
    global
    m++
    Employee_%m%_Name   := Name
    Employee_%m%_ID     := ID
    Employee_%m%_Salary := Salary
    Employee_%m%_Dept   := Department
}
