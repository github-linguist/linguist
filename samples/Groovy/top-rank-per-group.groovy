def displayRank(employees, number) {
    employees.groupBy { it.Department }.sort().each { department, staff ->
        println "Department $department"
        println "    Name                ID      Salary"
        staff.sort { e1, e2 -> e2.Salary <=> e1.Salary }
        staff[0..<Math.min(number, staff.size())].each { e ->
           println "    ${e.Name.padRight(20)}${e.ID}${sprintf('%8d', e.Salary)}"
        }
        println()
    }
}

def employees = [
        [Name: 'Tyler Bennett', ID: 'E10297', Salary: 32000, Department: 'D101'],
        [Name: 'John Rappl', ID: 'E21437', Salary: 47000, Department: 'D050'],
        [Name: 'George Woltman', ID: 'E00127', Salary: 53500, Department: 'D101'],
        [Name: 'Adam Smith', ID: 'E63535', Salary: 18000, Department: 'D202'],
        [Name: 'Claire Buckman', ID: 'E39876', Salary: 27800, Department: 'D202'],
        [Name: 'David McClellan', ID: 'E04242', Salary: 41500, Department: 'D101'],
        [Name: 'Rich Holcomb', ID: 'E01234', Salary: 49500, Department: 'D202'],
        [Name: 'Nathan Adams', ID: 'E41298', Salary: 21900, Department: 'D050'],
        [Name: 'Richard Potter', ID: 'E43128', Salary: 15900, Department: 'D101'],
        [Name: 'David Motsinger', ID: 'E27002', Salary: 19250, Department: 'D202'],
        [Name: 'Tim Sampair', ID: 'E03033', Salary: 27000, Department: 'D101'],
        [Name: 'Kim Arlich', ID: 'E10001', Salary: 57000, Department: 'D190'],
        [Name: 'Timothy Grove', ID: 'E16398', Salary: 29900, Department: 'D190']
]
displayRank(employees, 3)
